;;;; sdl.lisp — the SDL2 we need, bound with SBCL's own FFI.
;;;;
;;;; Not cl-sdl2, which is a fine library and wants cl-autowrap, which wants
;;;; `c2ffi' — a clang-based header parser you have to build from a tap.  That is
;;;; a lot of machinery to place a rectangle of pixels on a screen.  A framebuffer
;;;; viewer needs about fifteen entry points, so they are declared here against
;;;; sb-alien: nothing to install but the dylib itself, and nothing parsed at
;;;; build time.
;;;;
;;;; This is the one FFI in the workspace and it is deliberate.  glass is a
;;;; framebuffer and an RFB server in Common Lisp; putting its pixels in a window
;;;; means asking somebody else's window server, and on a hosted OS that is
;;;; always going to be C.  The boundary is here, in this file, and nothing
;;;; behind it changes.

(in-package #:glass-sdl)

(defparameter *sdl-candidates*
  #+darwin '("libSDL2-2.0.0.dylib" "libSDL2.dylib"
             "/opt/homebrew/lib/libSDL2-2.0.0.dylib"   ; Homebrew, Apple silicon
             "/usr/local/lib/libSDL2-2.0.0.dylib")     ; Homebrew, Intel
  #+win32 '("SDL2.dll")
  #-(or darwin win32) '("libSDL2-2.0.so.0" "libSDL2.so"
                        "/usr/lib/x86_64-linux-gnu/libSDL2-2.0.so.0"
                        "/usr/lib/aarch64-linux-gnu/libSDL2-2.0.so.0")
  "Tried in order.  Bare sonames first, so the system loader gets its say and a
   package-managed copy wins over a guessed path.")

(defvar *sdl-loaded* nil)

(defun load-sdl ()
  "Find libSDL2 once.  Signals with something actionable rather than a bare
   'shared object not found', because the fix is a package install."
  (unless *sdl-loaded*
    (let ((tried '()))
      (dolist (name *sdl-candidates*)
        (push name tried)
        (when (ignore-errors (sb-alien:load-shared-object name) t)
          (setf *sdl-loaded* name)
          (return)))
      (unless *sdl-loaded*
        (error "glass-sdl: no libSDL2 found (tried ~{~a~^, ~}).~%~
                Install it:  brew install sdl2   |   apt install libsdl2-2.0-0"
               (reverse tried)))))
  *sdl-loaded*)

;;; SDL, and anything under it that touches a GPU, trips the floating-point traps
;;; SBCL enables by default.  C does not expect them; every call goes through here.
(defmacro sdl (&body body)
  `(sb-int:with-float-traps-masked (:invalid :inexact :overflow :underflow :divide-by-zero)
     ,@body))

(sb-alien:define-alien-routine ("SDL_Init" %init) sb-alien:int (flags sb-alien:unsigned-int))
(sb-alien:define-alien-routine ("SDL_Quit" %quit) sb-alien:void)
(sb-alien:define-alien-routine ("SDL_GetError" %get-error) sb-alien:c-string)
;; Hints must be set BEFORE the object they affect is created — SDL reads
;; RENDER_SCALE_QUALITY when the texture is made, not when it is drawn.
(sb-alien:define-alien-routine ("SDL_SetHint" %set-hint) sb-alien:int
  (name sb-alien:c-string) (value sb-alien:c-string))
;; In POINTS, which is the same space mouse events arrive in — deliberately not the
;; drawable size, which on a Retina panel is twice this.
(sb-alien:define-alien-routine ("SDL_GetWindowSize" %get-window-size) sb-alien:void
  (window (* t)) (w (* sb-alien:int)) (h (* sb-alien:int)))
(sb-alien:define-alien-routine ("SDL_CreateWindow" %create-window) (* t)
  (title sb-alien:c-string) (x sb-alien:int) (y sb-alien:int)
  (w sb-alien:int) (h sb-alien:int) (flags sb-alien:unsigned-int))
(sb-alien:define-alien-routine ("SDL_DestroyWindow" %destroy-window) sb-alien:void (w (* t)))
(sb-alien:define-alien-routine ("SDL_SetWindowTitle" %set-window-title) sb-alien:void
  (w (* t)) (title sb-alien:c-string))
(sb-alien:define-alien-routine ("SDL_SetWindowSize" %set-window-size) sb-alien:void
  (w (* t)) (width sb-alien:int) (height sb-alien:int))
(sb-alien:define-alien-routine ("SDL_CreateRenderer" %create-renderer) (* t)
  (window (* t)) (index sb-alien:int) (flags sb-alien:unsigned-int))
(sb-alien:define-alien-routine ("SDL_DestroyRenderer" %destroy-renderer) sb-alien:void (r (* t)))
(sb-alien:define-alien-routine ("SDL_CreateTexture" %create-texture) (* t)
  (renderer (* t)) (format sb-alien:unsigned-int) (access sb-alien:int)
  (w sb-alien:int) (h sb-alien:int))
(sb-alien:define-alien-routine ("SDL_DestroyTexture" %destroy-texture) sb-alien:void (tex (* t)))
(sb-alien:define-alien-routine ("SDL_UpdateTexture" %update-texture) sb-alien:int
  (texture (* t)) (rect (* t)) (pixels (* t)) (pitch sb-alien:int))
(sb-alien:define-alien-routine ("SDL_RenderClear" %render-clear) sb-alien:int (r (* t)))
(sb-alien:define-alien-routine ("SDL_RenderCopy" %render-copy) sb-alien:int
  (renderer (* t)) (texture (* t)) (src (* t)) (dst (* t)))
(sb-alien:define-alien-routine ("SDL_RenderPresent" %render-present) sb-alien:void (r (* t)))
(sb-alien:define-alien-routine ("SDL_PumpEvents" %pump-events) sb-alien:void)
(sb-alien:define-alien-routine ("SDL_PollEvent" %poll-event) sb-alien:int (event (* t)))
;; An event WATCH, which is not the same thing as the queue.  A watch is called wherever the
;; event is generated — including from inside the modal run loop AppKit enters while a window
;; edge is being dragged, which is exactly when SDL_PollEvent is not being reached.
(sb-alien:define-alien-routine ("SDL_AddEventWatch" %add-event-watch) sb-alien:void
  (filter (* t)) (userdata (* t)))
(sb-alien:define-alien-routine ("SDL_DelEventWatch" %del-event-watch) sb-alien:void
  (filter (* t)) (userdata (* t)))
(sb-alien:define-alien-routine ("SDL_StartTextInput" %start-text-input) sb-alien:void)
(sb-alien:define-alien-routine ("SDL_Delay" %delay) sb-alien:void (ms sb-alien:unsigned-int))

(defconstant +init-video+ #x20)
(defconstant +window-shown+ 4)
(defconstant +window-resizable+ 32)
;; SDL_PIXELFORMAT_RGB888 is X8R8G8B8 — the alpha byte is ignored, which is
;; exactly right for glass's 0x00RRGGBB pixels.
(defconstant +format-rgb888+ #x16161804)
(defconstant +texture-streaming+ 1)
(defconstant +renderer-accelerated+ 2)
(defconstant +renderer-presentvsync+ 4)

(defun null-ptr () (sb-alien:sap-alien (sb-sys:int-sap 0) (* t)))

;;; ---- events -----------------------------------------------------------------
;;; SDL_Event is a 56-byte union with a stable ABI, so the fields are read by
;;; offset rather than by generating a struct definition.

(defconstant +quit+ #x100)
(defconstant +window-event+ #x200)
(defconstant +key-down+ #x300)
(defconstant +key-up+ #x301)
(defconstant +text-input+ #x303)
(defconstant +mouse-motion+ #x400)
(defconstant +mouse-button-down+ #x401)
(defconstant +mouse-button-up+ #x402)
(defconstant +mouse-wheel+ #x403)
(defconstant +windowevent-close+ 14)
(defconstant +windowevent-resized+ 5)
(defconstant +windowevent-size-changed+ 6)

(defun ev-u32 (sap off) (sb-sys:sap-ref-32 sap off))
(defun ev-i32 (sap off) (sb-sys:signed-sap-ref-32 sap off))
(defun ev-u8  (sap off) (sb-sys:sap-ref-8 sap off))
