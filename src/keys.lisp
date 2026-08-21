;;;; keys.lisp — SDL keycodes to X11 keysyms.
;;;;
;;;; RFB speaks X11 keysyms, and glass hands the server's keysym straight through
;;;; to the remote, so this is the only translation in the whole path.
;;;;
;;;; Printable ASCII needs none: SDL's keycode for a printable character IS its
;;;; ASCII code, and so is X11's keysym.  What follows is the rest — the keys with
;;;; no character, which SDL numbers from 1<<30 and X11 numbers from 0xFF00.

(in-package #:glass-sdl)

(defconstant +sdlk-scancode-mask+ (ash 1 30))

(defparameter *keysyms*
  ;; SDL keycode . X11 keysym
  `((8 . #xFF08)      ; backspace
    (9 . #xFF09)      ; tab
    (13 . #xFF0D)     ; return
    (27 . #xFF1B)     ; escape
    (127 . #xFFFF)    ; delete
    (,(logior +sdlk-scancode-mask+ 79) . #xFF53)   ; right
    (,(logior +sdlk-scancode-mask+ 80) . #xFF51)   ; left
    (,(logior +sdlk-scancode-mask+ 81) . #xFF54)   ; down
    (,(logior +sdlk-scancode-mask+ 82) . #xFF52)   ; up
    (,(logior +sdlk-scancode-mask+ 74) . #xFF50)   ; home
    (,(logior +sdlk-scancode-mask+ 77) . #xFF57)   ; end
    (,(logior +sdlk-scancode-mask+ 75) . #xFF55)   ; page up
    (,(logior +sdlk-scancode-mask+ 78) . #xFF56)   ; page down
    (,(logior +sdlk-scancode-mask+ 73) . #xFF63)   ; insert
    ;; modifiers — the remote needs these held, not just the character they shift
    (,(logior +sdlk-scancode-mask+ 225) . #xFFE1)  ; left shift
    (,(logior +sdlk-scancode-mask+ 229) . #xFFE2)  ; right shift
    (,(logior +sdlk-scancode-mask+ 224) . #xFFE3)  ; left control
    (,(logior +sdlk-scancode-mask+ 228) . #xFFE4)  ; right control
    (,(logior +sdlk-scancode-mask+ 226) . #xFFE9)  ; left alt
    (,(logior +sdlk-scancode-mask+ 230) . #xFFEA)  ; right alt
    (,(logior +sdlk-scancode-mask+ 227) . #xFFEB)  ; left gui / command
    (,(logior +sdlk-scancode-mask+ 231) . #xFFEC)  ; right gui
    (,(logior +sdlk-scancode-mask+ 57) . #xFFE5))  ; caps lock
  "Everything that is not simply its own character.")

(defun function-keysym (code)
  "F1..F12 are contiguous in both numbering schemes, so they are arithmetic."
  (let ((n (- code (logior +sdlk-scancode-mask+ 58))))
    (when (<= 0 n 11) (+ #xFFBE n))))

(defun sdl->keysym (code)
  "The X11 keysym for an SDL keycode, or NIL for keys we do not forward."
  (cond
    ((cdr (assoc code *keysyms*)))
    ((function-keysym code))
    ;; Printable ASCII is already the keysym.  Note this is the UNSHIFTED
    ;; character: SDL reports 'a' whether or not shift is down, and the remote
    ;; applies the modifier itself, which is what a real keyboard does too.
    ((<= 32 code 126) code)
    (t nil)))
