;;;; view.lisp — a glass desktop in a native window.
;;;;
;;;; The whole client is already written: glass/client speaks RFB, keeps a local
;;;; framebuffer of what the remote is showing, and takes keys and pointer events
;;;; back.  This adds the two things a hosted OS will not give you for free — a
;;;; window to put the pixels in, and a source of input events — and nothing else.

(in-package #:glass-sdl)

(defvar *probe-viewer* nil
  "The viewer most recently started, so a REPL (or a test) can reach the remote
   that is on screen without threading it through by hand.")

(defstruct (viewer (:conc-name v-))
  remote window renderer texture
  (width 0) (height 0)
  (buttons 0)                      ; RFB button mask, held across motion events
  (last-x 0) (last-y 0)            ; a wheel event carries no position
  (resized nil))

(defun make-texture (v)
  "A streaming texture the size of the remote's screen."
  (when (v-texture v) (sdl (%destroy-texture (v-texture v))))
  (setf (v-texture v)
        (sdl (%create-texture (v-renderer v) +format-rgb888+ +texture-streaming+
                              (v-width v) (v-height v))))
  (when (sb-alien:null-alien (v-texture v))
    (error "glass-sdl: could not create a ~dx~d texture: ~a"
           (v-width v) (v-height v) (sdl (%get-error)))))

(defun push-frame (v)
  "Upload the remote's framebuffer and present it.

   The whole screen, not the damaged part: glass/client reports dirtiness as a
   flag rather than a rectangle list, so there is nothing finer to act on, and a
   1280x800 upload is four megabytes of memcpy that a GPU eats without noticing.
   Idle costs nothing because we only get here when the flag was set."
  (let* ((fb (glass-client:remote-fb (v-remote v)))
         (px (glass:fb-pixels fb)))
    (sb-sys:with-pinned-objects (px)
      (sdl (%update-texture (v-texture v) (null-ptr)
                            (sb-alien:sap-alien (sb-sys:vector-sap px) (* t))
                            (* 4 (v-width v)))))
    (sdl (%render-clear (v-renderer v)))
    (sdl (%render-copy (v-renderer v) (v-texture v) (null-ptr) (null-ptr)))
    (sdl (%render-present (v-renderer v)))))

;;; ---- input -------------------------------------------------------------------

(defun button-bit (sdl-button)
  "SDL numbers buttons 1/2/3; RFB carries them as a bitmask."
  (case sdl-button (1 1) (2 2) (3 4) (t 0)))

(defun handle-event (v sap)
  "Translate one SDL event and forward it.  Returns NIL to stop the viewer."
  (let ((type (ev-u32 sap 0))
        (r (v-remote v)))
    (cond
      ((= type +quit+) nil)

      ((= type +window-event+)
       (let ((what (ev-u8 sap 12)))
         (not (= what +windowevent-close+))))

      ((or (= type +key-down+) (= type +key-up+))
       ;; keysym.sym is an Sint32 at offset 20 of SDL_KeyboardEvent.
       (let ((keysym (sdl->keysym (ev-i32 sap 20))))
         (when keysym
           (glass-client:remote-key r (= type +key-down+) keysym)))
       t)

      ((= type +mouse-motion+)
       (setf (v-last-x v) (ev-i32 sap 20) (v-last-y v) (ev-i32 sap 24))
       (glass-client:remote-pointer r (v-buttons v) (v-last-x v) (v-last-y v))
       t)

      ((or (= type +mouse-button-down+) (= type +mouse-button-up+))
       (let ((bit (button-bit (ev-u8 sap 16))))
         (setf (v-last-x v) (ev-i32 sap 20) (v-last-y v) (ev-i32 sap 24))
         (setf (v-buttons v) (if (= type +mouse-button-down+)
                                 (logior (v-buttons v) bit)
                                 (logandc2 (v-buttons v) bit)))
         (glass-client:remote-pointer r (v-buttons v) (ev-i32 sap 20) (ev-i32 sap 24)))
       t)

      ((= type +mouse-wheel+)
       ;; RFB has no wheel: it is buttons 4 and 5, pressed and released. The
       ;; position is not carried in a wheel event, so the last one stands.
       (let* ((dy (ev-i32 sap 20))
              (bit (cond ((plusp dy) 8) ((minusp dy) 16) (t 0))))
         (unless (zerop bit)
           (let ((x (v-last-x v)) (y (v-last-y v)))
             (glass-client:remote-pointer r (logior (v-buttons v) bit) x y)
             (glass-client:remote-pointer r (v-buttons v) x y))))
       t)

      (t t))))

;;; ---- the loop ----------------------------------------------------------------

(defun view (&key (host "127.0.0.1") (port 5901) (title nil) (fps 60))
  "Open a window onto the glass desktop at HOST:PORT and pump it until closed.

   Runs on the calling thread and does not return until the window closes, which
   on macOS is not a preference: Cocoa insists the event loop is the main thread,
   so this is a function you CALL from your main thread rather than a server you
   start."
  (load-sdl)
  (unless (zerop (sdl (%init +init-video+)))
    (error "glass-sdl: SDL_Init failed: ~a" (sdl (%get-error))))
  (let ((v (setf *probe-viewer* (make-viewer :remote (glass-client:connect-remote host port))))
        (ev (sb-alien:make-alien (sb-alien:unsigned 8) 64)))
    (unwind-protect
         (let ((r (v-remote v)))
           ;; Wait for the handshake so the window opens at the remote's size
           ;; rather than opening small and jumping.
           (loop repeat 200 until (glass-client:remote-connected-p r) do (sleep 0.05))
           (unless (glass-client:remote-connected-p r)
             (error "glass-sdl: no answer from ~a:~d" host port))
           (setf (v-width v) (glass-client:remote-width r)
                 (v-height v) (glass-client:remote-height r))
           (setf (glass-client:remote-on-resize r)
                 (lambda (w h) (declare (ignore w h)) (setf (v-resized v) t)))
           (setf (v-window v)
                 (sdl (%create-window (or title (format nil "glass — ~a:~d" host port))
                                      #x1FFF0000 #x1FFF0000
                                      (v-width v) (v-height v)
                                      (logior +window-shown+ +window-resizable+))))
           (when (sb-alien:null-alien (v-window v))
             (error "glass-sdl: could not open a window: ~a" (sdl (%get-error))))
           (setf (v-renderer v)
                 (sdl (%create-renderer (v-window v) -1
                                        (logior +renderer-accelerated+ +renderer-presentvsync+))))
           (when (sb-alien:null-alien (v-renderer v))
             ;; No GPU path (a bare VM, a stubborn driver): software still draws.
             (setf (v-renderer v) (sdl (%create-renderer (v-window v) -1 0))))
           (make-texture v)
           (sdl (%start-text-input))
           (let ((sap (sb-alien:alien-sap ev))
                 (frame-ms (max 1 (floor 1000 fps))))
             (loop
               (loop while (plusp (sdl (%poll-event (sb-alien:sap-alien sap (* t)))))
                     do (unless (handle-event v sap) (return-from view t)))
               (when (v-resized v)
                 (setf (v-resized v) nil
                       (v-width v) (glass-client:remote-width r)
                       (v-height v) (glass-client:remote-height r))
                 (sdl (%set-window-size (v-window v) (v-width v) (v-height v)))
                 (make-texture v)
                 (push-frame v))
               (if (glass-client:remote-take-dirty r)
                   (push-frame v)
                   (sdl (%delay frame-ms)))
               (unless (glass-client:remote-connected-p r)
                 ;; The client reconnects on its own; say so rather than dying.
                 (sdl (%set-window-title (v-window v) "glass — reconnecting…"))
                 (sdl (%delay 200))))))
      (ignore-errors (glass-client:remote-stop (v-remote v)))
      (when (v-texture v) (ignore-errors (sdl (%destroy-texture (v-texture v)))))
      (when (v-renderer v) (ignore-errors (sdl (%destroy-renderer (v-renderer v)))))
      (when (v-window v) (ignore-errors (sdl (%destroy-window (v-window v)))))
      (ignore-errors (sb-alien:free-alien ev))
      (ignore-errors (sdl (%quit))))))
