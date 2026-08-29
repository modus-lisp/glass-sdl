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

;;; ---- where the pixels come from ----------------------------------------------
;;;
;;; The viewer needs eight things from a desktop: its framebuffer, its size, whether it
;;; is still there, whether it has changed, and somewhere to put keys and pointer events.
;;; An RFB connection is ONE way to have those.  It is not the only one, and in the case
;;; that matters most it is the silly one: a desktop in this same image already holds the
;;; pixels as an object, and going through a socket to get them means encoding a screen we
;;; are holding, writing it to the kernel, reading it back and decoding it.
;;;
;;; So the eight are a struct of closures, and the transport is a detail of which
;;; constructor was called.  MAKE-REMOTE-SOURCE is glass/client, as before.
;;; MAKE-SEAT-SOURCE is a seat in this process, with nothing between.
(defstruct (source (:conc-name src-))
  fb          ; () -> framebuffer
  width       ; () -> px
  height      ; () -> px
  live-p      ; () -> generalized boolean
  dirty-p     ; () -> T if the screen changed since the last call
  key         ; (down-p keysym) -> ignored
  pointer     ; (button-mask x y) -> ignored
  stop        ; () -> ignored
  on-resize   ; (function-of-w-and-h) -> ignored; installs a resize callback
  want-size)  ; (w h) -> ignored; ASK the desktop to become this size

(defun make-remote-source (host port)
  "A desktop reached over RFB — another machine, or a container, or this one."
  (let ((r (glass-client:connect-remote host port)))
    (make-source :fb        (lambda () (glass-client:remote-fb r))
                 :width     (lambda () (glass-client:remote-width r))
                 :height    (lambda () (glass-client:remote-height r))
                 :live-p    (lambda () (glass-client:remote-connected-p r))
                 :dirty-p   (lambda () (glass-client:remote-take-dirty r))
                 :key       (lambda (down k) (glass-client:remote-key r down k))
                 :pointer   (lambda (b x y) (glass-client:remote-pointer r b x y))
                 :stop      (lambda () (glass-client:remote-stop r))
                 :on-resize (lambda (fn) (setf (glass-client:remote-on-resize r) fn))
                 ;; A viewer does not get to resize somebody else's desktop over RFB --
                 ;; that needs the extended-desktop-size pseudo-encoding, which glass's
                 ;; client does not speak.  Saying so as a no-op beats pretending.
                 :want-size (lambda (w h) (declare (ignore w h)) nil))))

(defun make-seat-source (seat)
  "A desktop in THIS image: no socket, no handshake, no encode/decode round trip.

   Dirtiness is GLASS:FB-FRAMENO, which the compositor advances on every change.
   Comparing it is exact where polling the seat's wake condition would race -- a
   broadcast that lands between two polls is simply lost, and the screen would then sit
   stale until something else happened to change."
  ;; Looked up by NAME, not depended on.  ATTACH-SEAT-LOCAL lives in the window-manager
  ;; layer, and this system must not pull that in to open a window: glass-sdl is a viewer,
  ;; and what it views is somebody else's business.  A caller holding a seat has already
  ;; loaded whatever made it.
  (let ((attach (find-symbol "ATTACH-SEAT-LOCAL" "CLIM-GLASS")))
    (unless (and attach (fboundp attach))
      (error "glass-sdl: :SEAT needs CLIM-GLASS:ATTACH-SEAT-LOCAL, which is not in this ~
              image -- load the window-manager layer, or pass :HOST/:PORT instead."))
  (multiple-value-bind (fb on-key on-pointer on-resize wake)
      (funcall attach seat)
    ;; WAKE is the compositor's condition variable; FB-FRAMENO says the same thing
    ;; without the race, so it is not used here.
    (declare (ignore wake))
    (let ((seen -1) (resize-cb nil) (last-w (glass:fb-width fb)) (last-h (glass:fb-height fb)))
      (make-source
       :fb        (lambda () fb)
       :width     (lambda () (glass:fb-width fb))
       :height    (lambda () (glass:fb-height fb))
       :live-p    (lambda () t)             ; it is us; it cannot go away without us
       :dirty-p   (lambda ()
                    ;; ...and notice a resize on the way past, since there is no
                    ;; server here to announce one.
                    (let ((w (glass:fb-width fb)) (h (glass:fb-height fb)))
                      (unless (and (eql w last-w) (eql h last-h))
                        (setf last-w w last-h h)
                        (when resize-cb (funcall resize-cb w h))))
                    (let ((n (glass:fb-frameno fb)))
                      (and (/= n seen) (setf seen n) t)))
       :key       (lambda (down k) (funcall on-key down k))
       :pointer   (lambda (b x y) (funcall on-pointer b x y))
       :stop      (lambda () nil)           ; nothing was opened, so nothing is closed
       :on-resize (lambda (fn) (setf resize-cb fn) (values))
       ;; The window IS the desktop here, so dragging its edge resizes the DESKTOP rather
       ;; than stretching a picture of one.  ATTACH-SEAT-LOCAL hands us the seat's own
       ;; resize -- not glass's SetDesktopSize callback, which resizes an application's
       ;; window and is the wrong end of the problem for a viewer holding a whole screen.
       :want-size (lambda (w h) (funcall on-resize w h)))))))

(defstruct (viewer (:conc-name v-))
  source window renderer texture
  (width 0) (height 0)
  (buttons 0)                      ; RFB button mask, held across motion events
  (last-x 0) (last-y 0)            ; a wheel event carries no position
  (resized nil))

(defun usable-size (want-w want-h)
  "WANT-W x WANT-H clamped to the part of the display a window can actually occupy.

   macOS does not refuse an oversized window, it SILENTLY CAPS it between the menu bar and
   the Dock — so asking for 1600x1000 on a screen with 1600x1000 of glass gets a window
   some tens of pixels shorter, and every symptom after that is a consequence of the
   framebuffer and the window no longer being the same size: SDL stretches the texture, and
   the pointer arithmetic below has a scale to undo.  Asking for a size that fits means the
   common case is 1:1 and no scaling happens at all.

   A margin comes off as well.  Usable bounds are exact, and a window at exactly that size
   has its title bar flush against the menu bar with nowhere to grab it."
  (let ((rect (sb-alien:make-alien sb-alien:int 4)))
    (unwind-protect
         (if (zerop (%get-display-usable-bounds 0 (sb-alien:cast rect (* t))))
             (let ((max-w (- (sb-alien:deref rect 2) 40))
                   (max-h (- (sb-alien:deref rect 3) 60)))
               (values (max 320 (min want-w max-w))
                       (max 240 (min want-h max-h))))
             ;; No answer (a headless display index, an SDL that does not implement it):
             ;; ask for what was wanted.  Being capped is the thing this avoids, not a
             ;; thing it must prevent.
             (values want-w want-h))
      (sb-alien:free-alien rect))))

(defun window-points (v)
  "The window's size in POINTS — the space mouse events arrive in."
  (let ((w (sb-alien:make-alien sb-alien:int))
        (h (sb-alien:make-alien sb-alien:int)))
    (unwind-protect
         (progn (%get-window-size (v-window v) w h)
                (values (sb-alien:deref w) (sb-alien:deref h)))
      (sb-alien:free-alien w) (sb-alien:free-alien h))))

(defun to-fb (v x y)
  "An SDL pointer position mapped into FRAMEBUFFER coordinates.

   RENDER-COPY stretches the whole texture over the whole window, so when the two are not
   the same size a click lands somewhere the pointer is not — increasingly wrong toward the
   bottom right, which is what an offset cursor over a scaled desktop is.  The same ratio
   that scales the pixels has to scale the input back.

   Identity in the normal case, and cheap enough not to be worth avoiding when it is not."
  (multiple-value-bind (ww wh) (window-points v)
    (if (or (zerop ww) (zerop wh)
            (and (eql ww (v-width v)) (eql wh (v-height v))))
        (values x y)
        (values (min (1- (v-width v))  (max 0 (floor (* x (v-width v))  ww)))
                (min (1- (v-height v)) (max 0 (floor (* y (v-height v)) wh)))))))

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
  (let* ((fb (funcall (src-fb (v-source v))))
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
        (r (v-source v)))
    (cond
      ((= type +quit+) nil)

      ((= type +window-event+)
       (let ((what (ev-u8 sap 12)))
         (when (= what +windowevent-size-changed+)
           ;; SDL_WindowEvent carries the new size in data1/data2, at 16 and 20.
           ;;
           ;; ONLY WHEN IT DIFFERS, and that guard is the whole of what keeps this from
           ;; oscillating: the desktop resizing makes the loop below call
           ;; %SET-WINDOW-SIZE, which SDL answers with another SIZE-CHANGED, which would
           ;; ask for the size we are already at, forever.
           (let ((w (ev-i32 sap 16)) (h (ev-i32 sap 20)))
             (when (and (plusp w) (plusp h)
                        (not (and (eql w (funcall (src-width r)))
                                  (eql h (funcall (src-height r))))))
               (funcall (src-want-size r) w h))))
         (not (= what +windowevent-close+))))

      ((or (= type +key-down+) (= type +key-up+))
       ;; keysym.sym is an Sint32 at offset 20 of SDL_KeyboardEvent.
       (let ((keysym (sdl->keysym (ev-i32 sap 20))))
         (when keysym
           (funcall (src-key r) (= type +key-down+) keysym)))
       t)

      ((= type +mouse-motion+)
       (multiple-value-bind (fx fy) (to-fb v (ev-i32 sap 20) (ev-i32 sap 24))
         (setf (v-last-x v) fx (v-last-y v) fy))
       (funcall (src-pointer r) (v-buttons v) (v-last-x v) (v-last-y v))
       t)

      ((or (= type +mouse-button-down+) (= type +mouse-button-up+))
       (let ((bit (button-bit (ev-u8 sap 16))))
         (multiple-value-bind (fx fy) (to-fb v (ev-i32 sap 20) (ev-i32 sap 24))
           (setf (v-last-x v) fx (v-last-y v) fy))
         (setf (v-buttons v) (if (= type +mouse-button-down+)
                                 (logior (v-buttons v) bit)
                                 (logandc2 (v-buttons v) bit)))
           ;; The MAPPED position, not the raw one: storing the mapping and then sending
           ;; the event's own coordinates is how a click misses what the cursor is over.
           (funcall (src-pointer r) (v-buttons v) (v-last-x v) (v-last-y v)))
       t)

      ((= type +mouse-wheel+)
       ;; RFB has no wheel: it is buttons 4 and 5, pressed and released. The
       ;; position is not carried in a wheel event, so the last one stands.
       (let* ((dy (ev-i32 sap 20))
              (bit (cond ((plusp dy) 8) ((minusp dy) 16) (t 0))))
         (unless (zerop bit)
           (let ((x (v-last-x v)) (y (v-last-y v)))
             (funcall (src-pointer r) (logior (v-buttons v) bit) x y)
             (funcall (src-pointer r) (v-buttons v) x y))))
       t)

      (t t))))

;;; ---- the loop ----------------------------------------------------------------

(defun view (&key (host "127.0.0.1") (port 5901) seat (title nil) (fps 60))
  "Open a window onto a glass desktop and pump it until closed.

   Runs on the calling thread and does not return until the window closes, which
   on macOS is not a preference: Cocoa insists the event loop is the main thread,
   so this is a function you CALL from your main thread rather than a server you
   start.

   SEAT is a desktop in THIS image (CLIM-GLASS:ADD-WM-SEAT, typically made with
   :SERVE NIL) and there is no transport at all: the framebuffer is read where it
   already is and keys go straight to the seat's injector.  Otherwise HOST:PORT is
   an RFB desktop somewhere -- a hostname beside a port, or `unix:/path/seat-1.rfb'
   for a socket file, which is the same thing with the kernel checking who may
   connect.

   The one-image case is the point rather than an optimisation.  On the hardware
   this is aimed at there is no socket to have and no second process to be: the
   desktop composites into a framebuffer and something puts that on a screen.  Here
   that something is SDL; there it is glass/fb itself."
  (load-sdl)
  (unless (zerop (sdl (%init +init-video+)))
    (error "glass-sdl: SDL_Init failed: ~a" (sdl (%get-error))))
  (let ((v (setf *probe-viewer*
                 (make-viewer :source (if seat
                                          (make-seat-source seat)
                                          (make-remote-source host port)))))
        (ev (sb-alien:make-alien (sb-alien:unsigned 8) 64)))
    (unwind-protect
         (let ((r (v-source v)))
           ;; Wait for the handshake so the window opens at the desktop's size
           ;; rather than opening small and jumping.  A local seat is up already.
           (loop repeat 200 until (funcall (src-live-p r)) do (sleep 0.05))
           (unless (funcall (src-live-p r))
             (error "glass-sdl: no answer from ~a:~d" host port))
           (setf (v-width v) (funcall (src-width r))
                 (v-height v) (funcall (src-height r)))
           ;; SMOOTH RATHER THAN BLOCKY when the window and the framebuffer are not the same
           ;; size — mid-drag, or a desktop that is a fixed size on purpose.  SDL's default is
           ;; "0", nearest neighbour, which is the hard staircase on every glyph of a scaled
           ;; desktop.  Set BEFORE the texture exists: SDL reads this when a texture is made,
           ;; not when one is drawn, so setting it afterwards is a setting that does nothing.
           (%set-hint "SDL_RENDER_SCALE_QUALITY" "linear")
           ;; ...AND MAKE 1:1 THE NORMAL CASE, which beats scaling well.  macOS does not refuse
           ;; an oversized window, it silently caps it between the menu bar and the Dock — so a
           ;; desktop asked to be taller than the screen becomes a permanently scaled one, with
           ;; no event to say so.  Ask the DESKTOP for a size that fits before opening a window
           ;; onto it.  For a local seat WANT-SIZE is the seat's own resize and has already
           ;; happened by the time it returns, so the window is created at the settled size; for
           ;; a remote one the request is in flight and the existing SIZE-CHANGED path converges,
           ;; exactly as it does for a drag.
           (multiple-value-bind (uw uh) (usable-size (v-width v) (v-height v))
             (unless (and (eql uw (v-width v)) (eql uh (v-height v)))
               (funcall (src-want-size r) uw uh)
               (setf (v-width v) (funcall (src-width r))
                     (v-height v) (funcall (src-height r)))))
           (funcall (src-on-resize r)
                    (lambda (w h) (declare (ignore w h)) (setf (v-resized v) t)))
           (setf (v-window v)
                 (sdl (%create-window (or title
                                          ;; THE SESSION's name, which is what an RFB
                                          ;; client puts in its title bar and what the
                                          ;; desktop writes in its own corner — the same
                                          ;; name in all three places.  The seat is a
                                          ;; place at the session, not the thing being
                                          ;; looked at, so it is the fallback and not the
                                          ;; answer.
                                          (if seat
                                              (let ((n (find-symbol "SEAT-NAME" "CLIM-GLASS")))
                                                (format nil "glass — ~a"
                                                        (if (and (stringp glass:*desktop-name*)
                                                                 (plusp (length glass:*desktop-name*))
                                                                 (not (string= glass:*desktop-name* "glass")))
                                                            glass:*desktop-name*
                                                            (or (and n (fboundp n)
                                                                     (ignore-errors (funcall n seat)))
                                                                "desktop"))))
                                              (format nil "glass — ~a:~d" host port)))
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
                       (v-width v) (funcall (src-width r))
                       (v-height v) (funcall (src-height r)))
                 (sdl (%set-window-size (v-window v) (v-width v) (v-height v)))
                 (make-texture v)
                 (push-frame v))
               (if (funcall (src-dirty-p r))
                   (push-frame v)
                   (sdl (%delay frame-ms)))
               (unless (funcall (src-live-p r))
                 ;; The client reconnects on its own; say so rather than dying.
                 (sdl (%set-window-title (v-window v) "glass — reconnecting…"))
                 (sdl (%delay 200))))))
      (ignore-errors (funcall (src-stop (v-source v))))
      (when (v-texture v) (ignore-errors (sdl (%destroy-texture (v-texture v)))))
      (when (v-renderer v) (ignore-errors (sdl (%destroy-renderer (v-renderer v)))))
      (when (v-window v) (ignore-errors (sdl (%destroy-window (v-window v)))))
      (ignore-errors (sb-alien:free-alien ev))
      (ignore-errors (sdl (%quit))))))
