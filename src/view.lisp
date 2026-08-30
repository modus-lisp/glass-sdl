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
  (resized nil)
  ;; A size we ASKED the window for and have not been given yet.  %SET-WINDOW-SIZE does not
  ;; take effect before it returns — the window server applies it on its own schedule — so
  ;; without this, SETTLE-SIZE reads the stale size a moment later, concludes the desktop
  ;; has drifted, and resizes the desktop back to it.  That is not hypothetical: it ratchets,
  ;; and it shrank a 1456-wide window to 1259 over a few rounds before this existed.
  (pending-w nil) (pending-h nil) (pending-frames 0)
  ;; The window-size probe and the thunk that frees its cells — see MAKE-SIZE-PROBE.
  (title "glass")                  ; the window's name WITHOUT the device indicators
  (devices-shown :none)            ; (MIC-LIVE SPEAKERS-LIVE) as last put in the title
  ;; WHO ASKED.  A seat resize started by SETTLE-SIZE is the window telling the desktop what
  ;; size it is; a seat resize started by anything else is the desktop asking the window to
  ;; become a size.  They must not be answered the same way — see the loop.
  (resize-from-window nil)
  ;; THE DISPLAY'S pixels-per-point, measured once.  Not WINDOW-SCALE: that is computed live
  ;; from the window's own points and pixels, and during a resize those two are updated at
  ;; different moments — so the ratio between them is briefly nonsense, and a conversion that
  ;; divides by it asks for a window of a nonsense size.  Asked for 1800 pixels wide, it
  ;; produced a request small enough to hit the floor.  A display's scale does not change
  ;; while a window is being dragged, so measuring it once is not a cache, it is the right
  ;; source.
  (display-scale 1)
  (audio nil)                      ; the AUDIO-OUT playing this seat, or NIL
  (mic nil)                        ; the AUDIO-IN feeding the session, or NIL
  (size-probe nil) (free-size-probe nil)
  ;; ...and the same in pixels.  Under ALLOW_HIGHDPI these differ by the display's scale.
  (pixel-probe nil) (free-pixel-probe nil)
  ;; What the last live-resize draw cost, and when it finished — the two numbers the watch
  ;; needs to decide whether it can afford another.  See the watch for the policy.
  (last-draw-ms 0.0) (last-draw-at 0))

(defun display-scale ()
  "This display's pixels per point, measured — 2 on a Retina panel, 1 otherwise.

   ASKED BEFORE A SESSION EXISTS, which is the whole reason it is here.  A viewer learns
   the density when it opens its window, and by then the desktop has already been built and
   its apps already spawned at whatever ppem they were given — so a terminal started at
   session boot came out at 1x and stayed there, sharp chrome around small text.  Answering
   the question early lets the session be MADE at the right density instead of corrected
   afterwards, which would mean re-rendering every window that already exists.

   Measured with a real window rather than SDL_GetDisplayDPI, which on macOS reports the
   panel's physical DPI and not the backing-store ratio the window server actually applies —
   a different number, and the wrong one.  The window is 1x1, never shown, and destroyed
   before returning.  Falls back to 1 if anything about that fails: a viewer that cannot
   measure should render exactly as it always did."
  (handler-case
      (progn
        (load-sdl)
        (sdl (%init +init-video+))
        (let ((w (sdl (%create-window "probe" 0 0 1 1
                                      (logior +window-hidden+ +window-allow-highdpi+)))))
          (if (sb-alien:null-alien w)
              1
              (unwind-protect
                   (sb-alien:with-alien ((pw sb-alien:int) (ph sb-alien:int)
                                         (lw sb-alien:int) (lh sb-alien:int))
                     (%get-window-pixels w (sb-alien:addr pw) (sb-alien:addr ph))
                     (%get-window-size   w (sb-alien:addr lw) (sb-alien:addr lh))
                     (if (and (plusp lw) (plusp pw)) (/ pw lw) 1))
                (sdl (%destroy-window w))))))
    (error () 1)))

(defparameter *min-points* 320
  "The smallest the window may become, in points, in either direction.

   THE DESKTOP FOLLOWS THE WINDOW, and nothing said where to stop.  Dragged down to about a
   hundred points it did not settle there — it collapsed, ending at a one-pixel desktop inside
   a two-pixel window, every number consistent and the whole thing unusable and unrecoverable,
   because a window two points wide has no edge left to grab.

   The collapse is a ratchet rather than one bad number: the window is asked for PIXELS/SCALE
   points, the server grants a shade less near its own limits, the next frame measures that and
   asks for less again.  Every step is correct.  A floor stops it because the server refuses to
   go under one, so the measurement feeding the next step cannot keep shrinking.

   Enforced by the WINDOW SERVER and not by us, which is what makes it hold during a drag:
   SDL_SetWindowMinimumSize means the edge stops moving, rather than us fighting the pointer a
   frame at a time and losing.")


(defun make-pixel-probe (window)
  "The window's size in PIXELS, as a closure over its own out-cells — the same shape and
   the same reason as MAKE-SIZE-PROBE, which answers in points.

   THE TWO ARE NOT THE SAME NUMBER once ALLOW_HIGHDPI is set, and everything downstream
   turns on which one it wants.  The framebuffer is pixels: making it the drawable size is
   what renders the desktop at the panel's real resolution instead of having the window
   server upscale a half-resolution picture.  Mouse events are points, so the pointer keeps
   asking the other probe.  Their ratio is the seat's density."
  (let ((w (sb-alien:make-alien sb-alien:int))
        (h (sb-alien:make-alien sb-alien:int)))
    (values (lambda ()
              (%get-window-pixels window w h)
              (values (sb-alien:deref w) (sb-alien:deref h)))
            (lambda () (sb-alien:free-alien w) (sb-alien:free-alien h)))))

(defun make-size-probe (window)
  "Two out-cells for SDL_GetWindowSize and a closure that reads them, plus the thunk that
   frees them.  Made once per window because this runs on the two hottest paths there are:
   every pointer event and every frame.

   CLOSED OVER, which is the whole point and is not interchangeable with the obvious
   alternatives.  Measured, per call:

     with-alien locals, ADDR each time     96.0 bytes   0.034 us
     cells in a special variable         4057.3 bytes   1.485 us
     a pinned (signed-byte 32) vector      32.1 bytes   0.027 us
     closed-over cells (this)               0.0 bytes   0.025 us

   The special is not a typo — caching the cells that way is forty times WORSE than not
   caching them, because the compiler cannot see an alien type through a special binding
   and falls back to generic coercion on every call.  A lexical binding it can see, so the
   call compiles to a direct store into two known addresses and conses nothing at all.

   The cells outlive the call and must be freed; VIEW does that in the same UNWIND-PROTECT
   that destroys the window."
  (let ((w (sb-alien:make-alien sb-alien:int))
        (h (sb-alien:make-alien sb-alien:int)))
    (values (lambda ()
              (%get-window-size window w h)
              (values (sb-alien:deref w) (sb-alien:deref h)))
            (lambda () (sb-alien:free-alien w) (sb-alien:free-alien h)))))

(defun window-pixels (v)
  "The window's size in PIXELS — what the framebuffer should be, so the desktop is drawn at
   the panel's resolution rather than upscaled to it."
  (funcall (v-pixel-probe v)))

(defun window-scale (v)
  "Pixels per point for this window: 2 on a Retina panel, 1 on an ordinary one, and 3/2 or
   similar under fractional scaling.  Rational on purpose — this is what a seat's density
   is set from, and rounding it here would throw away the fractional case at the one point
   where it is still exact."
  (multiple-value-bind (pw ph) (window-pixels v)
    (multiple-value-bind (lw lh) (window-points v)
      (declare (ignore ph lh))
      (if (and (plusp lw) (plusp pw)) (/ pw lw) 1))))

(defun window-points (v)
  "The window's size in POINTS — the space mouse events arrive in.

   Two struct reads inside SDL: no syscall, no round trip to the window server, and with
   MAKE-SIZE-PROBE's cells, no allocation.  Cheap enough to ask on every frame, which is
   what SETTLE-SIZE relies on."
  (funcall (v-size-probe v)))

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
        ;; POINTS in, PIXELS out.  With ALLOW_HIGHDPI these differ by the display's scale
        ;; even when nothing is being stretched, so on a 2x panel this is no longer an
        ;; identity in the ordinary case — it is the conversion that keeps the cursor on
        ;; what it is pointing at.  The arithmetic is the same either way: the ratio the
        ;; picture was scaled by is the ratio the input must be scaled by.
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

(defun settle-size (v)
  "Make the desktop the size the window ACTUALLY is, if it has drifted.  True when it did
   something, so the caller can skip a frame it is about to redraw anyway.

   Called every frame, because events have proved not to be a complete account of this.  A
   window is created at whatever size the system decided to grant and no SIZE_CHANGED
   follows, since from the window's point of view nothing changed; the same silence applies
   when %SET-WINDOW-SIZE is capped on the way out, which is the desktop-initiated half of
   the identical bug.  Both were being caught in one specific place each.  Asking outright
   catches those and whatever else behaves this way — a display reconfiguration, a
   fullscreen toggle, a tiling window manager that simply has opinions.

   THE WINDOW IS THE TRUTH, always in that direction, which is what keeps this from
   fighting the resize path that pushes the other way.  When the desktop resizes itself the
   loop asks the window to follow; if the request is granted the two agree and this does
   nothing, and if it is capped this adopts what was actually given.  Either way it settles
   in one step instead of leaving a scaled desktop nobody was told about.

   Costs a struct read per frame — the render below is a four-megabyte texture upload."
  (multiple-value-bind (gw gh) (window-pixels v)
    ;; PIXELS, not points: the framebuffer is the drawable, so that is the number it has to
    ;; agree with.  Comparing against points would leave a 2x window permanently "drifted"
    ;; by exactly the scale factor and resize the desktop every single frame.
    ;;
    ;; A REQUEST IN FLIGHT IS NOT A DRIFT.  While we are waiting to be given a size we asked
    ;; for, the window still reads as whatever it was, and acting on that would undo the
    ;; request — the desktop and the window would each keep answering the other's last word.
    ;; Wait for the size to arrive (request granted, nothing to do) or for the grace to run
    ;; out (request capped or refused, and then whatever we actually have is the truth).
    (when (v-pending-w v)
      (cond ((and (eql gw (v-pending-w v)) (eql gh (v-pending-h v)))
             (setf (v-pending-w v) nil (v-pending-h v) nil))
            ((plusp (v-pending-frames v))
             (decf (v-pending-frames v))
             (return-from settle-size nil))
            (t (setf (v-pending-w v) nil (v-pending-h v) nil))))
    ;; A SECOND FLOOR, for a server that reports under its own minimum — a miniaturise, a
    ;; display change, a platform where the minimum is advisory.  Belt and braces on
    ;; purpose: what this prevents is not a small window, it is a desktop that cannot be
    ;; recovered without a control socket.
    (let ((floor-px (max 1 (round (* *min-points* (v-display-scale v))))))
      (when (or (< gw floor-px) (< gh floor-px))
        (return-from settle-size nil)))
    (when (and (plusp gw) (plusp gh)
               (not (and (eql gw (v-width v)) (eql gh (v-height v)))))
      (let ((r (v-source v)))
        ;; Marked BEFORE the call, because WANT-SIZE resizes the seat synchronously for a
        ;; local viewer and the resize callback fires inside it.
        (setf (v-resize-from-window v) t)
        (funcall (src-want-size r) gw gh)
        (let ((nw (funcall (src-width r))) (nh (funcall (src-height r))))
          ;; Only when the source actually took the new size.  A remote desktop answers on
          ;; its own schedule, and rebuilding the texture against a size it has not adopted
          ;; would upload the old framebuffer at the new stride — a sheared picture, which
          ;; is a worse failure than the scaling this exists to remove.
          (when (and (eql nw gw) (eql nh gh)
                     (not (and (eql nw (v-width v)) (eql nh (v-height v)))))
            (setf (v-width v) nw (v-height v) nh)
            (make-texture v)
            t))))))

(defvar *live-viewer* nil
  "The viewer the live-resize watch should redraw, or NIL when no window is being viewed.

   A special rather than the watch's USERDATA because a Lisp object cannot be handed to C
   as a void* and got back without a registry, and there is one viewer per process here.")

(defvar *in-live-draw* nil
  "Guard against re-entering the watch from inside its own redraw: presenting a frame can
   pump events, and a watch that renders from within a render is a stack overflow with a
   confusing backtrace.")

(sb-alien:define-alien-callable live-resize-watch sb-alien:int
    ((userdata (* t)) (event (* t)))
  "Draw while the window is being dragged, which the main loop cannot do.

   THE MECHANIC, since it is not obvious and is entirely Cocoa's.  Dragging a window edge
   puts AppKit into a modal event-tracking run loop that does not return until the mouse is
   released.  Our loop is blocked inside SDL_PollEvent for that whole time, so no frame is
   drawn and the window shows stale or blank content until the drag ends — which is the
   'not live-resizing' this fixes, and is expected behaviour for any SDL program on macOS
   that only pumps the queue.  It is not a glass bug and it is not a bug in SDL.

   An event WATCH is called where the event is GENERATED rather than where it is polled,
   so it runs inside that modal loop.  Redrawing from here is the documented way out, and
   is why this is a C callback and not another branch in HANDLE-EVENT.

   Float traps are masked because this is entered from C, where SBCL's usual masking is not
   in force, and the compositor underneath does float arithmetic.  Errors are swallowed:
   this runs inside somebody else's run loop, and a condition unwinding through Cocoa is a
   crash rather than a backtrace."
  (declare (ignore userdata))
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow :underflow :divide-by-zero)
    (let ((v *live-viewer*)
          (sap (sb-alien:alien-sap event)))
      (when (and v (not *in-live-draw*) (v-texture v)
                 (= (ev-u32 sap 0) +window-event+)
                 ;; RESIZED is the one that arrives during a drag; SIZE_CHANGED covers the
                 ;; programmatic case, and taking both costs a redraw that was due anyway.
                 (let ((what (ev-u8 sap 12)))
                   (or (= what +windowevent-resized+)
                       (= what +windowevent-size-changed+))))
        (let ((*in-live-draw* t))
          (ignore-errors
            ;; THE SIZE IS STATE; THE PICTURE IS ONLY A PICTURE.  Different obligations and
            ;; very different costs, which is what makes eliding possible at all.  Measured
            ;; here: adopting a new size is 0.81 ms, a draw is 8.33 ms — and nearly all of
            ;; the draw is the wait for vblank, so the resize is free in the noise beside it.
            ;;
            ;; The size is therefore taken on EVERY event.  Skipping it would leave the
            ;; window and the desktop disagreeing, and until something corrected that every
            ;; frame would be scaled and every click misplaced.
            (settle-size v)
            ;; The draw is skippable, and skipping one costs an intermediate frame nobody
            ;; will miss mid-drag.  Self-pacing rather than a tuned constant: draw again
            ;; once as much time has passed as the last draw took.  At the normal 8 ms that
            ;; is every event and the drag is smooth; if a draw ever cost 100 ms — a much
            ;; bigger desktop, a slower machine, a compositor doing more — it spaces itself
            ;; out and the drag stays responsive with fewer frames in it.  Nothing to detect
            ;; separately and no threshold to pick: the measurement IS the policy, and one
            ;; slow frame buys one skip window rather than a standing penalty.
            (let ((since (/ (* 1000.0 (- (get-internal-real-time) (v-last-draw-at v)))
                            internal-time-units-per-second)))
              (when (>= since (v-last-draw-ms v))
                (let ((start (get-internal-real-time)))
                  (push-frame v)
                  (setf (v-last-draw-at v) (get-internal-real-time)
                        (v-last-draw-ms v)
                        (/ (* 1000.0 (- (v-last-draw-at v) start))
                           internal-time-units-per-second))))))))))
  1)                                    ; keep the event; a watch must not consume it

(defun push-frame (v)
    "Upload the remote's framebuffer and present it.

     The whole screen, not the damaged part: glass/client reports dirtiness as a
     flag rather than a rectangle list, so there is nothing finer to act on, and a
     1280x800 upload is four megabytes of memcpy that a GPU eats without noticing.
     Idle costs nothing because we only get here when the flag was set."
    ;; Before the fb is read, so a size settled here is reflected by this very frame rather
    ;; than by the next one.
    (settle-size v)
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

  (defvar *viewer* nil
  "The viewer currently showing a desktop in this image, or NIL.

   Kept so that things outside this file can reach its devices — muting the speakers or the
   microphone, asking whether they are muted — without being handed a VIEWER they have no
   other use for.  One, because a process has one window; the same reasoning SESSION-MIC uses
   for one microphone.")

(defun refresh-title (v)
  "Put the device state in the window's title.

   THE ONE PIECE OF REAL CHROME THIS PROGRAM HAS.  A desktop's own title bars are pixels it
   draws; the window title is the host's, so it is visible when the desktop is behind
   something, in the window list, and in the switcher — which is where you look to ask \"is my
   microphone open\" without first finding the window and opening a menu.

   Present when ON and absent when off, rather than a pair of symbols that swap.  A muted
   microphone should not put a microphone in the title: the question being asked is whether
   something is live, and the honest answer to no is nothing at all.  It also keeps a session
   whose devices are both quiet looking exactly like a session that has none, which is what it
   is from the room's point of view."
  (when (and v (v-window v))
    ;; SHOWING-P, not OPEN-P: the indicator outlives the device by a moment on purpose, so a
    ;; brief capture cannot flash past.  See *MIC-LINGER-SECONDS*.
    (let* ((mic (and (audio-in-showing-p (v-mic v)) (not (audio-in-muted-p (v-mic v)))))
           (spk (and (v-audio v) (not (audio-out-muted-p (v-audio v)))))
           (name (format nil "~a~@[ ~a~]~@[ ~a~]" (v-title v)
                         (and spk (string (code-char #x1F50A)))     ; speaker with waves
                         (and mic (string (code-char #x1F3A4))))))  ; microphone
      (ignore-errors (sdl (%set-window-title (v-window v) name))))))

(defun mute-speakers (&optional (on t))
  "Silence this machine's speakers, or unsilence with NIL.  Returns the new state."
  (prog1 (and *viewer* (setf (audio-out-muted-p (v-audio *viewer*)) on))
    (refresh-title *viewer*)))

(defun mute-microphone (&optional (on t))
  "Mute this machine's microphone, or unmute with NIL.  Returns the new state."
  (prog1 (and *viewer* (setf (audio-in-muted-p (v-mic *viewer*)) on))
    (refresh-title *viewer*)))

(defun speakers-muted-p () (and *viewer* (audio-out-muted-p (v-audio *viewer*))))
(defun microphone-muted-p () (and *viewer* (audio-in-muted-p (v-mic *viewer*))))

(defun view (&key (host "127.0.0.1") (port 5901) seat (title nil) (fps 60) (audio t)
                  (mic :auto))
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
                                        ;; IN POINTS, which is what SDL_CreateWindow takes — the framebuffer is in PIXELS.
                                        ;; Handing it pixels asks for a window SCALE times too big on a Retina panel; it
                                        ;; only looked right while the request happened to exceed the screen and was
                                        ;; clamped back, which is a coincidence that held for one --size and no other.
                                        ;; DISPLAY-SCALE answers before this window exists, which is what it is for.
                                        (max 1 (round (v-width v) (display-scale)))
                                        (max 1 (round (v-height v) (display-scale)))
                                        (logior +window-shown+ +window-resizable+
                                                ;; Ask for the panel's real pixels.  Without this macOS gives a
                                                ;; 1x backing store and the window server upscales it -- we pay
                                                ;; for a Retina display and decline to use it.
                                                +window-allow-highdpi+))))
             (when (sb-alien:null-alien (v-window v))
               (error "glass-sdl: could not open a window: ~a" (sdl (%get-error))))
             ;; Before anything asks the window its size, which the very next form does.
             (multiple-value-bind (probe free) (make-size-probe (v-window v))
               (setf (v-size-probe v) probe (v-free-size-probe v) free))
             (multiple-value-bind (probe free) (make-pixel-probe (v-window v))
               (setf (v-pixel-probe v) probe (v-free-pixel-probe v) free))
             ;; Before anything measures it, so the first measurement is already floored.
             (sdl (%set-window-minimum-size (v-window v) *min-points* *min-points*))
             ;; ...and the display's density, ASKED OF THE SEAT, which is the only party that
             ;; reliably knows it here.  Two closer sources were tried and are both wrong:
             ;; measuring THIS window now returns 1, because SDL reports the drawable as the
             ;; window's own size until the backing store exists, which is after it is shown; and
             ;; DISPLAY-SCALE's probe returns 1 from the other end, because its window is HIDDEN
             ;; and one that is never mapped never gets a backing store to be twice the size of.
             ;; Either answer halves every points<->pixels conversion below, which is what drove an
             ;; 1800-pixel request down into the minimum-size floor.
             ;;
             ;; kiln asks DISPLAY-SCALE before any window exists — the one moment the probe is
             ;; right — and hands it to the session, so the seat is already carrying the number
             ;; the whole desktop is drawn at.  A second opinion about that would be one too many.
             (setf (v-display-scale v)
                   (or (let ((f (and (find-package "CLIM-GLASS")
                                     (find-symbol "SEAT-SCALE" "CLIM-GLASS"))))
                         (and seat f (fboundp f) (ignore-errors (funcall f seat))))
                       1))
             ;; The same settling SETTLE-SIZE does every frame, done once here because the
             ;; texture below should be created at the right size rather than made, replaced
             ;; and thrown away on the first frame.  See SETTLE-SIZE for why a window's
             ;; granted size has to be asked for rather than waited for.
             (multiple-value-bind (gw gh) (window-pixels v)
               (when (and (plusp gw) (plusp gh)
                          (not (and (eql gw (v-width v)) (eql gh (v-height v)))))
                 (funcall (src-want-size r) gw gh)
                 (setf (v-width v) (funcall (src-width r))
                       (v-height v) (funcall (src-height r)))))
             ;; THE SEAT LEARNS ITS DENSITY, which is the point of the whole exercise.  The
             ;; desktop now has as many pixels as the panel does, and the seat knows each one
             ;; is half the size it used to assume — so text scaled through SEAT-PPEM comes
             ;; out sharp rather than small.  On an ordinary display the ratio is 1 and this
             ;; is the identity, so nothing moves for anyone without the pixels to spare.
             ;;
             ;; By name: glass-sdl must not depend on the window-manager layer in order to be
             ;; a viewer, which is the rule the rest of this file already follows.
             (let ((setter (and seat (find-package "CLIM-GLASS")
                                     (find-symbol "SEAT-SCALE" "CLIM-GLASS"))))
               (when (and setter (fboundp (list 'setf setter)))
                 (ignore-errors
                   (funcall (fdefinition (list 'setf setter)) (window-scale v) seat))))
             (setf (v-renderer v)
                   (sdl (%create-renderer (v-window v) -1
                                          (logior +renderer-accelerated+ +renderer-presentvsync+))))
             (when (sb-alien:null-alien (v-renderer v))
               ;; No GPU path (a bare VM, a stubborn driver): software still draws.
               (setf (v-renderer v) (sdl (%create-renderer (v-window v) -1 0))))
             (make-texture v)
             ;; Only now: the watch can fire the moment it is registered, and it draws.
             ;; SOUND, for a seat whose mixer is in this image.  Only for :SEAT: a remote RFB
           ;; desktop's audio arrives on its own socket and is that transport's business,
           ;; while a welded desktop has no socket at all and the mix is simply here.
           ;; The name WITHOUT indicators, so REFRESH-TITLE has something to build on rather
           ;; than having to parse emoji back out of a title it wrote.
           (setf (v-title v)
                 (or title
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
                         (format nil "glass — ~a:~d" host port))))
           (when (and audio seat)
             ;; THIS SEAT'S OWN MIX IF IT HAS ONE, otherwise the session's.  SEAT-MIX is a
             ;; HEADSET's — audio addressed to one person, reading their selection aloud —
             ;; and it is NIL on a seat with no headset, which is nearly every seat.  What a
             ;; desktop sounds like is the session mix, which is what the audio socket serves
             ;; to a remote listener and what a local one should hear for the same reason.
             (let ((mix (or (let ((f (and (find-package "CLIM-GLASS")
                                          (find-symbol "SEAT-MIX" "CLIM-GLASS"))))
                              (and f (fboundp f) (ignore-errors (funcall f seat))))
                            (let ((f (and (find-package "GLASS")
                                          (find-symbol "SESSION-MIXER" "GLASS"))))
                              (and f (fboundp f) (ignore-errors (funcall f)))))))
               (setf (v-audio v) (start-audio mix)))
             ;; ...and the other direction.  Attached even when there is no recognizer to read
             ;; it: what a microphone is for is not this file's business, and the Mixer window
             ;; showing an attached-but-silent device is how a denied permission prompt becomes
             ;; visible instead of looking like a quiet room.
             ;; THE MICROPHONE, IN ONE OF THREE POSTURES.
             ;;
             ;;   :OFF    no provider, no device, ever.  Something that listens hears the
             ;;           session mix, which is what a desktop with no microphone sounds like
             ;;           and is a true answer rather than a broken one.
             ;;   :AUTO   the ability is registered and the device is taken the first time
             ;;           something asks to be heard, then given back when nobody is taking
             ;;           frames.  The permission prompt happens because you asked to be
             ;;           heard, not because a desktop started.
             ;;   :ON     taken now and held.  For a session whose whole purpose is listening,
             ;;           where re-opening on every gap is worse than holding it.
             ;;
             ;; :OFF registers nothing rather than registering a provider that refuses, so
             ;; GLASS:ENSURE-MIC's answer is the honest "nothing here can open one".
             (when (member mic '(:on :auto))
               (let ((provider (and (find-package "GLASS")
                                    (find-symbol "*MIC-PROVIDER*" "GLASS"))))
                 (when provider
                   (setf (symbol-value provider)
                       ;; NO WINDOW WORK HERE.  This runs on whatever thread first wanted a
                       ;; microphone — the ear's, in practice — and NSWindow may only be
                       ;; touched on the main thread; doing it here terminated the process
                       ;; with an uncaught ObjC exception, which is not a condition anything
                       ;; in Lisp can catch.  Opening the audio device off-thread is fine and
                       ;; is all this does; the title catches up in the main loop, which is
                       ;; where it is allowed to.
                       (lambda ()
                         ;; OPEN-P and not merely non-NIL: a microphone given back after going
                         ;; idle leaves its AUDIO-IN behind, and asking for one again has to
                         ;; open a device rather than hand back the closed one.
                         (if (audio-in-open-p (v-mic v))
                             (ai-mic (v-mic v))
                             (let ((ai (start-mic :idle (when (eq mic :auto)
                                                          *mic-idle-seconds*))))
                               (setf (v-mic v) ai)
                               (and ai (ai-mic ai)))))))
               ;; :ON takes it now, through the same provider, so there is one path that opens
               ;; a microphone and one place that decides how long it is held.
               (when (eq mic :on)
                 (let ((f (and (find-package "GLASS") (find-symbol "ENSURE-MIC" "GLASS"))))
                   (when (and f (fboundp f)) (ignore-errors (funcall f)))))))
             (refresh-title v))
           (setf *live-viewer* v *viewer* v)
             (%add-event-watch (sb-alien:cast (sb-alien:alien-callable-function 'live-resize-watch)
                                              (* t))
                               (null-ptr))
             (sdl (%start-text-input))
             (let ((sap (sb-alien:alien-sap ev))
                   (frame-ms (max 1 (floor 1000 fps))))
               (loop
                 (loop while (plusp (sdl (%poll-event (sb-alien:sap-alien sap (* t)))))
                       do (unless (handle-event v sap) (return-from view t)))
                 (when (v-resized v)
                   ;; WHO ASKED DECIDES WHETHER TO ANSWER.
                   ;;
                   ;; A resize that came from the WINDOW is already true of the window; pushing it back
                   ;; tells the window a size it already has.  Doing that to a window somebody is dragging
                   ;; is how the desktop vanished on a one-pixel drag: the drag moved the edge, SETTLE-SIZE
                   ;; matched the desktop to it, and this told the window to become that — mid-drag,
                   ;; against the pointer — and the server answered a shade smaller each time round.  The
                   ;; PENDING guard could not help, because every step was a real request being really
                   ;; granted; the loop was between two correct behaviours, which is the kind that
                   ;; converges on nothing.
                   ;;
                   ;; A resize from the DESKTOP side — an application asking for a size, a script, the
                   ;; initial adopt — still has to reach the window, because nothing else will tell it.
                   ;; That is the only case this should act on.
                   (let ((from-window (v-resize-from-window v)))
                     (setf (v-resized v) nil (v-resize-from-window v) nil)
                     ;; THE WATCH MUST NOT RUN INSIDE THIS.  %SET-WINDOW-SIZE delivers its SIZE_CHANGED
                     ;; synchronously, so the watch fires from within the call below — after the width has
                     ;; been updated and before MAKE-TEXTURE has rebuilt to match, which is a frame at the
                     ;; wrong stride and SETTLE-SIZE reading a half-applied state.  It ratcheted a
                     ;; 1456-wide window down to 1029.
                     (let ((*in-live-draw* t))
                       (setf (v-width v) (funcall (src-width r))
                             (v-height v) (funcall (src-height r)))
                       (unless from-window
                         ;; Asked for, not yet granted — see the PENDING slots.  PENDING is in PIXELS
                         ;; because that is what SETTLE-SIZE compares; %SET-WINDOW-SIZE is told POINTS.
                         (setf (v-pending-w v) (v-width v) (v-pending-h v) (v-height v)
                               (v-pending-frames v) 30)
                         (let ((sc (v-display-scale v)))
                           (sdl (%set-window-size (v-window v)
                                                  (max *min-points* (round (v-width v) sc))
                                                  (max *min-points* (round (v-height v) sc))))))
                       (make-texture v))
                     (push-frame v)))
               ;; The title says which devices are live, and the devices can come and go on
               ;; other threads — a microphone opens the first time something listens.  Only
               ;; when it CHANGES: SDL_SetWindowTitle on every frame is a syscall per frame
               ;; for a string that is almost always the same one.
               (let ((now (list (and (audio-in-showing-p (v-mic v))
                                     (not (audio-in-muted-p (v-mic v))))
                                (and (v-audio v) (not (audio-out-muted-p (v-audio v)))))))
                 (unless (equal now (v-devices-shown v))
                   (setf (v-devices-shown v) now)
                   (refresh-title v)))
               (if (funcall (src-dirty-p r))
                   (push-frame v)
                   (sdl (%delay frame-ms)))
               (unless (funcall (src-live-p r))
                 ;; The client reconnects on its own; say so rather than dying.
                 (sdl (%set-window-title (v-window v) "glass — reconnecting…"))
                 (sdl (%delay 200))))))
      (ignore-errors (funcall (src-stop (v-source v))))
      ;; Off FIRST, before anything it touches is destroyed — a watch firing against a
      ;; freed texture is a crash inside Cocoa, the worst place to have one.
      (ignore-errors (stop-audio (v-audio v)))
      (ignore-errors (stop-mic (v-mic v)))
      (%del-event-watch (sb-alien:cast (sb-alien:alien-callable-function 'live-resize-watch)
                                       (* t))
                        (null-ptr))
      (setf *live-viewer* nil *viewer* nil)
      (when (v-free-size-probe v)
        (ignore-errors (funcall (v-free-size-probe v)))
        (setf (v-free-size-probe v) nil (v-size-probe v) nil))
      (when (v-free-pixel-probe v)
        (ignore-errors (funcall (v-free-pixel-probe v)))
        (setf (v-free-pixel-probe v) nil (v-pixel-probe v) nil))
      (when (v-texture v) (ignore-errors (sdl (%destroy-texture (v-texture v)))))
      (when (v-renderer v) (ignore-errors (sdl (%destroy-renderer (v-renderer v)))))
      (when (v-window v) (ignore-errors (sdl (%destroy-window (v-window v)))))
      (ignore-errors (sb-alien:free-alien ev))
      (ignore-errors (sdl (%quit))))))
