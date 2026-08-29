;;;; audio-in.lisp — this machine's microphone, into the session.
;;;;
;;;; The SOURCE half of the pair audio-out.lisp is the sink half of.  glass names the two
;;;; directions apart and means it: a sink is a read cursor on the session's mix, a mic is a
;;;; thing that produces into the session, and they are not symmetrical objects even though
;;;; they are symmetrical ideas.
;;;;
;;;; THE MICROPHONE IS NOT A MIXER SOURCE, which is worth stating because it is the obvious
;;;; wrong answer and it is wrong for a reason that gets worse locally.  A voice on the session
;;;; mixer is played out of the desktop's own audio, so over a gateway it goes back down the
;;;; wire to whoever said it — an echo with a network round trip in it.  Here it would go to
;;;; the speakers a foot from the microphone, which is not an echo but a feedback loop.  So the
;;;; mic is attached with ATTACH-MIC and read by whoever asked for it, which today is the ear.
;;;;
;;;; CAPTURE IS QUEUED for the same reason playback is: SDL fills the device's queue on its own
;;;; real-time thread, and we take whole frames off it whenever we get round to it.  Nothing of
;;;; ours runs where a GC pause would be a click.
;;;;
;;;; PERMISSION IS A REAL FAILURE MODE and not an error.  macOS asks the user the first time a
;;;; process opens a capture device, and a process that is denied gets a device that opens and
;;;; returns silence forever.  That is indistinguishable from a quiet room at this layer, which
;;;; is exactly why MIC-LIVE-P asks whether anything has ever arrived: an unanswered permission
;;;; prompt leaves the microphone attached and never live, and the Mixer window shows it as a
;;;; source with no level rather than as nothing at all.

(in-package #:glass-sdl)

(defparameter *mic-rate* 16000
  "Capture rate.  16 kHz because that is GLASS:*MIC-RATE* and the ear's model's front end was
built at it — asking the device for the rate the consumer wants means no resampler anywhere in
this path, and audio fed to a recognizer at the wrong rate produces confident nonsense rather
than an error.")

(defparameter *mic-frame-ms* 20
  "Milliseconds per frame handed to glass, matching the mixer's period and the socket path's.")

(defparameter *mic-linger-seconds* 4
  "Seconds the title keeps showing a microphone after the device has been given back.

A MacBook's camera light stays on for a moment after capture stops, and that is not a delay
in turning it off — it is the point.  An indicator that tracks the hardware exactly can be
true for a tenth of a second, which is long enough to have recorded you and short enough that
nobody sees it.  Lingering makes a brief capture impossible to miss.

It errs the SAFE way: the title can claim a microphone is live slightly after it has stopped,
and never the reverse.  Of the two possible lies that is the harmless one.")

(defparameter *mic-idle-seconds* 5
  "Seconds of nobody listening before the microphone is given back.

The device is taken when something first asks to hear you; this is the other half of that
bargain.  A microphone left open because a transcript was closed an hour ago is a recording
light on for nothing, and on this platform it is also a device held away from every other
program on the machine.

Long enough to survive the gap between stopping one listener and starting another — turning
dictation off to read what it typed and turning it back on should not cost a permission
prompt — and short enough that closing the last thing that listens visibly ends it.  NIL keeps
it open, for a caller who would rather hold the device than re-open it.")

(defstruct (audio-in (:conc-name ai-))
  (device 0 :type (unsigned-byte 32))
  mic
  thread
  (stop nil)
  (muted nil)
  ;; For the idle reaper: the consumer's frame count as last seen, and when it last moved.
  (seen-frames -1 :type fixnum)
  (seen-at 0 :type integer)
  ;; ...and when the device was given back, so the indicator can outlive it by a moment.
  (released-at 0 :type integer)
  ;; Per-device, so one viewer can hold a microphone open while another lets it go: NIL means
  ;; hold it, which is what --mic=on asks for.
  (idle-seconds *mic-idle-seconds*))

(defun start-mic (&key (rate *mic-rate*) (frame-ms *mic-frame-ms*) (name "local")
                       (idle *mic-idle-seconds*))
  "Open this machine's microphone and attach it to the session.  Returns an AUDIO-IN, or NIL
   if there is no capture device or glass has no microphone object in this image.

   NIL is a legitimate answer twice over — a box with no microphone, and a desktop built
   without :glass/mic — and neither should stop a desktop starting."
  (handler-case
      (let ((make   (and (find-package "GLASS") (find-symbol "MAKE-MIC" "GLASS")))
            (attach (and (find-package "GLASS") (find-symbol "ATTACH-MIC" "GLASS"))))
        (unless (and make attach (fboundp make) (fboundp attach))
          (return-from start-mic nil))
        (load-sdl)
        (sdl (%init +init-audio+))
        (let* ((frame (max 1 (round (* rate frame-ms) 1000)))
               (spec (%audio-spec rate 1 (ash 1 (max 6 (integer-length (1- frame))))))
               ;; ISCAPTURE 1, and ALLOWED-CHANGES 0: we want exactly this rate and mono, and
               ;; SDL converts internally when the hardware disagrees.  Letting it hand back
               ;; something else would put the conversion here, where the rate the ear needs is
               ;; not obviously anyone's business.
               (dev (unwind-protect
                         (sdl (%open-audio-device (sb-alien:sap-alien (sb-sys:int-sap 0)
                                                                      sb-alien:c-string)
                                                  1 (sb-alien:cast spec (* t))
                                                  (null-ptr) 0))
                      (sb-alien:free-alien spec))))
          (when (zerop dev)
            (format *error-output* "~&glass-sdl: no microphone — ~a~%" (sdl (%get-error)))
            (return-from start-mic nil))
          (let* ((mic (funcall make :name name :wire-rate rate :rate rate
                                    :wire-frame frame :frame-samples frame))
                 (ai (make-audio-in :device dev :mic mic :idle-seconds idle)))
            (funcall attach mic)
            (sdl (%pause-audio-device dev 0))          ; 0 = start capturing
            (setf (ai-thread ai)
                  (sb-thread:make-thread (lambda () (pump-mic ai)) :name "glass-sdl-mic"))
            ai)))
    (error (e)
      (format *error-output* "~&glass-sdl: microphone unavailable — ~a~%" e)
      nil)))

(defun pump-mic (ai)
  "Take whole frames off the capture queue and push them into the session's microphone,
   until nobody is taking them any more.

   WHOLE FRAMES ONLY.  A partial frame is not a small frame, it is a frame boundary in the
   wrong place, and every consumer downstream — the ear's window, the level gate — counts in
   frames.  So the queue is drained by the frame and a remainder is left where it is until the
   rest of it arrives, which costs one poll interval and never costs a boundary."
  (let* ((push (and (find-package "GLASS") (find-symbol "MIC-PUSH" "GLASS")))
         (frame (max 1 (round (* *mic-rate* *mic-frame-ms*) 1000)))
         (bytes (* 2 frame))
         (buf (make-array bytes :element-type '(unsigned-byte 8)))
         (pcm (make-array frame :element-type '(signed-byte 16))))
    (loop until (ai-stop ai)
          do (when (%idle-too-long-p ai)
               ;; GIVE THE DEVICE BACK HERE, in this thread, rather than returning and leaving
               ;; somebody else to notice.  STOP-MIC joins this thread, so it cannot be the one
               ;; to do it; the teardown is the same minus the join.
               (%release ai "nobody listening")
               (return))
             (handler-case
                 (let ((got (sb-sys:with-pinned-objects (buf)
                              (sdl (%dequeue-audio (ai-device ai)
                                                   (sb-alien:sap-alien (sb-sys:vector-sap buf) (* t))
                                                   bytes)))))
                   (cond
                     ((< got bytes)
                      ;; Not a whole frame yet.  Sleep less than a frame so the queue never has
                      ;; time to build a backlog we would then be chasing.
                      (sdl (%delay 5)))
                     (t
                      ;; MUTED PUSHES SILENCE rather than pushing nothing.  A microphone that
                      ;; stops sending goes not-live, and the ear then falls back to the
                      ;; session mix — so muting yourself would make the desktop start
                      ;; transcribing its own audio, which is a surprising thing for a mute
                      ;; button to do.  Silence keeps the source stable and the room quiet,
                      ;; which is what was asked for.
                      (if (ai-muted ai)
                          (fill pcm 0)
                          (dotimes (i frame)
                            (let ((v (logior (aref buf (* 2 i)) (ash (aref buf (1+ (* 2 i))) 8))))
                              (setf (aref pcm i) (if (> v 32767) (- v 65536) v)))))
                      (when push (funcall push (ai-mic ai) pcm)))))
               (error (e)
                 (format *error-output* "~&glass-sdl: microphone stopped — ~a~%" e)
                 (setf (ai-stop ai) t))))))

(defun %release (ai why)
  "Detach the microphone and give the capture device back, without joining anything.

   THE INDICATOR HAS TO FOLLOW THE DEVICE, which is the whole reason this sets the device to
   zero rather than just setting a flag.  A MacBook's camera light is wired to the camera's
   power, so there is no state in which the camera is on and the light is not; the title here
   should work the same way — AUDIO-IN-OPEN-P asks whether there is a device, so a microphone
   that has been given back cannot leave a microphone in the title.  A separate `mic is on'
   boolean would be a light with its own wiring, and the first bug in it would be exactly the
   one worth preventing."
  (let ((detach (and (find-package "GLASS") (find-symbol "DETACH-MIC" "GLASS"))))
    (when (and detach (fboundp detach) (ai-mic ai))
      (ignore-errors (funcall detach (ai-mic ai)))))
  (ignore-errors (sdl (%pause-audio-device (ai-device ai) 1)))
  (ignore-errors (sdl (%close-audio-device (ai-device ai))))
  (setf (ai-device ai) 0 (ai-stop ai) t (ai-released-at ai) (get-internal-real-time))
  (format *error-output* "~&glass-sdl: microphone released — ~a~%" why)
  (finish-output *error-output*)
  nil)

(defun audio-in-open-p (ai)
  "Whether this microphone actually holds a device right now.

   Asked of the device and not of a flag, so nothing can be listening while the title says it
   is not, or the reverse.  See %RELEASE."
  (and ai (plusp (ai-device ai)) (not (ai-stop ai))))

(defun audio-in-showing-p (ai)
  "Whether the title should show a microphone: one is open, or one was open just now.

   See *MIC-LINGER-SECONDS*.  AUDIO-IN-OPEN-P is the truth about the device and this is what
   an indicator should say about it, which are deliberately not the same question — an
   indicator that matched the hardware exactly could be true for a tenth of a second, which is
   long enough to have heard you and too short to notice."
  (and ai
       (or (audio-in-open-p ai)
           (and *mic-linger-seconds*
                (plusp (ai-released-at ai))
                (< (- (get-internal-real-time) (ai-released-at ai))
                   (* *mic-linger-seconds* internal-time-units-per-second))))))

(defun %idle-too-long-p (ai)
  "True when nothing has taken a frame from this microphone for *MIC-IDLE-SECONDS*.

   ASKED OF THE MICROPHONE, not of the ear.  MIC-FRAMES counts what has been handed to a
   consumer, so a count that stops moving IS nobody listening — whoever the consumer was, and
   whether it stopped politely or simply went away.  Asking the ear instead would mean this
   file knowing what an ear is, and would still be wrong for every other thing that might read
   a microphone."
  (let ((limit (ai-idle-seconds ai))
        (mic (ai-mic ai)))
    (when (and limit mic)
      (let ((n (ignore-errors (funcall (find-symbol "MIC-FRAMES" "GLASS") mic)))
            (now (get-internal-real-time)))
        (cond ((null n) nil)
              ;; moved, or first look: reset the clock and keep going
              ((/= n (ai-seen-frames ai))
               (setf (ai-seen-frames ai) n (ai-seen-at ai) now)
               nil)
              ;; never moved at all: time it from when we started watching, not from zero
              ((zerop (ai-seen-at ai))
               (setf (ai-seen-at ai) now)
               nil)
              (t (> (- now (ai-seen-at ai))
                    (* limit internal-time-units-per-second))))))))

(defun stop-mic (ai)
  "Stop capturing, detach the microphone, give the device back.  Safe on NIL."
  (when ai
    (setf (ai-stop ai) t)
    (when (ai-thread ai) (ignore-errors (sb-thread:join-thread (ai-thread ai) :default nil)))
    (let ((detach (and (find-package "GLASS") (find-symbol "DETACH-MIC" "GLASS"))))
      (when (and detach (fboundp detach) (ai-mic ai))
        (ignore-errors (funcall detach (ai-mic ai)))))
    (ignore-errors (sdl (%pause-audio-device (ai-device ai) 1)))
    (ignore-errors (sdl (%close-audio-device (ai-device ai))))
    (setf (ai-device ai) 0))
  nil)


(defun audio-in-muted-p (ai) (and ai (ai-muted ai)))

(defun (setf audio-in-muted-p) (on ai)
  "Mute the microphone without unplugging it.

   See PUMP-MIC: muting pushes SILENCE rather than pushing nothing.  A microphone that stops
   sending goes not-live, and the ear falls back to the session mix — so a mute button would
   make the desktop begin transcribing its own audio, which is not what anybody means by mute.
   The device stays open and the source stays stable; the room simply goes quiet."
  (when ai (setf (ai-muted ai) (and on t)))
  (and ai (ai-muted ai)))
