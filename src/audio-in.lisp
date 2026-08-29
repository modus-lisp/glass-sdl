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

(defstruct (audio-in (:conc-name ai-))
  (device 0 :type (unsigned-byte 32))
  mic
  thread
  (stop nil))

(defun start-mic (&key (rate *mic-rate*) (frame-ms *mic-frame-ms*) (name "local"))
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
                 (ai (make-audio-in :device dev :mic mic)))
            (funcall attach mic)
            (sdl (%pause-audio-device dev 0))          ; 0 = start capturing
            (setf (ai-thread ai)
                  (sb-thread:make-thread (lambda () (pump-mic ai)) :name "glass-sdl-mic"))
            ai)))
    (error (e)
      (format *error-output* "~&glass-sdl: microphone unavailable — ~a~%" e)
      nil)))

(defun pump-mic (ai)
  "Take whole frames off the capture queue and push them into the session's microphone.

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
          do (handler-case
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
                      (dotimes (i frame)
                        (let ((v (logior (aref buf (* 2 i)) (ash (aref buf (1+ (* 2 i))) 8))))
                          (setf (aref pcm i) (if (> v 32767) (- v 65536) v))))
                      (when push (funcall push (ai-mic ai) pcm)))))
               (error (e)
                 (format *error-output* "~&glass-sdl: microphone stopped — ~a~%" e)
                 (setf (ai-stop ai) t))))))

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
