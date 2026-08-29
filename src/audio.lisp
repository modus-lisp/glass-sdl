;;;; audio.lisp — the desktop, out of this machine's speakers.
;;;;
;;;; A local viewer was silent, and had always been: glass mixes session audio and can serve
;;;; it over a socket to a remote listener, but nothing ever played it HERE.  On the welded
;;;; desktop — one process, its own window, no wire — that is the one path where a socket
;;;; makes no sense at all, so there was nothing to hear.
;;;;
;;;; QUEUED AUDIO, NOT A CALLBACK.  SDL will happily call a function on the audio device's
;;;; real-time thread every few milliseconds; pointing that at Lisp means a GC pause can land
;;;; inside the callback, and a GC pause inside an audio callback is an audible click.
;;;; SDL_QueueAudio inverts it: an ordinary Lisp thread pushes frames when convenient and SDL
;;;; drains them in C on its own thread, where nothing of ours can stall it.  What we take on
;;;; in exchange is watching the queue depth, which is a cheap thing to get right and a
;;;; visible thing to get wrong.

(in-package #:glass-sdl)

(defparameter *audio-rate* 48000
  "Playback rate.  The mixer's own is 48 kHz, so asking for it means no resampler in this
path at all — the sink hands over exactly what was mixed.")

(defparameter *audio-frame-ms* 20
  "Milliseconds per pushed frame, matching the mixer's period so one pull is one frame.")

(defparameter *audio-target-ms* 120
  "How much audio to keep queued ahead of the device.

Latency against safety, and the numbers are what decide it: a frame is 20 ms, so this is six
of them.  Less and an ordinary scheduling hiccup empties the queue, which is a click; much
more and the desktop's sounds arrive visibly after the thing that made them.  The pump tops
up toward this and stops, so a stall costs one gap rather than a growing delay that never
recovers.")

(defstruct (audio-out (:conc-name ao-))
  (device 0 :type (unsigned-byte 32))
  sink
  thread
  (stop nil))

(defun %audio-spec (freq channels samples)
  "A filled-in SDL_AudioSpec: 32 bytes, and the fields have to land where SDL expects them.
   NULL callback is what selects queued mode."
  (let ((spec (sb-alien:make-alien (sb-alien:unsigned 8) 32)))
    (dotimes (i 32) (setf (sb-alien:deref spec i) 0))
    (let ((sap (sb-alien:alien-sap spec)))
      (setf (sb-sys:signed-sap-ref-32 sap 0) freq)              ; int freq
      (setf (sb-sys:sap-ref-16 sap 4) +audio-s16lsb+)           ; SDL_AudioFormat format
      (setf (sb-sys:sap-ref-8  sap 6) channels)                 ; Uint8 channels
      (setf (sb-sys:sap-ref-16 sap 10) samples))                ; Uint16 samples
    spec))

(defun start-audio (mix &key (rate *audio-rate*) (frame-ms *audio-frame-ms*))
  "Play MIX (a glass mix or mixer) out of this machine's speakers.  Returns an AUDIO-OUT, or
   NIL if there is no audio device or glass's mixer is not in this image — a desktop with no
   sound card is still a desktop, and this is not the thing that should stop one starting."
  (handler-case
      (let* ((subscribe (and (find-package "GLASS") (find-symbol "MIXER-SUBSCRIBE" "GLASS")))
             (next      (and (find-package "GLASS") (find-symbol "SINK-NEXT-FRAME" "GLASS"))))
        (unless (and mix subscribe next (fboundp subscribe) (fboundp next))
          (return-from start-audio nil))
        (load-sdl)
        ;; VIDEO is already up by the time anyone calls this; SDL_Init is additive, so this
        ;; asks for the audio subsystem without disturbing it.
        (sdl (%init +init-audio+))
        (let* ((frame (max 1 (round (* rate frame-ms) 1000)))
               (spec (%audio-spec rate 1 (ash 1 (max 6 (integer-length (1- frame))))))
               (dev (unwind-protect
                         (sdl (%open-audio-device (sb-alien:sap-alien (sb-sys:int-sap 0)
                                                                      sb-alien:c-string)
                                                  0 (sb-alien:cast spec (* t))
                                                  (null-ptr) 0))
                      (sb-alien:free-alien spec))))
          (when (zerop dev)
            (format *error-output* "~&glass-sdl: no audio device — ~a~%" (sdl (%get-error)))
            (return-from start-audio nil))
          (let* ((sink (funcall subscribe mix :name "glass-sdl" :rate rate
                                              :frame-samples frame))
                 (ao (make-audio-out :device dev :sink sink)))
            (sdl (%pause-audio-device dev 0))                   ; 0 = play
            (setf (ao-thread ao)
                  (sb-thread:make-thread (lambda () (pump-audio ao next))
                                         :name "glass-sdl-audio"))
            ao)))
    (error (e)
      (format *error-output* "~&glass-sdl: audio unavailable — ~a~%" e)
      nil)))

(defun pump-audio (ao next)
  "Keep the device's queue topped up, and no more than topped up.

   The mixer produces on its own clock and the device consumes on another; neither is the
   other's, and the queue is what absorbs the difference.  Pushing everything available would
   grow it without bound whenever the mixer runs a shade fast, and the symptom of that is not
   a glitch — it is audio drifting further behind the picture for as long as the session
   lasts, which is much harder to notice and much worse.  So: top up toward the target, then
   stop and wait."
  (let* ((bytes-per-ms (round (* *audio-rate* 2) 1000))
         (target (* *audio-target-ms* bytes-per-ms)))
    (loop until (ao-stop ao)
          do (handler-case
                 (let ((queued (sdl (%queued-audio-size (ao-device ao)))))
                   (if (>= queued target)
                       (sdl (%delay 5))
                       (let ((pcm (funcall next (ao-sink ao))))
                         (if (null pcm)
                             ;; The mix has not produced this slot yet.  Not an error and not
                             ;; a gap: ask again.  SDL plays what is already queued meanwhile,
                             ;; which is what the cushion is for.
                             (sdl (%delay 2))
                             (let* ((n (length pcm))
                                    (octets (make-array (* 2 n) :element-type '(unsigned-byte 8))))
                               (dotimes (i n)
                                 (let ((v (ldb (byte 16 0) (aref pcm i))))
                                   (setf (aref octets (* 2 i)) (ldb (byte 8 0) v)
                                         (aref octets (1+ (* 2 i))) (ldb (byte 8 8) v))))
                               (sb-sys:with-pinned-objects (octets)
                                 (sdl (%queue-audio (ao-device ao)
                                                    (sb-alien:sap-alien (sb-sys:vector-sap octets) (* t))
                                                    (length octets)))))))))
               ;; A pump that dies takes the sound with it and nothing else; say so once and
               ;; keep the desktop.
               (error (e)
                 (format *error-output* "~&glass-sdl: audio pump stopped — ~a~%" e)
                 (setf (ao-stop ao) t))))))

(defun stop-audio (ao)
  "Stop playing and give the device back.  Safe on NIL, which is what START-AUDIO returns
   when there was no device to open."
  (when ao
    (setf (ao-stop ao) t)
    (when (ao-thread ao) (ignore-errors (sb-thread:join-thread (ao-thread ao) :default nil)))
    (let ((unsub (and (find-package "GLASS") (find-symbol "SINK-UNSUBSCRIBE" "GLASS"))))
      (when (and unsub (fboundp unsub) (ao-sink ao))
        (ignore-errors (funcall unsub (ao-sink ao)))))
    (ignore-errors (sdl (%pause-audio-device (ao-device ao) 1)))
    (ignore-errors (sdl (%close-audio-device (ao-device ao))))
    (setf (ao-device ao) 0))
  nil)
