;;;; packages.lisp

(defpackage #:glass-sdl
  (:use #:cl)
  (:export #:view #:*sdl-candidates* #:load-sdl #:display-scale
           ;; sound out of this machine's speakers -- see src/audio.lisp
           #:start-audio #:stop-audio #:*audio-target-ms*
           ;; ...and the source half — see src/audio-in.lisp
           #:start-mic #:stop-mic #:*mic-rate*
           ;; mute either direction without giving the device back
           #:audio-out-muted-p #:audio-in-muted-p #:audio-in-open-p #:audio-in-showing-p
           #:*mic-idle-seconds* #:*mic-linger-seconds*
           #:mute-speakers #:mute-microphone #:speakers-muted-p #:microphone-muted-p
           #:refresh-title))
