;;;; packages.lisp

(defpackage #:glass-sdl
  (:use #:cl)
  (:export #:view #:*sdl-candidates* #:load-sdl #:display-scale
           ;; sound out of this machine's speakers -- see src/audio.lisp
           #:start-audio #:stop-audio #:*audio-target-ms*))
