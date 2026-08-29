;;;; inspect/audio-probe.lisp — does the desktop's sound reach a device on this platform?
;;;;
;;;; The sibling of bindings-probe.lisp, and the same kind of question: not "can you hear
;;;; it" — nothing in a script can answer that — but every step that has to be true before
;;;; hearing it is possible.  Is there an audio device, does the mixer hand over frames, do
;;;; the bytes reach SDL's queue, and does that queue behave.
;;;;
;;;; THE QUEUE DEPTH IS THE INTERESTING NUMBER, and the reason this is a probe and not a
;;;; one-shot check.  Two clocks are involved — the mixer's 20 ms period and the device's
;;;; drain — and neither is the other's.  A depth that climbs means the pump is pushing
;;;; faster than the device plays, which does not glitch: it just puts the sound further and
;;;; further behind the picture, for as long as the session lasts.  A depth that reaches zero
;;;; is the audible failure, a click.  Steady is what correct looks like, so this samples it
;;;; over a second rather than asking once.
;;;;
;;;;   sbcl --script inspect/audio-probe.lisp

(require :asdf)
(unless (find-package :quicklisp)
  (let ((setup (find-if #'probe-file
                        (remove nil (list (let ((e (sb-ext:posix-getenv "QUICKLISP_SETUP")))
                                            (and e (pathname e)))
                                          #p"/opt/quicklisp/setup.lisp"
                                          (merge-pathnames "quicklisp/setup.lisp"
                                                           (user-homedir-pathname)))))))
    (unless setup
      (format *error-output* "~&audio-probe: no Quicklisp — glass comes from there.~%")
      (sb-ext:exit :code 1))
    (load setup)))
(let* ((here (or *load-truename* *default-pathname-defaults*))
       ;; NAME AND TYPE STRIPPED, then TRUENAME.  HERE is a FILE, and merging a directory
       ;; string onto a file keeps the file's name — so "../../" produces a path ending in
       ;; audio-probe.lisp two directories up, which does not exist.  Left as-is ASDF scans
       ;; a tree that is not there and reports the system missing, which is a confusing way
       ;; to be told about a pathname.
       (root (truename (make-pathname :name nil :type nil
                                      :defaults (merge-pathnames "../../" here)))))
  (asdf:initialize-source-registry
   `(:source-registry (:tree ,root) (:exclude "vendor") (:exclude "deps")
                      :inherit-configuration))
  (handler-bind ((warning #'muffle-warning))
    (let ((*standard-output* (make-broadcast-stream)))
      (asdf:load-asd (merge-pathnames "../glass-sdl.asd" here))
      (asdf:load-system :glass-sdl))))

(in-package :glass-sdl)

(defvar *fail* 0)
(defun ok (name got &optional detail)
  (if got (format t "  [pass] ~a~@[ — ~a~]~%" name detail)
      (progn (incf *fail*) (format t "  [FAIL] ~a~@[ — ~a~]~%" name detail)))
  (finish-output) got)

(load-sdl)
(ok "libSDL2 is here" *sdl-loaded* *sdl-loaded*)
(sdl (%init +init-audio+))

(let* ((mixer (glass:session-mixer))
       (ao (start-audio mixer)))
  (if (not (ok "an audio device opens" (not (null ao))
               "no device is a legitimate answer on a headless box; this probe then stops"))
      (sb-ext:exit :code (if (plusp *fail*) 1 0))
      (progn
        ;; A tone through the SESSION mix, which is the path a desktop's own sounds take.
        (glass:mixer-play mixer (glass:audio-tone 440 2.0) :name "probe")
        (let ((depths '()))
          (dotimes (i 8)
            (sleep 0.125)
            (push (sdl (%queued-audio-size (ao-device ao))) depths))
          (setf depths (nreverse depths))
          (format t "  queue over 1s: ~{~a~^ ~}~%" depths)
          (ok "frames reach the device's queue" (some #'plusp depths))
          (ok "...and it never runs dry" (notany #'zerop depths))
          ;; Growth is the silent failure: sound drifting behind the picture, never recovering.
          (ok "...and it does not grow without bound"
              (< (car (last depths)) (* 3 (max 1 (first depths))))
              (format nil "first ~a, last ~a" (first depths) (car (last depths)))))
        (stop-audio ao)
        (ok "it stops cleanly" t))))

(format t "~&=> ~:[FAIL~;PASS~]~%" (zerop *fail*))
(finish-output)
(sb-ext:exit :code (if (plusp *fail*) 1 0))
