;;;; bin/build-exe.lisp — save glass-sdl as a standalone executable.
;;;;
;;;;   sbcl --dynamic-space-size 2048 --script bin/build-exe.lisp [output]
;;;;
;;;; Produces a single binary with the whole viewer inside it.  The only thing
;;;; that has to sit beside it is libSDL2 (SDL2.dll on Windows), because that is
;;;; loaded at RUN time, not baked in: glass-sdl does not touch the library until
;;;; VIEW is called, so the dump holds no open handle to relocate.
;;;;
;;;; Must be built on the platform it will run on -- an SBCL core is machine code
;;;; and a heap image, not bytecode.

(require :asdf)
(asdf:initialize-source-registry
 (let ((here (make-pathname :name nil :type nil :defaults *load-truename*)))
   `(:source-registry (:tree ,(merge-pathnames "../../" here))
                      (:exclude "vendor" "deps") :inherit-configuration)))

(handler-bind ((warning #'muffle-warning))
  (let ((*standard-output* (make-broadcast-stream)))
    (asdf:load-system :glass-sdl)))

(defun usage ()
  (format t "~&glass-view — a glass desktop in a window~%~%~
             usage:  glass-view [host] [port]~%~
             default: 127.0.0.1 5901~%~%~
             Needs libSDL2 beside it (SDL2.dll on Windows).~%"))

(defun main ()
  (let* ((args (rest sb-ext:*posix-argv*))
         (first-arg (first args)))
    (when (member first-arg '("-h" "--help" "/?") :test #'equal)
      (usage) (sb-ext:quit :unix-status 0))
    (let ((host (or first-arg "127.0.0.1"))
          (port (or (ignore-errors (parse-integer (or (second args) "5901"))) 5901)))
      (format t "~&connecting to ~a:~d …~%" host port)
      (finish-output)
      (handler-case (glass-sdl:view :host host :port port)
        (error (e)
          ;; A window that vanishes with no message is the worst outcome for a
          ;; double-clicked binary, so say what happened and wait to be read.
          (format *error-output* "~&~%glass-view: ~a~%~%" e)
          (finish-output *error-output*)
          (when (find :win32 *features*)
            (format *error-output* "Press Enter to close.~%")
            (finish-output *error-output*)
            (ignore-errors (read-line)))
          (sb-ext:quit :unix-status 1))))
    (sb-ext:quit :unix-status 0)))

(let* ((given (second sb-ext:*posix-argv*))
       (out (or given (if (find :win32 *features*) "glass-view.exe" "glass-view"))))
  (format t "~&saving ~a~%" out)
  (finish-output)
  ;; :save-runtime-options keeps the binary from reading --dynamic-space-size and
  ;; friends out of the user's arguments -- those are OUR arguments now.
  (sb-ext:save-lisp-and-die out :executable t :toplevel #'main
                                :save-runtime-options t))
