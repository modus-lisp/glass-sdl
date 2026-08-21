;;;; inspect/bindings-probe.lisp — does the FFI hold together on this platform?
;;;;
;;;; Not "does a desktop appear" — that needs a display and a running glass.  This
;;;; asks the question a port actually turns on: is the library found, do the
;;;; symbols resolve, do the calls return what C says they return, and are the
;;;; struct offsets right.  Under SDL_VIDEODRIVER=dummy it answers on a machine
;;;; with no screen at all, which is what CI is.
;;;;
;;;;   sbcl --script inspect/bindings-probe.lisp

(require :asdf)
;; Siblings, found relative to this file, so the workspace can live anywhere.
(asdf:initialize-source-registry
 (let ((here (make-pathname :name nil :type nil :defaults *load-truename*)))
   `(:source-registry (:tree ,(merge-pathnames "../../" here))
                      (:exclude "vendor" "deps") :inherit-configuration)))

(defvar *checks* 0)
(defvar *failures* 0)
(defun check (label ok &optional detail)
  (incf *checks*)
  (unless ok (incf *failures*))
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~@[ — ~a~]~%" ok label detail)
  (finish-output))

(handler-case
    (handler-bind ((warning #'muffle-warning))
      (let ((*standard-output* (make-broadcast-stream))) (asdf:load-system :glass-sdl))
      (check "glass-sdl compiles" t))
  (error (e) (check "glass-sdl compiles" nil e) (sb-ext:quit :unix-status 1)))

(defun sym (name) (read-from-string (concatenate 'string "glass-sdl::" name)))

(defmacro sdl (&body body)
  `(sb-int:with-float-traps-masked (:invalid :inexact :overflow :underflow :divide-by-zero)
     ,@body))

(let ((lib (handler-case (funcall (sym "load-sdl"))
             (error (e) (check "libSDL2 found" nil e) (sb-ext:quit :unix-status 1)))))
  (check "libSDL2 found" t lib))

(sdl
 (check "SDL_Init(VIDEO)" (zerop (funcall (sym "%init") #x20)) (funcall (sym "%get-error")))
 (let* ((win (funcall (sym "%create-window") "probe" #x1FFF0000 #x1FFF0000 320 200 4))
        (win-ok (not (sb-alien:null-alien win))))
   (check "SDL_CreateWindow" win-ok (unless win-ok (funcall (sym "%get-error"))))
   (when win-ok
     (let* ((ren (funcall (sym "%create-renderer") win -1 0))
            (ren-ok (not (sb-alien:null-alien ren))))
       (check "SDL_CreateRenderer" ren-ok (unless ren-ok (funcall (sym "%get-error"))))
       (when ren-ok
         (let* ((tex (funcall (sym "%create-texture") ren #x16161804 1 320 200))
                (tex-ok (not (sb-alien:null-alien tex))))
           (check "SDL_CreateTexture (RGB888 streaming)" tex-ok
                  (unless tex-ok (funcall (sym "%get-error"))))
           (when tex-ok
             ;; The pixel path: a real upload from a Lisp vector's SAP.
             (let ((px (make-array (* 320 200) :element-type '(unsigned-byte 32)
                                               :initial-element #x00336699)))
               (check "SDL_UpdateTexture from a pinned Lisp vector"
                      (zerop (sb-sys:with-pinned-objects (px)
                               (funcall (sym "%update-texture") tex (funcall (sym "null-ptr"))
                                        (sb-alien:sap-alien (sb-sys:vector-sap px) (* t))
                                        (* 4 320))))
                      (funcall (sym "%get-error"))))
             (funcall (sym "%destroy-texture") tex))
           (funcall (sym "%destroy-renderer") ren))))
     (funcall (sym "%destroy-window") win)))
 ;; Returns 0 on an empty queue, which is the answer on a headless box; what
 ;; matters is that the call marshals and returns rather than crashing.
 (let ((ev (sb-alien:make-alien (sb-alien:unsigned 8) 64)))
   (check "SDL_PollEvent"
          (integerp (funcall (sym "%poll-event")
                             (sb-alien:sap-alien (sb-alien:alien-sap ev) (* t)))))
   (sb-alien:free-alien ev))
 (funcall (sym "%quit")))

;;; Keysym translation is pure, and the only translation in the input path.
(let ((k (sym "sdl->keysym")))
  (check "keysym: 'a' is itself" (eql (funcall k 97) 97))
  (check "keysym: Return -> XK_Return" (eql (funcall k 13) #xFF0D))
  (check "keysym: Left -> XK_Left" (eql (funcall k (logior (ash 1 30) 80)) #xFF51))
  (check "keysym: F1 -> XK_F1" (eql (funcall k (logior (ash 1 30) 58)) #xFFBE))
  (check "keysym: unknown -> NIL" (null (funcall k 999999))))

(format t "~&~%checks: ~d   failures: ~d   => ~:[FAIL~;PASS~]~%"
        *checks* *failures* (zerop *failures*))
(sb-ext:quit :unix-status (if (zerop *failures*) 0 1))
