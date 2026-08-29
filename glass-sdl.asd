;;;; glass-sdl.asd

(asdf:defsystem :glass-sdl
  :description "A glass desktop in a native window: glass/client's RFB framebuffer
put on screen with SDL2, and SDL2's keyboard and mouse sent back.  The bindings are
SBCL's own sb-alien rather than cl-sdl2, so there is nothing to install but libSDL2
itself -- no autowrap, no c2ffi, nothing parsed at build time.

This is the one place in the workspace that uses an FFI, and it is the right place:
glass is a framebuffer and an RFB server in Common Lisp, but putting its pixels on a
screen means asking somebody else's window server, and on a hosted OS that is always
going to be C.  On modus there is no window server to ask -- glass/fb IS the screen --
so none of this is needed there, which is the point of keeping it on this side of the
boundary."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  ;; sb-alien, sb-sys and sb-int only -- deliberately no sb-posix, which does not
  ;; exist on Windows and which nothing here needs.
    ;; glass/audio for the session mix a local viewer plays.  A viewer that could show the
  ;; desktop but not hear it was the state of things until now -- and on the welded desktop
  ;; (one process, its own window, no wire) there is no socket for sound to arrive on, so it
  ;; is pulled from the mixer directly.  Same capability as the WebRTC gateway's, without any
  ;; of its connectivity: there is nothing to connect to when the mixer is in this image.
  :depends-on ("glass/client" "glass/audio")
  :serial t
  :components ((:module "src" :serial t
                :components ((:file "packages")
                             (:file "sdl")
                             (:file "keys")
                             (:file "audio")
                             (:file "view")))))
