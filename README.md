# glass-sdl

**A [glass](https://github.com/modus-lisp/glass) desktop in a native window.**

Not a browser, not a VNC client — a window on your machine showing a glass
desktop, wherever that desktop is running.

```lisp
(ql:quickload :glass-sdl)
(glass-sdl:view :host "127.0.0.1" :port 5901)
```

Or, against a containerised desktop: `kiln view`.

## What it is

Almost nothing, which is the point. `glass/client` already speaks RFB, keeps a
local framebuffer of what the remote is showing, and takes keys and pointer
events back. This adds the two things a hosted operating system will not give you
for free — a window to put the pixels in, and a source of input events.

The pixels go up as one streaming texture. glass/client reports damage as a flag
rather than a rectangle list, so there is nothing finer to act on; an idle desktop
uploads nothing, and a busy one uploads four megabytes that a GPU does not notice.

## The FFI

This is the one place in the modus-lisp workspace that binds a C library, and
that is deliberate. glass is a framebuffer and an RFB server in pure Common Lisp,
but *putting its pixels on a screen* means asking somebody else's window server,
and on a hosted OS that is always going to be C. The boundary is one file —
[`src/sdl.lisp`](src/sdl.lisp) — and nothing behind it changes.

On [modus](https://github.com/modus-lisp/modus) there is no window server to ask:
`glass/fb` **is** the screen. So none of this is needed there, which is the
reason for keeping it on this side of the line rather than inside glass.

The bindings are SBCL's own `sb-alien`, not `cl-sdl2`. cl-sdl2 is a good library
that uses `cl-autowrap`, which needs `c2ffi` — a clang-based header parser you
build from a tap. That is a lot of machinery to place a rectangle of pixels on a
screen. Fifteen `define-alien-routine` forms need nothing installed but libSDL2
itself, and parse nothing at build time.

Two things the binding has to get right, both discovered the hard way:

- **Floating-point traps.** SDL, and anything under it that touches a GPU, trips
  the FP traps SBCL enables by default. Every call goes through a
  `with-float-traps-masked`, or you get `FLOATING-POINT-INVALID-OPERATION` at
  window creation.
- **The main thread.** macOS insists the event loop is the main thread, so `view`
  is a function you *call* from yours rather than a server you start.

## Requirements

SBCL, and libSDL2 at runtime:

```sh
brew install sdl2          # macOS
apt install libsdl2-2.0-0  # Debian/Ubuntu
```

## Input

RFB speaks X11 keysyms, and glass hands the server's keysym straight through to
the remote, so `src/keys.lisp` is the only translation in the whole path — and
most of it is the identity, since SDL's keycode for a printable character is its
ASCII code and so is X11's keysym. What is left is the keys with no character.

Mouse buttons become the RFB button mask; the wheel becomes buttons 4 and 5,
pressed and released, because RFB has no wheel.

MIT. Research / educational; **not audited**.
