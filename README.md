<p><a href="http://www.gnu.org/licenses/gpl-3.0.txt"><img src="https://img.shields.io/badge/license-GPL_3-green.svg" alt="License GPL 3" /></a>
<a href="https://elpa.gnu.org/packages/async.html"><img src="https://elpa.gnu.org/packages/async.svg" alt="GNU ELPA" title="" /></a>
<a href="http://melpa.org/#/async"><img src="http://melpa.org/packages/async-badge.svg" alt="MELPA" title="" /></a>
<a href="http://stable.melpa.org/#/async"><img src="http://stable.melpa.org/packages/async-badge.svg" alt="MELPA Stable" title="" /></a></p>

# emacs-async

I've been using Emacs long enough to know that its single-threaded nature is
both a strength and a weakness. It keeps things simple -- until you need to
copy a hundred files over TRAMP, byte-compile a stack of packages, or send an
email through a slow SMTP server. That's when your entire editor locks up, and
you're stuck watching the modeline blink.

`async.el` fixes this by letting you run Emacs Lisp code in a child Emacs
process. The child starts with `-Q` (no init files, no fuss), executes
whatever you give it, and passes the result back. Your main Emacs never
blocks.

## What's in the box

The core library is `async.el`, but the package ships with several
ready-to-use applications built on top of it:

- **dired-async** -- Async file operations (copy, rename, symlink) in Dired
- **async-bytecomp** -- Byte-compiles packages in a clean subprocess, avoiding
  the stale-bytecode bugs you get when compiling in a running session
- **async-package** -- Downloads and installs packages asynchronously
- **smtpmail-async** -- Sends email in the background

## Installation

The package is available on [GNU ELPA](https://elpa.gnu.org/packages/async.html)
and [MELPA](https://melpa.org/#/async):

```elisp
(package-install 'async)
```

Debian/Ubuntu users: `apt-get install elpa-async`

From source:

```bash
make all && make install
```

## Usage

### The basics

Run a function in a subprocess and get the result via callback:

```elisp
(async-start
 (lambda ()
   (sleep-for 3)
   42)
 (lambda (result)
   (message "Got %s" result)))
```

If you'd rather wait for the result yourself, skip the callback and use
`async-get`:

```elisp
(let ((future (async-start
               (lambda ()
                 (sleep-for 3)
                 42))))
  ;; do other work...
  (message "Result: %s" (async-get future)))
```

If you don't care about the return value, pass `'ignore` as the callback.
Don't just leave it nil -- you'll leak process buffers:

```elisp
(async-start
 (lambda ()
   (delete-file "/ssh:remote:big-file.tar.gz"))
 'ignore)
```

### Passing variables to the child

The child Emacs starts fresh, so it doesn't have your variables. Use
`async-inject-variables` to send them over:

```elisp
(async-start
 `(lambda ()
    ,(async-inject-variables "\\`user-mail-address\\'")
    (format "Mail: %s" user-mail-address))
 (lambda (result) (message "%s" result)))
```

### Bidirectional messaging

The parent and child can exchange messages while the child runs:

```elisp
(let ((proc
       (async-start
        (lambda ()
          (async-send :status "working")
          (sleep-for 2)
          (let ((msg (async-receive)))
            (plist-get msg :data)))
        (lambda (result)
          (if (async-message-p result)
              (message "Status: %s" (plist-get result :status))
            (message "Done: %s" result))))))
  (async-send proc :data "here you go"))
```

### Async let bindings

`async-let` evaluates bindings asynchronously and runs the body when they're
ready:

```elisp
(async-let ((x (heavy-computation-1))
            (y (heavy-computation-2)))
  (message "x=%s y=%s" x y))
```

## Dired async mode

```elisp
(dired-async-mode 1)
```

That's it. Copy, rename, and symlink operations in Dired now run in the
background. If you use [Helm](https://github.com/emacs-helm/helm), this works
there too.

## Async byte compilation

```elisp
(async-bytecomp-package-mode 1)
(setq async-bytecomp-allowed-packages '(all))
```

This compiles packages in a clean Emacs subprocess, which avoids the nasty
bugs you get when the old version of a package is still loaded during
compilation.

## Async email

```elisp
(setq message-send-mail-function 'async-smtpmail-send-it)
```

A word of caution: send at least one email synchronously first, so any TLS
negotiation or auth prompts happen interactively. After that, async sending
works fine.

## Authentication

For anything involving remote hosts (TRAMP, SMTP), you'll want `auth-sources`
configured so the child process can authenticate without prompting you:

```elisp
(setq auth-sources '("~/.authinfo.gpg" "~/.netrc"))
```

On Emacs 26+, set `async-quiet-switch` to `"-q"` when using TRAMP -- there's
a bug where `-Q` prevents auth-source from working properly.

## Development

If you have Nix installed:

```bash
nix develop          # Enter development shell with all dependencies
nix flake check      # Run all checks (compile, lint, format, tests)
```

Otherwise:

```bash
make all             # Clean, generate autoloads, compile
eask install-deps --dev
eask test buttercup  # Run tests
make lint            # Byte-compile with warnings-as-errors + package-lint
make format-check    # Check code formatting
make format          # Fix code formatting
```
