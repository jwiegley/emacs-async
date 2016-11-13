<p><a href="http://www.gnu.org/licenses/gpl-3.0.txt"><img src="https://img.shields.io/badge/license-GPL_3-green.svg" alt="License GPL 3" /></a>
<a href="http://melpa.org/#/async"><img src="http://melpa.org/packages/async-badge.svg" alt="MELPA" title="" /></a>
<a href="http://stable.melpa.org/#/async"><img src="http://stable.melpa.org/packages/async-badge.svg" alt="MELPA Stable" title="" /></a></p>


# emacs-async

`async.el` is a module for doing asynchronous processing in Emacs.
Some async applications are provided as well with this package:

* Dired-async
* smtp-mail-async
* async-bytecomp

# Install

## Install dired-async

Add to your `.emacs.el`:

    (autoload 'dired-async-mode "dired-async.el" nil t)
    (dired-async-mode 1)

This will allow you to run  asynchronously
the dired commands for copying, renaming and symlinking.
If you are a [helm](https://github.com/emacs-helm/helm) user, this will allow you
to copy, rename etc... asynchronously from [helm](https://github.com/emacs-helm/helm).
Note that with [helm](https://github.com/emacs-helm/helm)
you can disable this by running the copy, rename etc... commands with a prefix argument.

If you don't want to make dired/helm asynchronous disable it with `dired-async-mode`.

### Debian and Ubuntu

Users of Debian 9 or later or Ubuntu 16.04 or later may simply `apt-get install elpa-async`.

## Enable asynchronous compilation of your (M)elpa packages

By default emacs package.el compile packages in its running emacs session.
This is not a problem when installing a new package (which is not actually loaded in current emacs)
but it may create errors and bad compilation when upgrading a package (old version of package is already loaded
and running in current emacs).
You can remedy to this by allowing async to compile your packages asynchronously,
(helm and magit actually do this by default,
so if you are using these packages they will compile asynchronously)
to do this, add to your init file:
    
    (async-bytecomp-package-mode 1)


You can control which packages will compile async with `async-bytecomp-allowed-packages`.
Set it to `'(all)` to be sure you will compile all packages asynchronously.

# Async usage

The interface is intended to be very easy to use:

## async-start

    async-start START-FUNC FINISH-FUNC

Execute START-FUNC (often a lambda) in a subordinate Emacs process.  When
done, the return value is passed to FINISH-FUNC.  Example:

```elisp
(async-start
   ;; What to do in the child process
   (lambda ()
     (message "This is a test")
     (sleep-for 3)
     222)

   ;; What to do when it finishes
   (lambda (result)
     (message "Async process done, result should be 222: %s" result)))
```

If FINISH-FUNC is `nil` or missing, a future is returned that can be inspected
using `async-get`, blocking until the value is ready.  Example:

```elisp
(let ((proc (async-start
               ;; What to do in the child process
               (lambda ()
                 (message "This is a test")
                 (sleep-for 3)
                 222))))

    (message "I'm going to do some work here") ;; ....

    (message "Waiting on async process, result should be 222: %s"
             (async-get proc)))
```

If you don't want to use a callback, and you don't care about any return value
from the child process, pass the `'ignore` symbol as the second argument (if
you don't, and never call `async-get`, it will leave ``*emacs*`` process buffers
hanging around):

```elisp
(async-start
 (lambda ()
   (delete-file "a remote file on a slow link" nil))
 'ignore)
```

Note: Even when FINISH-FUNC is present, a future is still returned except that
it yields no value (since the value is passed to FINISH-FUNC).  Calling
`async-get` on such a future always returns `nil`.  It can still be useful,
however, as an argument to `async-ready` or `async-wait`.

## async-start-process

    async-start-process NAME PROGRAM FINISH-FUNC &rest PROGRAM-ARGS

Start the executable PROGRAM asynchronously.  See `async-start`.  PROGRAM is
passed PROGRAM-ARGS, calling FINISH-FUNC with the process object when done.
If FINISH-FUNC is `nil`, the future object will return the process object when
the program is finished.  Set DEFAULT-DIRECTORY to change PROGRAM's current
working directory.

## async-get

    async-get FUTURE

Get the value from an asynchronously called function when it is ready.  FUTURE is
returned by `async-start` or `async-start-process` when its FINISH-FUNC is
`nil`.

## async-ready

    async-ready FUTURE

Query a FUTURE to see if its function's value is ready -- i.e., if no blocking
would result from a call to `async-get` on that FUTURE.

## async-wait

    async-wait FUTURE

Wait for FUTURE to become ready.

## async-inject-variables

    async-inject-variables INCLUDE-REGEXP &optional PREDICATE EXCLUDE-REGEXP

Return a `setq` form that replicates part of the calling environment.  It sets
the value for every variable matching INCLUDE-REGEXP and also PREDICATE.  It
will not perform injection for any variable matching EXCLUDE-REGEXP (if
present).  It is intended to be used as follows:

```elisp
(async-start
   `(lambda ()
      (require 'smtpmail)
      (with-temp-buffer
        (insert ,(buffer-substring-no-properties (point-min) (point-max)))
        ;; Pass in the variable environment for smtpmail
        ,(async-inject-variables "\\`\\(smtpmail\\|\\(user-\\)?mail\\)-")
        (smtpmail-send-it)))
   'ignore)
```

## async-let

    async-let BINDINGS &rest FORMS
    
Allow to establish let bindings asynchronously.
Each value of binding can refer to the symbols already bound in BINDINGS (like `let*`).
FORMS are executed once BINDINGS have been evaluated, but without blocking emacs.

Examples:

```elisp
(async-let ((x "hello")
            (y "world"))
  (message "%s %s" x y))
  
(async-let ((x (* 5 2))
            (y (+ x 4))
            (z (+ x y)))
  (message "%d + %d = %d" x y z))

```

Note that if you bind something to nil and set it afterward in body, the evaluation
of this binding will NOT be asynchronous, but will happen in you current emacs, blocking it
if the evaluation of this value is sufficiently important, e.g:

```elisp
(async-let ((x "hello")
            (y "world")
            z)
  (setq z (+ 1 2)) ;; Huge calculation of Z will block emacs.
  (message "%s %s %d" x y z))

```

IOW if the calculation of Z is huge and you want it asynchronous evaluate it in BINDINGS
but not in FORMS.
