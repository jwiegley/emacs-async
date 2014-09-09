# emacs-async

async.el is a module for doing asynchronous processing in Emacs.

# Install

Add to your .emacs.el:

     (when (require 'dired-aux)
       (require 'dired-async))

This will allow you to run  asynchronously
the dired commands for copying, renaming and symlinking.
If you are a [helm](https://github.com/emacs-helm/helm) user, this will allow you
to copy, rename etc... asynchronously from [helm](https://github.com/emacs-helm/helm).
Note that with [helm](https://github.com/emacs-helm/helm)
you can disable this by running the copy, rename etc... commands with a prefix argument.

If you don't want to make dired/helm asynchronous, you can either
disable this with `dired-async-be-async` or just load `async.el`.

# Usage

The interface is intended to be very easy to use:

## async-start

    async-start START-FUNC FINISH-FUNC
    
Execute START-FUNC (often a lambda) in a subordinate Emacs process.  When
done, the return value is passed to FINISH-FUNC.  Example:

    (async-start
       ;; What to do in the child process
       (lambda ()
         (message "This is a test")
         (sleep-for 3)
         222)

       ;; What to do when it finishes
       (lambda (result)
         (message "Async process done, result should be 222: %s" result)))
             
If FINISH-FUNC is nil or missing, a future is returned that can be inspected
using `async-get', blocking until the value is ready.  Example:

    (let ((proc (async-start
                   ;; What to do in the child process
                   (lambda ()
                     (message "This is a test")
                     (sleep-for 3)
                     222))))

        (message "I'm going to do some work here") ;; ....

        (message "Waiting on async process, result should be 222: %s"
                 (async-get proc)))

If you don't want to use a callback, and you don't care about any return value
form the child process, pass the `'ignore` symbol as the second argument (if
you don't, and never call `async-get`, it will leave *emacs* process buffers
hanging around):

    (async-start
     (lambda ()
       (delete-file "a remote file on a slow link" nil))
     'ignore)

Note: Even when FINISH-FUNC is present, a future is still returned except that
it yields no value (since the value is passed to FINISH-FUNC).  Call
`async-get' on such a future always returns nil.  It can still be useful,
however, as an argument to `async-ready' or `async-wait'.

## async-start-process

    async-start-process NAME PROGRAM FINISH-FUNC &rest PROGRAM-ARGS
    
Start the executable PROGRAM asynchronously.  See `async-start'.  PROGRAM is
passed PROGRAM-ARGS, calling FINISH-FUNC with the process object when done.
If FINISH-FUNC is nil, the future object will return the process object when
the program is finished.

## async-get

    async-get FUTURE
    
Get the value from an asynchronously function when it is ready.  FUTURE is
returned by `async-start' or `async-start-process' when its FINISH-FUNC is
nil.

## async-ready

    async-ready FUTURE

Query a FUTURE to see if the ready is ready -- i.e., if no blocking
would result from a call to `async-get' on that FUTURE.

## async-wait

    async-wait FUTURE

Wait for FUTURE to become ready.

## async-inject-variables

    async-inject-variables INCLUDE-REGEXP &optional PREDICATE EXCLUDE-REGEXP

Return a `setq' form that replicates part of the calling environment.  It sets
the value for every variable matching INCLUDE-REGEXP and also PREDICATE.  It
will not perform injection for any variable matching EXCLUDE-REGEXP (if
present).  It is intended to be used as follows:

    (async-start
       `(lambda ()
          (require 'smtpmail)
          (with-temp-buffer
            (insert ,(buffer-substring-no-properties (point-min) (point-max)))
            ;; Pass in the variable environment for smtpmail
            ,(async-inject-variables "\\`\\(smtpmail\\|\\(user-\\)?mail\\)-")
            (smtpmail-send-it)))
       'ignore)
