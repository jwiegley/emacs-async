# emacs-async

async.el is an exceedingly simple (78 lines of code) module for doing
asynchronous processing in Emacs, by spawning a child Emacs interpreter to
execute a lambda function, and calling back when the job is done.

It uses a very simple syntax:

    (async-start
       ;; What to do in the child process
       (lambda ()
         (message "This is a test")
         (sleep-for 3)
         222)

       ;; What to do when it finishes
       (lambda (result)
         (message "Async process done, result should be 222: %s" result)))

If you omit the callback function, `async-start` returns a process object that
you can call `async-get` on when you're ready to wait for the result value:

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
form the child process, pass the `ignore` symbol as the second argument (if
you don't, and never call `async-get', it will leave *emacs* process buffers
hanging around):

    (async-start
     (lambda ()
       (delete-file "a remote file on a slow link" nil))
     'ignore)
