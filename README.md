# emacs-async

Adds the ability to process Lisp concurrently, with a very simple syntax:

    (async-start
       ;; What to do in the child process
       (lambda ()
         (message "This is a test")
         (sleep-for 3)
         222)

       ;; What to do when it finishes
       (lambda (result)
         (message "Async process done, result should be 222: %s" result)))

If you omit the callback function, `async-start` will return a process object
that you can `async-get` on when you're ready to wait for the result value:

    (let ((proc (async-start
                   ;; What to do in the child process
                   (lambda ()
                     (message "This is a test")
                     (sleep-for 3)
                     222))))
        (message "I'm going to do some work here")
        ;; ....
        (message "Async process done, result should be 222: %s"
                 (async-get proc)))
