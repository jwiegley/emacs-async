# emacs-async

Adds the ability to process Lisp concurrently, with a very simple syntax:

    (emacs-async-fork
         ;; What to do in the child process
         (lambda ()
           (message "This is a test")
           (sleep-for 3)
           async-sample-variable)

         ;; What to do when it finishes
         (lambda (result)
           (message "Async process done, result should be 222: %d" result)))
