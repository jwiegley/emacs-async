;; -*- lexical-binding: t -*-
(require 'subr-x)

(require 'buttercup)
(require 'async)

(describe "Async Core"

  (describe "Running emacs lisp in a subprocess"

    (it "should execute function in subprocess and pass result back to parent through a callback"

      (let ((messages nil))
        (push "Starting async-test-1..." messages)

        (async-start
         ;; What to do in the child process
         (lambda ()
           (message "This is a test")
           (sleep-for 0.5)
           222)

         ;; What to do when it finishes
         (lambda (result)
           (push (format "Async process done, result should be 222: %s" result) messages)))

        (push "Starting async-test-1...done" messages)
        (sleep-for 1)

        (expect (string-join (nreverse messages) "\n")
                :to-equal "Starting async-test-1...\nStarting async-test-1...done\nAsync process done, result should be 222: 222")))

    (it "should let the user do work while subprocess runs and then wait for the result"

      (let ((messages nil))
        (push "Starting async-test-2..." messages)
        (let ((proc (async-start
                     ;; What to do in the child process
                     (lambda ()
                       (message "This is a test")
                       (sleep-for 0.5)
                       222))))
          (push "I'm going to do some work here" messages)
          ;; ....
          (push (format "Async process done, result should be 222: %s" (async-get proc)) messages))

        (expect (string-join (nreverse messages) "\n")
                :to-equal "Starting async-test-2...\nI'm going to do some work here\nAsync process done, result should be 222: 222")))

    (xit "should allow both a callback and async-get for the same future"
      (let ((messages nil))
        (push "Starting async-test..." messages)
        (let ((proc (async-start
                     ;; What to do in the child process
                     (lambda ()
                       (sleep-for 0.5)
                       222)

                     (lambda (result)
                       (push (format "%s" result) messages)))))
          (push "I'm going to do some work here" messages)

          (push (format "async-get: %s" (async-get proc)) messages))

        (expect (string-join (nreverse messages) "\n")
                :to-equal "Starting async-test...\nI'm going to do some work here\n222\nasync-get: 222")))

    (it "should handle errors in the child process"

      (expect (progn
                (let ((messages nil))
                  (push "Starting async-test-3..." messages)
                  (async-start
                   ;; What to do in the child process
                   (lambda ()
                     (message "This is a test")
                     (sleep-for 0.5)
                     (error "Error in child process")
                     222)

                   ;; What to do when it finishes
                   (lambda (result)
                     (push (format "Async process done, result should be 222: %s" result) messages)))
                  (push "Starting async-test-3...done" messages)

                  (expect (string-join (nreverse messages) "\n")
                          :to-equal "Starting async-test-3...\nStarting async-test-3...done"))

                (sleep-for 1))
              :to-throw 'error))

    (it "should handle unreadable forms in the return value"

      (let ((messages nil))
        (let ((proc (async-start
                     ;; What to do in the child process
                     (lambda ()
                       (message "This is a test")
                       (sleep-for 0.1)
                       (current-buffer))

                     ;; What to do when it finishes
                     (lambda (result)
                       (push (format "Async process done, result should be 222: %s" result) messages)))))

          (async-get proc)

          (expect (string-join (nreverse messages) "\n")
                  :to-equal "Async process done, result should be 222: (buffer *scratch*)")))))

  (describe "Starting non-emacs process"

    (it "should start a process and return a process object"

      (let ((messages nil))
        (push "Starting async-test-4..." messages)
        (async-start-process "sleep" "sleep"
                             ;; What to do when it finishes
                             (lambda (proc)
                               (push (format "Sleep done, exit code was %d"
                                             (process-exit-status proc))
                                     messages))
                             "1")
        (push "Starting async-test-4...done" messages)

        (sleep-for 1.5)

        (expect (string-join (nreverse messages) "\n")
                :to-equal "Starting async-test-4...\nStarting async-test-4...done\nSleep done, exit code was 0"))))

  (describe "Interprocess communication"

    (it "should be possible to send and receive messages between parent and child"

      (let ((messages nil))
        (push "Starting async-test-5..." messages)
        (let ((proc
               (async-start
                ;; What to do in the child process
                (lambda ()
                  (message "This is a test, sending message")
                  (async-send :hello "world")
                  ;; wait for a message
                  (let ((msg (async-receive)))
                    (message "Child got message: %s"
                             (plist-get msg :goodbye)))
                  (sleep-for 0.5)
                  222)

                ;; What to do when it finishes
                (lambda (result)
                  (if (async-message-p result)
                      (push (format "Got hello from child process: %s" (plist-get result :hello))
                            messages)
                    (push (format "Async process done, result should be 222: %s" result)
                          messages))))))
          (async-send proc :goodbye "everyone"))
        (push "Starting async-test-5...done" messages)

        (sleep-for 1)

        (expect (string-join (nreverse messages) "\n")
                :to-equal "Starting async-test-5...\nStarting async-test-5...done\nGot hello from child process: world\nAsync process done, result should be 222: 222")))

    (it "child should be able to send really long messages to the parent (1 MB)"

      (let ((messages nil))
        (let ((proc (async-start
                     ;; What to do in the child process
                     (lambda ()
                       (async-send :hello (make-string 1048576 ?x))
                       t)

                     ;; What to do when it finishes
                     (lambda (result)
                       (if (async-message-p result)
                           (push (plist-get result :hello) messages)
                         (push result messages))))))

          ;;(sleep-for 0.5)
          (async-wait proc)
          )

        (expect (car messages) :to-equal t)
        (expect (cadr messages) :to-equal (make-string 1048576 ?x))))

    ;; windows process performance is horrible so we need to skip
    ;; this as it takes too much time.
    (unless (and (eq system-type 'windows-nt)
                 (version< emacs-version "27"))
      (it "child should be able to send really long messages to the parent (10 MB)"

        (let ((messages nil))
          (let ((proc (async-start
                       ;; What to do in the child process
                       (lambda ()
                         (async-send :hello (make-string 10485760 ?x))
                         t)

                       ;; What to do when it finishes
                       (lambda (result)
                         (if (async-message-p result)
                             (push (plist-get result :hello) messages)
                           (push result messages))))))
            (async-wait proc))

          (expect (car messages) :to-equal t)
          (expect (cadr messages) :to-equal (make-string 10485760 ?x))))))

  (describe "Handling process buffers"

    (it "should automatically close stdout and stderr buffer when process exits"

      (let ((messages nil))
        (async-start
         (lambda ()
           (message "This is a test")
           (sleep-for 0.5)
           222)

         (lambda (result)
           (push (format "Async process done, result should be 222: %s" result) messages)))

        (sleep-for 1)

        (expect (string-join (nreverse messages) "\n")
                :to-equal "Async process done, result should be 222: 222")
        (expect (cl-find-if (lambda (x) (string-match-p "emacs" x)) (mapcar #'buffer-name (buffer-list)))
                :to-be nil)
        (expect (cl-find-if (lambda (x) (string-match-p "emacs:err" x)) (mapcar #'buffer-name (buffer-list)))
                :to-be nil)))

    (it "should keep stdout and stderr buffer when process exits if debug is active"

      (unwind-protect
          (let ((messages nil)
                (async-debug t))
            (async-start
             (lambda ()
               (message "This is a test")
               (sleep-for 0.5)
               222)

             (lambda (result)
               (push (format "Async process done, result should be 222: %s" result) messages)))

            (sleep-for 1)

            (expect (string-join (nreverse messages) "\n")
                    :to-equal "Async process done, result should be 222: 222")
            (expect (cl-find-if (lambda (x) (string-match-p "emacs" x)) (mapcar #'buffer-name (buffer-list)))
                    :to-be-truthy)
            (expect (cl-find-if (lambda (x) (string-match-p "emacs:err" x)) (mapcar #'buffer-name (buffer-list)))
                    :to-be-truthy))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer "*emacs*")
          (kill-buffer "*emacs:err*")))))

  (describe "Injecting environment"

    (it "should construct a form for injecting the current environment"
      (with-temp-buffer
        (setq-local user-mail-address "hello@gnu.org")

        (let ((messages nil))
          (push "Starting async-test-6..." messages)
          (let ((proc (async-start
                       ;; What to do in the child process
                       `(lambda ()
                          ,(async-inject-variables "\\`user-mail-address\\'")
                          (format "user-mail-address = %s" user-mail-address))

                       ;; What to do when it finishes
                       (lambda (result)
                         (push (format "Async process done: %s" result) messages)))))
            (async-get proc))

          (expect (string-join (nreverse messages) "\n")
                  :to-equal "Starting async-test-6...\nAsync process done: user-mail-address = hello@gnu.org"))))))
