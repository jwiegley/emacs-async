;;; async --- Asynchronous processing in Emacs

;; Copyright (C) 2012 John Wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Created: 18 Jun 2012
;; Version: 1.0
;; Keywords: async
;; X-URL: https://github.com/jwiegley/async

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Adds the ability to process Lisp concurrently, with a very simple syntax:
;;
;;   (async-start
;;      ;; What to do in the child process
;;      (lambda ()
;;        (message "This is a test")
;;        (sleep-for 3)
;;        222)
;;
;;      ;; What to do when it finishes
;;      (lambda (result)
;;        (message "Async process done, result should be 222: %s" result)))
;;
;; If you omit the callback function, `async-start' will return a process
;; object that you can call `async-get' on when you're ready to wait for the
;; result value:
;;
;;   (let ((proc (async-start
;;                  ;; What to do in the child process
;;                  (lambda ()
;;                    (message "This is a test")
;;                    (sleep-for 3)
;;                    222))))
;;       (message "I'm going to do some work here")
;;       ;; ....
;;       (message "Async process done, result should be 222: %s"
;;                (async-get proc)))
;;
;; If you don't want to use a callback, and you don't care about any return
;; value form the child proces, pass the `ignore' symbol as the second
;; argument:
;;
;;   (async-start
;;    (lambda ()
;;      (delete-file "a remote file on a slow link" nil))
;;      'ignore)

;;; Code:

(defgroup async nil
  "Simple asynchronous processing in Emacs"
  :group 'emacs)

(defvar async-callback nil)
(defvar async-callback-value nil)
(defvar async-callback-value-set nil)

(defmacro async-inject-environment
  (include-regexp &optional predicate exclude-regexp)
  "Inject a part of the parent environment into an async function."
  `'(setq
     ,@(let (bindings)
         (mapatoms
          (lambda (sym)
            (if (and (boundp sym)
                     (or (null include-regexp)
                         (string-match include-regexp (symbol-name sym)))
                     (not (string-match
                           (or exclude-regexp "-syntax-table\\'")
                           (symbol-name sym))))
                (let ((value (symbol-value sym)))
                  (when (funcall (or predicate
                                     (lambda (sym)
                                       (let ((value (symbol-value sym)))
                                         (or (not (functionp value))
                                             (symbolp value)))))
                                 sym)
                    (setq bindings (cons `(quote ,value) bindings)
                          bindings (cons sym bindings)))))))
         bindings)))

(defun async-when-done (proc &optional change)
  "Process sentinal used to retrieve the value from the child process."
  (when (eq 'exit (process-status proc))
    (with-current-buffer (process-buffer proc)
      (if (= 0 (process-exit-status proc))
          (progn
            (goto-char (point-max))
            (backward-sexp)
            (let ((result (read (current-buffer))))
              (if (and (listp result)
                       (eq 'async-signal (car result)))
                  (if (eq 'error (car (cdr result)))
                      (error (cadr (cdr result)))
                    (signal (cadr result)
                            (cddr result)))
                (if async-callback
                    (prog1
                        (funcall async-callback result)
                      (kill-buffer (current-buffer)))
                  (set (make-local-variable 'async-callback-value) result)
                  (set (make-local-variable 'async-callback-value-set) t)))))
        (set (make-local-variable 'async-callback-value) 'error)
        (set (make-local-variable 'async-callback-value-set) t)
        (error "Async Emacs process failed with exit code %d"
               (process-exit-status proc))))))

(defun async-batch-invoke ()
  "Called from the child Emacs process' command-line."
  (condition-case err
      (prin1 (funcall (eval (read (nth 5 command-line-args)))))
    (signal
     (prin1 `(async-signal . ,err)))
    (error
     (prin1 `(async-signal . ,err)))))

(defun async-get (proc)
  "Wait until PROC has successfully completed."
  (with-current-buffer (process-buffer proc)
    (while (and (not (eq 'exit (process-status proc)))
                (not async-callback-value-set))
      (sit-for 0 50))
    (prog1
        async-callback-value
      (kill-buffer (current-buffer)))))

(defmacro async-start (start-func &optional finish-func)
  "Fork execution of `start-func' into its own Emacs process.
`start-func' must be a `read'-able symbol or lambda form.  It
cannot be a byte-compiled lambda.

`finish-func' is called with the result of `start-func' when that
process has completed.  If it is nil, `async-start' will return a
process object that you can block on with `async-future-get' in
order to wait for the result of `start-func'.  This would allow
you to start some expensive background processing at the
beginning of a command, then wait for the result only when you're
ready to use it."
  (let ((bufvar (make-symbol "buf"))
        (procvar (make-symbol "proc")))
    (require 'find-func)
    `(let* ((,bufvar (generate-new-buffer "*emacs*"))
            (,procvar
             (start-process "emacs" ,bufvar
                            (expand-file-name invocation-name
                                              invocation-directory)
                            "-Q" "-l" (find-library-name "async")
                            "-batch" "-f" "async-batch-invoke"
                            (prin1-to-string (list 'quote ,start-func)))))
       (with-current-buffer ,bufvar
         (set (make-local-variable 'async-callback) ,finish-func)
         (set-process-sentinel ,procvar #'async-when-done)
         ,procvar))))

(defun async-test-1 ()
  (interactive)
  (message "Starting async-test-1...")
  (async-start
   ;; What to do in the child process
   (lambda ()
     (message "This is a test")
     (sleep-for 3)
     222)

   ;; What to do when it finishes
   (lambda (result)
     (message "Async process done, result should be 222: %s" result)))
  (message "Starting async-test-1...done"))

(defun async-test-2 ()
  (interactive)
  (message "Starting async-test-2...")
  (let ((proc (async-start
               ;; What to do in the child process
               (lambda ()
                 (message "This is a test")
                 (sleep-for 3)
                 222))))
    (message "I'm going to do some work here")
    ;; ....
    (message "Async process done, result should be 222: %s"
             (async-get proc))))

(defun async-test-3 ()
  (interactive)
  (message "Starting async-test-3...")
  (async-start
   ;; What to do in the child process
   (lambda ()
     (message "This is a test")
     (sleep-for 3)
     (error "Error in child process")
     222)

   ;; What to do when it finishes
   (lambda (result)
     (message "Async process done, result should be 222: %s" result)))
  (message "Starting async-test-1...done"))

(provide 'async)

;;; async.el ends here
