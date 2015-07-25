;;; async --- Asynchronous processing in Emacs

;; Copyright (C) 2012~2014 John Wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Created: 18 Jun 2012

;; Keywords: async
;; X-URL: https://github.com/jwiegley/emacs-async

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

;; Adds the ability to call asynchronous functions and process with ease.  See
;; the documentation for `async-start' and `async-start-process'.

;;; Code:

(defgroup async nil
  "Simple asynchronous processing in Emacs"
  :group 'emacs)

(defvar async-debug nil)
(defvar async-send-over-pipe t)
(defvar async-in-child-emacs nil)
(defvar async-callback nil)
(defvar async-callback-for-process nil)
(defvar async-callback-value nil)
(defvar async-callback-value-set nil)
(defvar async-current-process nil)

(defun async-inject-variables
  (include-regexp &optional predicate exclude-regexp)
  "Return a `setq' form that replicates part of the calling environment.
It sets the value for every variable matching INCLUDE-REGEXP and
also PREDICATE.  It will not perform injection for any variable
matching EXCLUDE-REGEXP (if present).  It is intended to be used
as follows:

    (async-start
       `(lambda ()
          (require 'smtpmail)
          (with-temp-buffer
            (insert ,(buffer-substring-no-properties (point-min) (point-max)))
            ;; Pass in the variable environment for smtpmail
            ,(async-inject-variables \"\\`\\(smtpmail\\|\\(user-\\)?mail\\)-\")
            (smtpmail-send-it)))
       'ignore)"
  `(setq
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
                 (when (or (null predicate)
                           (funcall predicate sym))
                   (setq bindings (cons `(quote ,value) bindings)
                         bindings (cons sym bindings)))))))
        bindings)))

(defalias 'async-inject-environment 'async-inject-variables)

(defun async-handle-result (func result buf)
  (if (null func)
      (progn
        (set (make-local-variable 'async-callback-value) result)
        (set (make-local-variable 'async-callback-value-set) t))
    (unwind-protect
        (if (and (listp result)
                 (eq 'async-signal (nth 0 result)))
            (signal (car (nth 1 result))
                    (cdr (nth 1 result)))
          (funcall func result))
      (unless async-debug
        (kill-buffer buf)))))

(defun async-when-done (proc &optional change)
  "Process sentinal used to retrieve the value from the child process."
  (when (eq 'exit (process-status proc))
    (with-current-buffer (process-buffer proc)
      (let ((async-current-process proc))
        (if (= 0 (process-exit-status proc))
            (if async-callback-for-process
                (if async-callback
                    (prog1
                        (funcall async-callback proc)
                      (unless async-debug
                        (kill-buffer (current-buffer))))
                  (set (make-local-variable 'async-callback-value) proc)
                  (set (make-local-variable 'async-callback-value-set) t))
              (goto-char (point-max))
              (backward-sexp)
              (async-handle-result async-callback (read (current-buffer))
                                   (current-buffer)))
          (set (make-local-variable 'async-callback-value)
               (list 'error
                     (format "Async process '%s' failed with exit code %d"
                             (process-name proc) (process-exit-status proc))))
          (set (make-local-variable 'async-callback-value-set) t))))))

(defun async--receive-sexp (&optional stream)
  (let ((sexp (decode-coding-string (base64-decode-string
                                     (read stream)) 'utf-8-unix)))
    (if async-debug
        (message "Received sexp {{{%s}}}" (pp-to-string sexp)))
    (setq sexp (read sexp))
    (if async-debug
        (message "Read sexp {{{%s}}}" (pp-to-string sexp)))
    (eval sexp)))

(defun async--insert-sexp (sexp)
  (let (print-level print-length)
    (prin1 sexp (current-buffer))
    ;; Just in case the string we're sending might contain EOF
    (encode-coding-region (point-min) (point-max) 'utf-8-unix)
    (base64-encode-region (point-min) (point-max) t)
    (goto-char (point-min)) (insert ?\")
    (goto-char (point-max)) (insert ?\" ?\n)))

(defun async--transmit-sexp (process sexp)
  (with-temp-buffer
    (if async-debug
        (message "Transmitting sexp {{{%s}}}" (pp-to-string sexp)))
    (async--insert-sexp sexp)
    (process-send-region process (point-min) (point-max))))

(defun async-batch-invoke ()
  "Called from the child Emacs process' command-line."
  (setq async-in-child-emacs t
        debug-on-error async-debug)
  (if debug-on-error
      (prin1 (funcall
              (async--receive-sexp (unless async-send-over-pipe
                                     command-line-args-left))))
    (condition-case err
        (prin1 (funcall
                (async--receive-sexp (unless async-send-over-pipe
                                       command-line-args-left))))
      (error
       (prin1 (list 'async-signal err))))))

(defun async-ready (future)
  "Query a FUTURE to see if the ready is ready -- i.e., if no blocking
would result from a call to `async-get' on that FUTURE."
  (and (memq (process-status future) '(exit signal))
       (with-current-buffer (process-buffer future)
         async-callback-value-set)))

(defun async-wait (future)
  "Wait for FUTURE to become ready."
  (while (not (async-ready future))
    (sit-for 0.05)))

(defun async-get (future)
  "Get the value from an asynchronously function when it is ready.
FUTURE is returned by `async-start' or `async-start-process' when
its FINISH-FUNC is nil."
  (async-wait future)
  (with-current-buffer (process-buffer future)
    (async-handle-result #'identity async-callback-value (current-buffer))))

(defun async-message-p (value)
  "Return true of VALUE is an async.el message packet."
  (and (listp value)
       (plist-get value :async-message)))

(defun async-send (&rest args)
  "Send the given messages to the asychronous Emacs PROCESS."
  (let ((args (append args '(:async-message t))))
    (if async-in-child-emacs
        (if async-callback
            (funcall async-callback args))
      (async--transmit-sexp (car args) (list 'quote (cdr args))))))

(defun async-receive (&rest args)
  "Send the given messages to the asychronous Emacs PROCESS."
  (async--receive-sexp))

;;;###autoload
(defun async-start-process (name program finish-func &rest program-args)
  "Start the executable PROGRAM asynchronously.  See `async-start'.
PROGRAM is passed PROGRAM-ARGS, calling FINISH-FUNC with the
process object when done.  If FINISH-FUNC is nil, the future
object will return the process object when the program is
finished.  Set DEFAULT-DIRECTORY to change PROGRAM's current
working directory."
  (let* ((buf (generate-new-buffer (concat "*" name "*")))
         (proc (let ((process-connection-type nil))
                 (apply #'start-process name buf program program-args))))
    (with-current-buffer buf
      (set (make-local-variable 'async-callback) finish-func)
      (set-process-sentinel proc #'async-when-done)
      (unless (string= name "emacs")
        (set (make-local-variable 'async-callback-for-process) t))
      proc)))

;;;###autoload
(defmacro async-start (start-func &optional finish-func)
  "Execute START-FUNC (often a lambda) in a subordinate Emacs process.
When done, the return value is passed to FINISH-FUNC.  Example:

    (async-start
       ;; What to do in the child process
       (lambda ()
         (message \"This is a test\")
         (sleep-for 3)
         222)

       ;; What to do when it finishes
       (lambda (result)
         (message \"Async process done, result should be 222: %s\"
                  result)))

If FINISH-FUNC is nil or missing, a future is returned that can
be inspected using `async-get', blocking until the value is
ready.  Example:

    (let ((proc (async-start
                   ;; What to do in the child process
                   (lambda ()
                     (message \"This is a test\")
                     (sleep-for 3)
                     222))))

        (message \"I'm going to do some work here\") ;; ....

        (message \"Waiting on async process, result should be 222: %s\"
                 (async-get proc)))

If you don't want to use a callback, and you don't care about any
return value form the child process, pass the `ignore' symbol as
the second argument (if you don't, and never call `async-get', it
will leave *emacs* process buffers hanging around):

    (async-start
     (lambda ()
       (delete-file \"a remote file on a slow link\" nil))
     'ignore)

Note: Even when FINISH-FUNC is present, a future is still
returned except that it yields no value (since the value is
passed to FINISH-FUNC).  Call `async-get' on such a future always
returns nil.  It can still be useful, however, as an argument to
`async-ready' or `async-wait'."
  (require 'find-func)
  (let ((procvar (make-symbol "proc")))
    `(let* ((sexp ,start-func)
            (,procvar
             (async-start-process
              "emacs" (file-truename
                       (expand-file-name invocation-name
                                         invocation-directory))
              ,finish-func
              "-Q" "-l"
              ;; Using `locate-library' ensure we use the right file
              ;; when the .elc have been deleted.
              ,(locate-library "async")
              "-batch" "-f" "async-batch-invoke"
              (if async-send-over-pipe
                  "<none>"
                (with-temp-buffer
                  (async--insert-sexp (list 'quote sexp))
                  (buffer-string))))))
       (if async-send-over-pipe
           (async--transmit-sexp ,procvar (list 'quote sexp)))
       ,procvar)))

(defmacro async-sandbox(func)
  "Evaluate FUNC in a separate Emacs process, synchronously."
  `(async-get (async-start ,func)))

(provide 'async)

;;; async.el ends here
