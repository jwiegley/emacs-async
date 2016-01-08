;;; async-test.el --- async.el-related tests

;; Copyright (C) 2012-2016 Free Software Foundation, Inc.

;; Author: John Wiegley <jwiegley@gmail.com>
;; Created: 10 Jul 2012
;; Version: 1.0
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

;; Contains tests for all the async modules.

;;; Code:

(require 'async)


(eval-when-compile
  (require 'cl))

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

(defun async-test-4 ()
  (interactive)
  (message "Starting async-test-4...")
  (async-start-process "sleep" "sleep"
                       ;; What to do when it finishes
                       (lambda (proc)
                         (message "Sleep done, exit code was %d"
                                  (process-exit-status proc)))
                       "3")
  (message "Starting async-test-4...done"))

(defun async-test-5 ()
  (interactive)
  (message "Starting async-test-5...")
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
            (sleep-for 3)
            222)

          ;; What to do when it finishes
          (lambda (result)
            (if (async-message-p result)
                (message "Got hello from child process: %s"
                         (plist-get result :hello))
              (message "Async process done, result should be 222: %s"
                       result))))))
    (async-send proc :goodbye "everyone"))
  (message "Starting async-test-5...done"))

(defun async-test-6 ()
  (interactive)
  (message "Starting async-test-6...")
  (async-start
   ;; What to do in the child process
   `(lambda ()
      ,(async-inject-variables "\\`user-mail-address\\'")
      (format "user-mail-address = %s" user-mail-address))

   ;; What to do when it finishes
   (lambda (result)
     (message "Async process done: %s" result))))


(provide 'async-test)

;;; async-test.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
