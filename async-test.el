;;; async-test --- async.el-related tests

;; Copyright (C) 2012 John Wiegley

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

(add-to-list 'load-path (file-name-directory (or load-file-name (buffer-file-name))))
(require 'async)
(require 'async-file)

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

(defun async-test-7 ()
  (interactive)
  (message "Starting async-test-7...")
  (eval
   '(progn
      (print
       (mapcar #'async-get
               (cl-loop repeat 2 collect
                        (async-start (lambda () t)))))
      (print
       (mapcar #'async-get
               (cl-loop repeat 2 collect
                        (async-start '(lambda () t)))))
      (print
       (mapcar #'async-get
               (cl-loop repeat 2 collect
                        (async-start `(lambda () ,(* 150 2)))))))
   t)
  (message "Finished async-test-7 successfully."))

(defsubst async-file-contents (file)
  "Return the contents of FILE, as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun* async-do-copy-file-test (ok-if-already-exists
                                 keep-time preserve-uid-gid
                                 preserve-selinux-context
                                 &key use-native-commands
                                 synchronously)
  (let* ((temp-file (make-temp-file "async-do-copy-file-test"))
         (temp-file2 (concat temp-file ".target")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert "async-do-copy-file-test")
            (write-region (point-min) (point-max) temp-file))

          (let* ((async-file-use-native-commands use-native-commands)
                 (future (if synchronously
                             (copy-file temp-file temp-file2
                                        ok-if-already-exists
                                        keep-time
                                        preserve-uid-gid
                                        preserve-selinux-context)
                           (async-copy-file temp-file temp-file2
                                            ok-if-already-exists
                                            keep-time
                                            preserve-uid-gid
                                            preserve-selinux-context
                                            :callback nil))))
            (unless synchronously
              (if use-native-commands
                  (let ((proc (async-get future)))
                    (should (processp proc))
                    (should (equal 'exit (process-status proc))))
                (should (equal (async-get future) nil))))

            (should (file-readable-p temp-file2))

            (should (equal "async-do-copy-file-test"
                           (async-file-contents temp-file2)))))

      (if (file-exists-p temp-file)  (delete-file temp-file))
      (if (file-exists-p temp-file2) (delete-file temp-file2)))))

(ert-deftest async-copy-file-lisp-sync-1 ()
  (async-do-copy-file-test t t t nil :synchronously t))
(ert-deftest async-copy-file-lisp-1 ()
  (async-do-copy-file-test t t t nil :use-native-commands nil))
(ert-deftest async-copy-file-native-1 ()
  (async-do-copy-file-test t t t nil :use-native-commands t))

(defsubst async-file-make-temp-dir (prefix)
  "Make a temporary directory using PREFIX.
Return the name of the directory."
  (let ((dir (make-temp-name
              (expand-file-name prefix temporary-file-directory))))
    (make-directory dir)
    dir))

(defsubst async-file-make-file (file contents)
  "Create a new FILE with the given CONTENTS."
  (with-temp-buffer
    (insert contents)
    (write-region (point-min) (point-max) file)))

(defun* async-do-copy-directory-test (keep-time parents copy-contents
                                                &key use-native-commands
                                                synchronously)
  (let* ((temp-dir (async-file-make-temp-dir "async-do-copy-directory-test"))
         (temp-dir2 (concat temp-dir ".target")))
    (unwind-protect
        (progn
          (async-file-make-file (expand-file-name "foo" temp-dir) "foo")
          (async-file-make-file (expand-file-name "bar" temp-dir) "bar")

          ;; Shouldn't the parents argument cause this to happen when needed?
          ;; But if the following is wrapped with "unless parents", even
          ;; `async-copy-directory-lisp-sync-2' fails.
          (make-directory temp-dir2)

          (let* ((async-file-use-native-commands use-native-commands)
                 (future (if synchronously
                             (copy-directory temp-dir temp-dir2
                                             keep-time
                                             parents
                                             copy-contents)
                           (async-copy-directory temp-dir temp-dir2
                                                 keep-time
                                                 parents
                                                 copy-contents
                                                 :callback nil))))
            (unless synchronously
              (if use-native-commands
                  (let ((proc (async-get future)))
                    (should (processp proc))
                    (should (equal 'exit (process-status proc))))
                ;; Ignore the return value from `copy-directory'
                (async-get future)))

            (if (and parents copy-contents)
                (should (file-directory-p temp-dir2)))

            (let* ((target (if copy-contents
                               temp-dir2
                             (expand-file-name (file-name-nondirectory temp-dir)
                                               temp-dir2)))
                   (foo-file (expand-file-name "foo" target))
                   (bar-file (expand-file-name "bar" target)))

              (should (file-readable-p foo-file))
              (should (file-readable-p bar-file))

              (should (equal "foo" (async-file-contents foo-file)))
              (should (equal "bar" (async-file-contents bar-file))))))

      (if (file-directory-p temp-dir)  (delete-directory temp-dir t))
      (if (file-directory-p temp-dir2) (delete-directory temp-dir2 t)))))

(ert-deftest async-copy-directory-lisp-sync-1 ()
  (async-do-copy-directory-test t nil nil :synchronously t))
(ert-deftest async-copy-directory-lisp-sync-2 ()
  (async-do-copy-directory-test t t nil :synchronously t))
(ert-deftest async-copy-directory-lisp-sync-3 ()
  (async-do-copy-directory-test t nil t :synchronously t))
(ert-deftest async-copy-directory-lisp-sync-4 ()
  (async-do-copy-directory-test t t t :synchronously t))

(ert-deftest async-copy-directory-lisp-1 ()
  (async-do-copy-directory-test t nil nil :use-native-commands nil))
(ert-deftest async-copy-directory-lisp-2 ()
  (async-do-copy-directory-test t t nil :use-native-commands nil))
(ert-deftest async-copy-directory-lisp-3 ()
  (async-do-copy-directory-test t nil t :use-native-commands nil))
(ert-deftest async-copy-directory-lisp-4 ()
  (async-do-copy-directory-test t t t :use-native-commands nil))

(ert-deftest async-copy-directory-native-1 ()
  (async-do-copy-directory-test t nil nil :use-native-commands t))
(ert-deftest async-copy-directory-native-2 ()
  (async-do-copy-directory-test t t nil :use-native-commands t))
(ert-deftest async-copy-directory-native-3 ()
  (async-do-copy-directory-test t nil t :use-native-commands t))
(ert-deftest async-copy-directory-native-4 ()
  (async-do-copy-directory-test t t t :use-native-commands t))

(provide 'async-test)

;;; async-test.el ends here
