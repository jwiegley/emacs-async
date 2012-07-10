;;; async --- Asynchronous file operations

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

;; Provides asynchronous versions of the following operations:
;;
;;   Function                 Command equivalent
;;   -----------------------  ---------------------------------
;;   copy-file                cp
;;   copy-directory           cp -R
;;   rename-file              mv
;;   delete-file              rm
;;   delete-directory         rm -r
;;
;; Additional features:
;;
;; - If `async-file-use-native-commands' is non-nil, and none of the files
;;   involved are `file-remote-p', the native command equivalents are used
;;   above rather than spawning a child Emacs to call the related function.
;;
;; - Operations are queued, so that only one asynchronous operation is
;;   performed at one time.  If an error occurs while processing the queue,
;;   the whole queue is aborted.

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup async-file nil
  "Asynchronous file processing using async.el"
  :group 'async)

(defcustom async-file-use-native-commands nil
  "If non-nil, use native cp/mv/rm commands for local-only files."
  :type 'boolean
  :group 'async-file)

(defvar async-file-queue nil
  "Queue of pending asynchronous file operations.
Each operation that succeeds will start the next member of the queue.  If an
error occurs at any point, the rest of the queue is flushed.")
(defvar async-file-queue-mutex nil)

(defun* async-copy-file
    (file newname
          &optional ok-if-already-exists keep-time
          preserve-uid-gid preserve-selinux-context
          &key (callback 'ignore))
  "Asynchronous version of `copy-file'.
Accepts a key argument `:callback' which takes a lambda that
receives the return value from `copy-file' (nil if Lisp was used,
a process object otherwise) when the copy is done."
  (if (and async-file-use-native-commands
           (not (or (file-remote-p file)
                    (file-remote-p newname))))

      (let ((args (list "-f" file newname)))
        (if keep-time
            (setq args (cons "-p" args)))
        (unless ok-if-already-exists
          (setq args (cons "-n" args)))
        (apply #'async-start-process "cp" (executable-find "cp")
               callback args))

    (async-start (apply-partially #'copy-file
                                  file newname ok-if-already-exists
                                  keep-time preserve-uid-gid
                                  preserve-selinux-context)
                 callback)))

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

(defun* async-copy-directory
    (directory newname
               &optional keep-time parents copy-contents
               &key (callback 'ignore))
  "Asynchronous version of `copy-directory'.
Accepts a key argument `:callback' which takes a lambda that
receives the return value from `copy-directory' (always nil) when
the copy is done."
  (if (and async-file-use-native-commands
           (not (or (file-remote-p directory)
                    (file-remote-p newname))))
      (progn
        (if parents
            (let ((dest-dir (if copy-contents
                                (file-name-directory newname)
                              newname)))
              (unless (file-directory-p dest-dir)
                (message "Creating directory '%s'" dest-dir)
                (make-directory dest-dir t))))

        (if copy-contents
            (let ((args (list "-r" (file-name-as-directory directory)
                              (file-name-as-directory newname))))
              (if keep-time
                  (setq args (cons "-a" args)))
              (apply #'async-start-process "rsync" (executable-find "rsync")
                     callback args))

          (let ((args (list "-fR" directory newname)))
            (if keep-time
                (setq args (cons "-p" args)))
            (apply #'async-start-process "cp" (executable-find "cp")
                   callback args))))

    (async-start (apply-partially #'copy-directory
                                  directory newname keep-time parents
                                  copy-contents)
                 callback)))

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

(provide 'async-file)

;;; async-file.el ends here
