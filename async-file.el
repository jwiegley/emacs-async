;;; async-file --- Asynchronous file operations

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

(provide 'async-file)

;;; async-file.el ends here
