;;; dired-async.el --- Copy/move/delete asynchronously in dired.

;; Copyright (C) 2012~2014 John Wiegley
;; Copyright (C) 2012~2014 Thierry Volpiatto

;; Authors: John Wiegley <jwiegley@gmail.com>
;;          Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Keywords: dired async network
;; X-URL: https://github.com/jwiegley/dired-async

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

;; This file provide a redefinition of `dired-create-file' function,
;; which must be loaded *after* dired-aux.el, performs copies,
;; moves and all what is handled by `dired-create-file' in the background
;; using a slave Emacs process, by means of the async.el module.
;; To use it, put this in your .emacs:
;;
;;   (eval-after-load "dired-aux"
;;     '(require 'dired-async))
;;
;;

;;; Code:

(require 'cl-lib)
(require 'dired-aux)
(require 'async)

(eval-when-compile
  (defvar async-callback))
(defvar dired-async-operation nil)

(defgroup dired-async nil
  "Copy rename files asynchronously from dired."
  :group 'dired)

(defcustom dired-async-env-variables-regexp
  "\\`\\(tramp-\\(default\\|connection\\|remote\\)\\|ange-ftp\\)-.*"
  "Variables matching this regexp will be loaded on Child Emacs."
  :type  'regexp
  :group 'dired-async)

(defcustom dired-async-message-function 'dired-async-mode-line-message
  "Function to use to notify result when operation finish.
Should take same args as `message'."
  :group 'dired-async
  :type  'function)

(defcustom dired-async-log-file "/tmp/dired-async.log"
  "File use to communicate errors from Child Emacs to host Emacs."
  :group 'dired-async
  :type 'string)

(defcustom dired-async-be-async t
  "When non--nil make `dired-create-file' async.
This allow to turn off async features provided to this package."
  :group 'dired-async
  :type  'boolean)

(defface dired-async-message
    '((t (:foreground "yellow")))
  "Face used for mode-line message."
  :group 'dired-async)

(defface dired-async-mode-message
    '((t (:background "Firebrick1")))
  "Face used for `dired-async-mode' lighter."
  :group 'dired-async)

;;;###autoload
(define-minor-mode dired-async-mode
    "Notify mode-line that an async process run."
  :group 'dired-async
  :global t
  :lighter (:eval (propertize (format " [%s Async job(s) running]"
                                      (length (dired-async-processes)))
                              'face 'dired-async-mode-message))
  (unless dired-async-mode
    (let ((visible-bell t)) (ding))))

(defun dired-async-mode-line-message (text &rest args)
  "Notify end of operation in `mode-line'."
  (message nil)
  (let ((mode-line-format (concat
                           " " (propertize
                                (if args
                                    (apply #'format text args)
                                    text)
                                'face 'dired-async-message))))
    (force-mode-line-update)
    (sit-for 3)
    (force-mode-line-update)))

(defun dired-async-processes ()
  (cl-loop for p in (process-list)
           when (cl-loop for c in (process-command p) thereis
                         (string= "async-batch-invoke" c))
           collect p))

(defun dired-async-kill-process ()
  (interactive)
  (let* ((processes (dired-async-processes))
         (proc (car (last processes))))
    (delete-process proc)
    (unless (> (length processes) 1)
      (dired-async-mode -1))))

(defun dired-async-after-file-create (len-flist)
  "Callback function used for operation handled by `dired-create-file'."
  (unless (dired-async-processes)
    ;; Turn off mode-line notification
    ;; only when last process end.
    (dired-async-mode -1))
  (when dired-async-operation
    (if (file-exists-p dired-async-log-file)
        (progn
          (pop-to-buffer (get-buffer-create "*dired async*"))
          (erase-buffer)
          (insert "Error: ")
          (insert-file-contents dired-async-log-file)
          (delete-file dired-async-log-file))
        (run-with-timer
         0.1 nil
         dired-async-message-function "Asynchronous %s of %s file(s) on %s file(s) done"
         (car dired-async-operation) (cadr dired-async-operation) len-flist))))

(defun dired-async-maybe-kill-ftp ()
  "Return a form to kill ftp process in child emacs."
  (quote
   (progn
     (require 'cl-lib)
     (let ((buf (cl-loop for b in (buffer-list)
                         thereis (and (string-match
                                       "\\`\\*ftp.*"
                                       (buffer-name b)) b))))
       (when buf (kill-buffer buf))))))

(defun dired-create-files (file-creator operation fn-list name-constructor
                           &optional marker-char)
  "Create one or more new files from a list of existing files FN-LIST.
This function also handles querying the user, updating Dired
buffers, and displaying a success or failure message.

FILE-CREATOR should be a function.  It is called once for each
file in FN-LIST, and must create a new file, querying the user
and updating Dired buffers as necessary.  It should accept three
arguments: the old file name, the new name, and an argument
OK-IF-ALREADY-EXISTS with the same meaning as in `copy-file'.

OPERATION should be a capitalized string describing the operation
performed (e.g. `Copy').  It is used for error logging.

FN-LIST is the list of files to copy (full absolute file names).

NAME-CONSTRUCTOR should be a function accepting a single
argument, the name of an old file, and returning either the
corresponding new file name or nil to skip.

Optional MARKER-CHAR is a character with which to mark every
newfile's entry, or t to use the current marker character if the
old file was marked."
  (setq dired-async-operation nil)
  (let (dired-create-files-failures failures async-fn-list
        skipped (success-count 0) (total (length fn-list))
        (callback `(lambda (&optional ignore)
                     (dired-async-after-file-create ,(length fn-list)))))
    (let (to overwrite-query
             overwrite-backup-query)   ; for dired-handle-overwrite
      (dolist (from fn-list)
        (setq to (funcall name-constructor from))
        (if (equal to from)
            (progn
              (setq to nil)
              (dired-log "Cannot %s to same file: %s\n"
                         (downcase operation) from)))
        (if (not to)
            (setq skipped (cons (dired-make-relative from) skipped))
          (let* ((overwrite (file-exists-p to))
                 (dired-overwrite-confirmed ; for dired-handle-overwrite
                  (and overwrite
                       (let ((help-form '(format "\
Type SPC or `y' to overwrite file `%s',
DEL or `n' to skip to next,
ESC or `q' to not overwrite any of the remaining files,
`!' to overwrite all remaining files with no more questions." to)))
                         (dired-query 'overwrite-query
                                      "Overwrite `%s'?" to))))
                 ;; must determine if FROM is marked before file-creator
                 ;; gets a chance to delete it (in case of a move).
                 (actual-marker-char
                  (cond  ((integerp marker-char) marker-char)
                         (marker-char (dired-file-marker from)) ; slow
                         (t nil))))
            ;; Handle the `dired-copy-file' file-creator specially
            ;; When copying a directory to another directory or
            ;; possibly to itself or one of its subdirectories.
            ;; e.g "~/foo/" => "~/test/"
            ;; or "~/foo/" =>"~/foo/"
            ;; or "~/foo/ => ~/foo/bar/")
            ;; In this case the 'name-constructor' have set the destination
            ;; TO to "~/test/foo" because the old emacs23 behavior
            ;; of `copy-directory' was to not create the subdirectory
            ;; and instead copy the contents.
            ;; With the new behavior of `copy-directory'
            ;; (similar to the `cp' shell command) we don't
            ;; need such a construction of the target directory,
            ;; so modify the destination TO to "~/test/" instead of "~/test/foo/".
            (let ((destname (file-name-directory to)))
              (when (and (file-directory-p from)
                         (file-directory-p to)
                         (eq file-creator 'dired-copy-file))
                (setq to destname))
              ;; If DESTNAME is a subdirectory of FROM, not a symlink,
              ;; and the method in use is copying, signal an error.
              (and (eq t (car (file-attributes destname)))
                   (eq file-creator 'dired-copy-file)
                   (file-in-directory-p destname from)
                   (error "Cannot copy `%s' into its subdirectory `%s'"
                          from to)))
            (if dired-async-be-async
                (if overwrite
                    (or (and dired-overwrite-confirmed
                             (push (cons from to) async-fn-list))
                        (progn
                          (push (dired-make-relative from) failures)
                          (dired-log "%s `%s' to `%s' failed"
                                     operation from to)))
                    (push (cons from to) async-fn-list))
                (condition-case err
                    (progn
                      (funcall file-creator from to dired-overwrite-confirmed)
                      (if overwrite
                          ;; If we get here, file-creator hasn't been aborted
                          ;; and the old entry (if any) has to be deleted
                          ;; before adding the new entry.
                          (dired-remove-file to))
                      (setq success-count (1+ success-count))
                      (message "%s: %d of %d" operation success-count total)
                      (dired-add-file to actual-marker-char))
                  (file-error		; FILE-CREATOR aborted
                   (progn
                     (push (dired-make-relative from)
                           failures)
                     (dired-log "%s `%s' to `%s' failed:\n%s\n"
                                operation from to err)))))))))
    ;; Handle error happening in host emacs.
    (cond
     (dired-create-files-failures
      (setq failures (nconc failures dired-create-files-failures))
      (dired-log-summary
       (format "%s failed for %d file%s in %d requests"
                operation (length failures)
                (dired-plural-s (length failures))
                total)
       failures))
     (failures
      (dired-log-summary
       (format "%s failed for %d of %d file%s"
                operation (length failures)
                total (dired-plural-s total))
       failures))
     (skipped
      (dired-log-summary
       (format "%s: %d of %d file%s skipped"
                operation (length skipped) total
                (dired-plural-s total))
       skipped))
     (t (message "%s: %s file%s"
                   operation success-count (dired-plural-s success-count))))
    ;; Start async process.
    (when (and async-fn-list dired-async-be-async)
      (async-start `(lambda ()
                      (require 'cl-lib) (require 'dired-aux) (require 'dired-x)
                      ,(async-inject-variables dired-async-env-variables-regexp)
                      (condition-case err
                          (let ((dired-recursive-copies (quote always)))
                            (cl-loop for (f . d) in (quote ,async-fn-list)
                                     do (funcall (quote ,file-creator) f d t)))
                        (file-error
                         (with-temp-file ,dired-async-log-file
                           (insert (format "%S" err)))))
                      ,(dired-async-maybe-kill-ftp))
                   callback)
      ;; Run mode-line notifications while process running.
      (dired-async-mode 1)
      (setq dired-async-operation (list operation (length async-fn-list)))
      (message "%s proceeding asynchronously..." operation)))
  (unless dired-async-be-async
    (dired-move-to-filename)))


(provide 'dired-async)

;;; dired-async.el ends here
