;;; -*- lexical-binding: t -*-
;;; dired-async --- Copy/move/delete asynchronously in dired

;; Copyright (C) 2012 John Wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Created: 14 Jun 2012
;; Version: 1.0
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

;; The function, which must be loaded *after* dired-aux.el, performs copies,
;; moves and deletes in the background using a slave Emacs process, by means
;; of the async.el module.  To use it, put this in your .emacs:
;;
;;   (eval-after-load "dired-aux"
;;     '(require 'dired-async))
;;
;; NOTE: If you have `delete-by-moving-to-trash' set to t, and you enable
;; `dired-async-use-native-commands', you will need to install the following
;; bash script on your system's PATH as "rmtrash".  Please edit to suit your
;; system.  It depends on the GNU realpath
;;
;; #!/bin/bash
;;
;; function mv_to_trash {
;;     path="$1"
;;     trash="$2"
;;
;;     if test -L "$path"; then
;;         rm -f "$path"           # don't trash symlinks, just remove them
;;     else
;;         target="$trash"/$(basename "$path")
;;         if test -e "$target"; then
;;             for (( index=$$ ; 1; index=index+1 )); do
;;                 target="$target"-"$index"
;;                 if ! test -e "$target"; then
;;                     break
;;                 fi
;;             done
;;         fi
;;         mv -f "$path" "$target" # don't worry about race-condition overwrites
;;     fi
;; }
;;
;; for item in "$@"; do
;;     if [[ -n "$item" && ${item:0:1} == '-' ]]; then
;;         continue
;;     elif ! test -e "$item"; then
;;         continue
;;     else
;;         target=$(realpath "$item")
;;         if [[ "$target" =~ ^/Volumes/([^/]+)/ ]]; then
;;             mv_to_trash "$item" "/Volumes/${BASH_REMATCH[1]}/.Trashes/$EUID"
;;         else
;;             mv_to_trash "$item" "$HOME/.Trash"
;;         fi
;;     fi
;; done

;;; Code:

(require 'dired-aux)
(require 'async)

(defgroup dired-async nil
  "Copy/move/delete asynchronously in dired"
  :group 'dired)

(defcustom dired-async-use-native-commands nil
  "If non-nil, use native cp/mv/rm commands for local-only files."
  :type 'boolean
  :group 'dired-async)

(defun dired-after-file-create (to actual-marker-char &optional overwrite)
  (if overwrite
      ;; If we get here, file-creator hasn't been aborted
      ;; and the old entry (if any) has to be deleted
      ;; before adding the new entry.
      (dired-remove-file to))
  (dired-add-file to actual-marker-char))

(eval-when-compile
  (defvar actual-marker-char)
  (defvar overwrite)
  (defvar async-callback))

(defun dired-copy-file-recursive (from to ok-flag &optional
                                       preserve-time top recursive)
  (when (and (eq t (car (file-attributes from)))
             (file-in-directory-p to from))
    (error "Cannot copy `%s' into its subdirectory `%s'" from to))
  (let ((attrs (file-attributes from))
        (callback `(lambda (&optional ignore)
                     (dired-after-file-create ,to ,actual-marker-char
                                              ,overwrite))))
    (if (and recursive
             (eq t (car attrs))
             (or (eq recursive 'always)
                 (yes-or-no-p (format "Recursive copies of %s? " from))))
        ;; This is a directory.
        (if (and dired-async-use-native-commands
                 (not (file-remote-p from))
                 (not (file-remote-p to)))
            (async-start-process "cp" (executable-find "cp") callback
                                 (if preserve-time "-pR" "-R") from to)
          (async-start (apply-partially #'copy-directory from to preserve-time)
                       callback))
      ;; Not a directory.
      (or top (dired-handle-overwrite to))
      (condition-case err
          (if (stringp (car attrs))
              ;; It is a symlink
              (make-symbolic-link (car attrs) to ok-flag)
            (if (and dired-async-use-native-commands
                     (not (file-remote-p from))
                     (not (file-remote-p to)))
                (let ((args (list "-f" from to)))
                  (if preserve-time
                      (setq args (cons "-p" args)))
                  (apply #'async-start-process "cp" (executable-find "cp")
                         callback args))
              (async-start (apply-partially #'copy-file from to ok-flag
                                            preserve-time)
                           callback)))
        (file-date-error
         (push (dired-make-relative from)
               dired-create-files-failures)
         (dired-log "Can't set date on %s:\n%s\n" from err))))))

(defun dired-rename-file (file newname ok-if-already-exists)
  (dired-handle-overwrite newname)
  (let ((callback
         `(lambda (&optional ignore)
            ;; Silently rename the visited file of any buffer visiting this
            ;; file.
            (and (get-file-buffer ,file)
                 (with-current-buffer (get-file-buffer ,file)
                   (set-visited-file-name ,newname nil t)))
            (dired-remove-file ,file)
            ;; See if it's an inserted subdir, and rename that, too.
            (dired-rename-subdir ,file ,newname)

            (dired-after-file-create ,newname ,actual-marker-char ,overwrite))))
    (if (and dired-async-use-native-commands
             (not (file-remote-p file))
             (not (file-remote-p newname)))
        (let ((args (list "-f" file newname)))
          (unless ok-if-already-exists
            (setq args (cons "-n" args)))
          (apply #'async-start-process "mv" (executable-find "mv")
                 callback args))
      (async-start (apply-partially #'rename-file file newname
                                    ok-if-already-exists)
                   callback))))

(defun dired-delete-file (file &optional recursive trash) "\
Delete FILE or directory (possibly recursively if optional RECURSIVE is true.)
RECURSIVE determines what to do with a non-empty directory.  If RECURSIVE is:
nil, do not delete.
`always', delete recursively without asking.
`top', ask for each directory at top level.
Anything else, ask for each sub-directory."
  ;; This test is equivalent to
  ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
  ;; but more efficient
  (if (not (eq t (car (file-attributes file))))
      (cond
       ;; How to reliably trash files on other systems?  Use Emacs to do it
       (trash
        (async-start-process "rmtrash" (executable-find "rmtrash")
                             'ignore "-f" file))
       ((and (not trash) dired-async-use-native-commands
             (not (file-remote-p file)))
        (async-start-process "rm" (executable-find "rm") 'ignore "-f" file))
       (t
        (async-start (apply-partially #'delete-file file trash)
                     'ignore)))
    (if (and recursive
             (directory-files file t dired-re-no-dot) ; Not empty.
             (or (eq recursive 'always)
                 (yes-or-no-p (format "Recursively %s %s? "
                                      (if (and trash
                                               delete-by-moving-to-trash)
                                          "trash"
                                        "delete")
                                      (dired-make-relative file)))))
        (if (eq recursive 'top) (setq recursive 'always)) ; Don't ask again.
      (setq recursive nil))
    (if (and dired-async-use-native-commands
             (not (file-remote-p file)))
        (if recursive
            (if trash
                (async-start-process "rmtrash" (executable-find "rmtrash")
                                     'ignore "-fr" file)
              (async-start-process "rm" (executable-find "rm")
                                   'ignore "-fr" file))
          (async-start-process "rmdir" (executable-find "rmdir")
                               'ignore file))
      (async-start (apply-partially #'delete-directory file recursive trash)
                   'ignore))))

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
  (let (dired-create-files-failures failures
        skipped (success-count 0) (total (length fn-list)))
    (let (to overwrite-query
             overwrite-backup-query)	; for dired-handle-overwrite
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
            (condition-case err
                (funcall file-creator from to dired-overwrite-confirmed)
              (file-error		; FILE-CREATOR aborted
               (progn
                 (push (dired-make-relative from)
                       failures)
                 (dired-log "%s `%s' to `%s' failed:\n%s\n"
                            operation from to err))))))))
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
     (t
      (message "%s proceeding asynchronously..." operation)))))

(defun dired-internal-do-deletions (l arg &optional trash)
  ;; L is an alist of files to delete, with their buffer positions.
  ;; ARG is the prefix arg.
  ;; Filenames are absolute.
  ;; (car L) *must* be the *last* (bottommost) file in the dired buffer.
  ;; That way as changes are made in the buffer they do not shift the
  ;; lines still to be changed, so the (point) values in L stay valid.
  ;; Also, for subdirs in natural order, a subdir's files are deleted
  ;; before the subdir itself - the other way around would not work.
  (let* ((files (mapcar (function car) l))
         (count (length l))
         (succ 0)
         (trashing (and trash delete-by-moving-to-trash))
         (progress-reporter
          (make-progress-reporter
           (if trashing "Trashing..." "Deleting...")
           succ count)))
    ;; canonicalize file list for pop up
    (setq files (nreverse (mapcar (function dired-make-relative) files)))
    (if (dired-mark-pop-up
         " *Deletions*" 'delete files dired-deletion-confirmer
         (format "%s %s "
                 (if trashing "Trash" "Delete")
                 (dired-mark-prompt arg files)))
        (save-excursion
          (let (failures);; files better be in reverse order for this loop!
            (while l
              (goto-char (cdr (car l)))
              (let ((inhibit-read-only t))
                (condition-case err
                    (let ((fn (car (car l))))
                      (dired-delete-file fn dired-recursive-deletes trash)
                      ;; if we get here, removing worked
                      (setq succ (1+ succ))
                      (progress-reporter-update progress-reporter succ)
                      (dired-fun-in-all-buffers
                       (file-name-directory fn) (file-name-nondirectory fn)
                       (function dired-delete-entry) fn))
                  (error;; catch errors from failed deletions
                   (dired-log "%s\n" err)
                   (setq failures (cons (car (car l)) failures)))))
              (setq l (cdr l)))
            (if (not failures)
                (progress-reporter-done progress-reporter)
              (dired-log-summary
               (format "%d of %d deletion%s failed"
                       (length failures) count
                       (dired-plural-s count))
               failures))))
      (message "(No deletions performed)"))))

(provide 'dired-async)

;;; dired-async.el ends here
