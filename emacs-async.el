;;; -*- lexical-binding: t -*-
;;; emacs-async --- Asynchronous processing in Emacs

;; Copyright (C) 2012 John Wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Created: 18 Jun 2012
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

;; Adds the ability to process Lisp concurrently, with a very simple syntax:
;;
;;   (emacs-async-fork
;;        ;; What to do in the child process
;;        (lambda ()
;;          (message "This is a test")
;;          (sleep-for 3)
;;          async-sample-variable)
;;
;;        ;; What to do when it finishes
;;        (lambda (result)
;;          (message "Async process done, result should be 222: %d" result)))

(defgroup emacs-async nil
  "Simple asynchronous processing in Emacs"
  :group 'emacs)

(defvar emacs-async-callback)

(defun emacs-async-when-done (proc &optional change)
  (when (eq 'exit (process-status proc))
    (if (= 0 (process-exit-status proc))
        (with-current-buffer (process-buffer proc)
          (goto-char (point-max))
          (backward-sexp)
          (funcall emacs-async-callback (read (current-buffer)))
          ;;(kill-buffer (current-buffer))
          )
      (error "Async Emacs process failed with exit code %d"
             (process-exit-status proc)))))

(defun emacs-async-batch-invoke ()
  (with-temp-buffer
    (insert (nth 5 command-line-args))
    (goto-char (point-min))
    ;; Strip out the binding to `buf', as it is unreadable
    (while (re-search-forward "(buf \\. #<[^)]+)" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (prin1 (funcall (eval (read (current-buffer)))))))

(defmacro emacs-async-fork (start-func &optional finish-func)
  (let ((bufvar (make-symbol "buf"))
        (procvar (make-symbol "proc")))
    (require 'find-func)
    `(let* ((,bufvar (generate-new-buffer "*emacs*"))
            (,procvar
             (start-process "emacs" ,bufvar
                            (expand-file-name invocation-name
                                              invocation-directory)
                            "-Q" "-l" (find-library-name "emacs-async")
                            "-batch" "-f" "emacs-async-batch-invoke"
                            (prin1-to-string (list 'quote ,start-func)))))
       (with-current-buffer ,bufvar
         (set (make-local-variable 'emacs-async-callback) ,finish-func)
         (set-process-sentinel ,procvar #'emacs-async-when-done)))))

(provide 'emacs-async)

;;; emacs-async.el ends here
