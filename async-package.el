;;; async-package.el --- Fetch packages asynchronously -*- lexical-binding: t -*-

;; Copyright (C) 2014-2022 Free Software Foundation, Inc.

;; Author: Thierry Volpiatto <thievol@posteo.net>

;; Keywords: dired async byte-compile package
;; X-URL: https://github.com/jwiegley/emacs-async

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide the function `async-package-do-action' to
;; (re)install/upgrade packages asynchronously.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'async-bytecomp)
(require 'dired-async)
(require 'package)

(define-minor-mode async-package--modeline-mode
    "Notify mode-line that an async process run."
  :group 'async
  :global t
  :lighter (:eval (propertize (format " [%s async job Installing package(s)]"
                                      (length (dired-async-processes
                                               'async-pkg-install)))
                              'face 'async-package-message))
  (unless async-package--modeline-mode
    (let ((visible-bell t)) (ding))))

(defvar async-pkg-install-after-hook nil
  "Hook that run after package installation.
The hook runs in the call-back once installation is done in child emacs.")

(defface async-package-message
    '((t (:foreground "yellow")))
  "Face used for mode-line message.")

(defun async-package-do-action (action packages error-file)
  "Execute ACTION asynchronously on PACKAGES.
Argument ACTION can be one of \\='install, \\='upgrade, \\='reinstall.
Argument PACKAGES is a list of packages (symbols).
Argument ERROR-FILE is the file where errors are logged, if some."
  (require 'async-bytecomp)
  (let ((fn (pcase action
              ('install 'package-install)
              ('upgrade 'package-upgrade)
              ('reinstall 'package-reinstall)))
        (action-string (pcase action
                         ('install "Installing")
                         ('upgrade "Upgrading")
                         ('reinstall "Reinstalling"))))
    (message "%s %s package(s)..." action-string (length packages))
    (process-put
     (async-start
      `(lambda ()
         (require 'bytecomp)
         (setq package-archives ',package-archives
               package-pinned-packages ',package-pinned-packages
               package-archive-contents ',package-archive-contents
               package-user-dir ,package-user-dir
               package-alist ',package-alist
               load-path ',load-path)
         (prog1
             (condition-case err
                 (mapc ',fn ',packages)
               (error
                (with-temp-file ,error-file
                  (insert
                   (format
                    "%S:\n Please refresh package list before %s"
                    err ,action-string)))))
           (let (error-data)
             (when (get-buffer byte-compile-log-buffer)
               (setq error-data (with-current-buffer byte-compile-log-buffer
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))
               (unless (string= error-data "")
                 (with-temp-file ,async-byte-compile-log-file
                   (erase-buffer)
                   (insert error-data)))))))
      (lambda (result)
        (if (file-exists-p error-file)
            (let ((buf (find-file-noselect error-file)))
              (pop-to-buffer
               buf '(nil . ((window-height . fit-window-to-buffer))))
              (special-mode)
              (delete-file error-file)
              (async-package--modeline-mode -1))
          (when result
            (let ((pkgs (if (listp result) result (list result))))
              (when (eq action 'install)
                (customize-save-variable
                 'package-selected-packages
                 (delete-dups (append pkgs package-selected-packages))))
              (package-load-all-descriptors) ; refresh package-alist.
              (mapc #'package-activate pkgs) ; load packages.
              (async-package--modeline-mode -1)
              (message "%s %s packages done" action-string (length packages))
              (run-with-timer
               0.1 nil
               (lambda (lst str)
                 (dired-async-mode-line-message
                  "%s %d package(s) done"
                  'async-package-message
                  str (length lst)))
               packages action-string)
              (when (file-exists-p async-byte-compile-log-file)
                (let ((buf (get-buffer-create byte-compile-log-buffer)))
                  (with-current-buffer buf
                    (goto-char (point-max))
                    (let ((inhibit-read-only t))
                      (insert-file-contents async-byte-compile-log-file)
                      (compilation-mode))
                    (display-buffer buf)
                    (delete-file async-byte-compile-log-file)))))))
        (run-hooks 'async-pkg-install-after-hook)))
     'async-pkg-install t)
    (async-package--modeline-mode 1)))

(provide 'async-package)

;;; async-package.el ends here
