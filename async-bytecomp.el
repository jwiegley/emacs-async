;;; async-bytecomp.el --- Async functions to compile elisp files async

;; Copyright (C) 2014 John Wiegley
;; Copyright (C) 2014 Thierry Volpiatto

;; Authors: John Wiegley <jwiegley@gmail.com>
;;          Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Keywords: dired async byte-compile
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
;;
;;  This package provide the `async-byte-recompile-directory' function
;;  which allows, as the name says to recompile a directory outside of
;;  your running emacs.
;;  The benefit is your files will be compiled in a clean environment without
;;  the old *.el files loaded.
;;  Among other things, this fix a bug in package.el which recompile
;;  the new files in the current environment with the old files loaded, creating
;;  errors in most packages after upgrades.
;;
;;  NB: This package is advicing the function `package--compile'.

;;; Code:

(require 'cl-lib)
(require 'async)

(defvar async-byte-compile-log-file "~/.emacs.d/async-bytecomp.log")

(defun async-byte-recompile-directory (directory &optional arg force)
  (cl-loop with dir = (directory-files directory t "\\.elc\\'")
           unless dir return nil
           for f in dir
           when (file-exists-p f) do (delete-file f))
  ;; Ensure async is reloaded when async.elc is deleted.
  ;; This happen when recompiling its own directory.
  (load "async")
  (let ((call-back
         `(lambda (&optional ignore)
            (if (file-exists-p async-byte-compile-log-file)
                (progn
                  (pop-to-buffer (generate-new-buffer-name
                                  byte-compile-log-buffer))
                  (erase-buffer)
                  (insert-file-contents async-byte-compile-log-file)
                  (compilation-mode)
                  (delete-file async-byte-compile-log-file)
                  (let ((n 0))
                    (save-excursion
                      (goto-char (point-min))
                      (while (re-search-forward "^.*:Error:" nil t)
                        (incf n)))
                    (if (> n 0)
                        (message "Failed to compile %d files in directory `%s'" n ,directory)
                        (message "Directory `%s' compiled asynchronously with warnings" ,directory))))
                (message "Directory `%s' compiled asynchronously with success" ,directory)))))
    (async-start
     `(lambda ()
        (require 'bytecomp)
        ,(async-inject-variables "\\`load-path\\'")
        (let ((default-directory (file-name-as-directory ,directory))
              error-data)
          (add-to-list 'load-path default-directory)
          (byte-recompile-directory ,directory ,arg ,force)
          (when (get-buffer byte-compile-log-buffer)
            (setq error-data (with-current-buffer byte-compile-log-buffer
                               (buffer-substring-no-properties (point-min) (point-max))))
            (unless (string= error-data "")
              (with-temp-file ,async-byte-compile-log-file
                (erase-buffer)
                (insert error-data))))))
     call-back)
    (message "Started compiling asynchronously directory %s..." directory)))

(defadvice package--compile (around byte-compile-async activate)
  ;; FIXME this seems redundant and unneeded, the only thing it
  ;; does is loading the autoload file to update load-path but
  ;; async-byte-recompile-directory is already doing this.
  ;; for the rest (i.e installing info) it is done anyway after
  ;; compilation in package-activate (force arg).
  (package-activate-1 pkg-desc)
  (async-byte-recompile-directory (package-desc-dir pkg-desc) 0 t))

(provide 'async-bytecomp)

;;; async-bytecomp.el ends here
