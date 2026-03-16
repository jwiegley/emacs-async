# makefile for async.

# Author: Thierry Volpiatto.
# Copyright (C) 2011~2022, Thierry Volpiatto, all rights reserved.

## This file is NOT part of GNU Emacs
##
## License
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.

# Emacs invocation
EMACS_COMMAND   := emacs

EMACS		:= $(EMACS_COMMAND) -Q -batch

EVAL := $(EMACS) --eval

PKGDIR := .

# Additional emacs loadpath
LOADPATH	:= -L .

# Files to compile
EL			:= $(sort $(wildcard *async*.el))

# Compiled files
ELC			:= $(EL:.el=.elc)


# Source files (excluding generated autoloads)
SRC_EL		:= $(filter-out async-autoloads.el,$(EL))

.PHONY: clean autoloads batch-compile install uninstall \
	format format-check lint lint-compile lint-package test

all: clean autoloads batch-compile

$(ELC): %.elc: %.el
	$(EMACS) $(LOADPATH) -f batch-byte-compile $<

# Compile needed files
compile: $(ELC)

# Compile all files at once
batch-compile:
	$(EMACS) $(LOADPATH) -f batch-byte-compile $(EL)

# Remove all generated files
clean:
	rm -f $(ELC)

# Make autoloads file
autoloads:
	$(EVAL) "(progn (setq generated-autoload-file (expand-file-name \"async-autoloads.el\" \"$(PKGDIR)\")) \
(setq backup-inhibited t) (update-directory-autoloads \"$(PKGDIR)\"))"

PREFIX=/usr/local/share/
DESTDIR=${PREFIX}emacs/site-lisp/emacs-async/
install:
	test -d ${DESTDIR} || mkdir ${DESTDIR}
	cp -vf *.el $(DESTDIR)
	cp -vf *.elc $(DESTDIR)
	cp -vf async-autoloads.el $(DESTDIR)

uninstall:
	rm -vf ${DESTDIR}*.elc
	rm -vf ${DESTDIR}*.el

# Format all elisp files
format:
	@for f in $(SRC_EL); do \
		$(EMACS) $(LOADPATH) $$f --eval \
		'(progn (setq-default indent-tabs-mode nil) (indent-region (point-min) (point-max)) (save-buffer))'; \
	done

# Check formatting without modifying files
format-check:
	@for f in $(SRC_EL); do \
		$(EMACS) $(LOADPATH) $$f --eval \
		'(progn (setq-default indent-tabs-mode nil) (let ((original (buffer-string))) (indent-region (point-min) (point-max)) (unless (string= original (buffer-string)) (message "Formatting differs: %s" buffer-file-name) (kill-emacs 1))))'; \
	done

# Byte-compile with warnings as errors
lint-compile:
	$(EMACS) $(LOADPATH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(SRC_EL)

# Run package-lint (requires package-lint on load-path)
lint-package:
	$(EMACS) $(LOADPATH) -l package-lint --eval \
	'(progn (require (quote cl-lib)) (find-file "async.el") (let* ((issues (package-lint-buffer)) (errors (cl-remove-if-not (lambda (i) (eq (nth 2 i) (quote error))) issues))) (dolist (i issues) (message "%s:%d:%d: %s: %s" (buffer-file-name) (nth 0 i) (nth 1 i) (nth 2 i) (nth 3 i))) (when errors (kill-emacs 1))))'

# Run all linters
lint: lint-compile lint-package

# Run buttercup tests (requires buttercup on load-path)
test:
	$(EMACS) $(LOADPATH) -l buttercup -f buttercup-run-discover
