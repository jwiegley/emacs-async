# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

emacs-async is an Emacs Lisp library that provides asynchronous processing capabilities in Emacs. It allows running elisp code in subprocess Emacs instances to avoid blocking the main Emacs session.

## Architecture

The library consists of several modules:

- **async.el**: Core async functionality - provides `async-start`, `async-start-process`, and related functions for running code asynchronously
- **dired-async.el**: Asynchronous file operations for Dired (copy, rename, symlink)
- **async-bytecomp.el**: Asynchronous byte compilation of packages
- **async-package.el**: Asynchronous package installation/upgrades
- **smtpmail-async.el**: Send emails asynchronously via SMTP

The core mechanism works by starting a child Emacs process with minimal initialization, injecting required variables from parent, executing the provided code, and returning results via process communication.

## Development Commands

```bash
# Build and compile
make all                    # Clean, generate autoloads, and compile all .el files
make compile               # Compile individual files  
make batch-compile         # Compile all files at once
make clean                 # Remove compiled .elc files
make autoloads            # Generate async-autoloads.el

# Testing
eask test buttercup       # Run buttercup tests (tests/test-async.el)

# Installation
make install              # Install to /usr/local/share/emacs/site-lisp/emacs-async/
make uninstall           # Remove installation

# Eask commands (alternative build system)
eask install-deps --dev   # Install development dependencies
eask package             # Create package
eask compile            # Byte compile files
```

## Key Implementation Details

- Child Emacs processes are started with `-Q` flag for minimal startup
- Variable injection between parent/child uses `async-inject-variables` 
- Results are transmitted via base64-encoded messages through process pipes
- For remote operations (TRAMP), set `async-quiet-switch` to `-q` in Emacs 26+ to enable auth-sources
- Password prompts are handled via `async-prompt-for-password` variable