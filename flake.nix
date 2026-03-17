{
  description = "emacs-async - Asynchronous processing in Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        emacs = pkgs.emacs-nox;
        emacsWithPackages = (pkgs.emacsPackagesFor emacs).emacsWithPackages
          (epkgs: [
            epkgs.package-lint
            epkgs.buttercup
          ]);

        sourceFiles =
          "async.el async-bytecomp.el async-package.el dired-async.el smtpmail-async.el";

        copySrc = ''
          cp $src/*.el .
          chmod +w *.el
          if [ -d $src/tests ]; then
            mkdir -p tests
            cp $src/tests/*.el tests/
            chmod +w tests/*.el
          fi
        '';

      in {
        checks = {
          compile = pkgs.runCommand "async-compile" {
            nativeBuildInputs = [ emacsWithPackages ];
            src = self;
          } ''
            ${copySrc}
            HOME=$TMPDIR emacs -Q --batch -L . \
              --eval '(setq byte-compile-error-on-warn t)' \
              -f batch-byte-compile \
              ${sourceFiles}
            touch $out
          '';

          lint = pkgs.runCommand "async-lint" {
            nativeBuildInputs = [ emacsWithPackages ];
            src = self;
          } ''
            ${copySrc}
            HOME=$TMPDIR emacs -Q --batch -L . \
              -l package-lint \
              --eval '
              (progn
                (require (quote cl-lib))
                (find-file "async.el")
                (let* ((issues (package-lint-buffer))
                       (errors (cl-remove-if-not
                                (lambda (i) (eq (nth 2 i) (quote error)))
                                issues)))
                  (dolist (i issues)
                    (message "%s:%d:%d: %s: %s"
                             (buffer-file-name) (nth 0 i) (nth 1 i)
                             (nth 2 i) (nth 3 i)))
                  (when errors
                    (kill-emacs 1))))'
            touch $out
          '';

          format = pkgs.runCommand "async-format-check" {
            nativeBuildInputs = [ emacsWithPackages ];
            src = self;
          } ''
            ${copySrc}
            for f in ${sourceFiles}; do
              HOME=$TMPDIR emacs -Q --batch "$f" --eval '
                (progn
                  (require (quote cl-lib))
                  (setq-default indent-tabs-mode nil)
                  (let ((original (buffer-string)))
                    (indent-region (point-min) (point-max))
                    (unless (string= original (buffer-string))
                      (message "Formatting differs: %s" buffer-file-name)
                      (kill-emacs 1))))'
            done
            touch $out
          '';

          test = pkgs.runCommand "async-test" {
            nativeBuildInputs = [ emacsWithPackages ];
            src = self;
          } ''
            ${copySrc}
            HOME=$TMPDIR emacs -Q --batch -L . \
              -l buttercup \
              -f buttercup-run-discover
            touch $out
          '';
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            emacsWithPackages
            pkgs.lefthook
          ];
          shellHook = ''
            lefthook install 2>/dev/null || true
          '';
        };
      });
}
