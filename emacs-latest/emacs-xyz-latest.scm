(define-module (emacs-latest emacs-xyz-latest)
  #:use-module (srfi srfi-1)
  #:use-module (guix packages)
  #:use-module (guix git)
  #:use-module (guix git-download))

(define (package-commit pkg commit checksum)
  "Return a package variant using the given commit and sha256."
  (package
    (inherit pkg)
    (name (package-name pkg))
    (version (substring commit 0 7))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (git-checkout-url (git-reference->git-checkout
                                     (origin-uri (package-source pkg)))))
             (commit commit)))
       (sha256 (base32 checksum))
       (file-name (git-file-name name version))))))

(define %emacs-xyz-module (resolve-module '(gnu packages emacs-xyz)))

(define (package-from-data sym commit checksum)
  (let ((var (module-variable %emacs-xyz-module sym)))
    (if var
        (let ((pkg (variable-ref var)))
          (if (eq? (origin-method (package-source pkg)) git-fetch)
              (cons sym (cons pkg (package-commit pkg commit checksum)))
              #f))
        #f)))

(define %commits-file (string-append (dirname (current-filename))
                                     file-name-separator-string
                                     "commits.data"))

(define %pkgs (filter-map (lambda (x) (apply package-from-data x))
                  (call-with-input-file %commits-file read)))

(define-public %emacs-xyz-latest-replacements (map cdr %pkgs))

(define-public with-emacs-xyz-latest
  (package-input-rewriting %emacs-xyz-latest-replacements))

(define-public (emacs-xyz-latest-only sym)
  (cdr (assq-ref %pkgs sym)))

(define-public (emacs-xyz-latest sym)
  (with-emacs-xyz-latest (cdr (assq-ref %pkgs sym))))
