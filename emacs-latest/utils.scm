(define-module (emacs-latest utils)
  #:use-module (srfi srfi-1)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module (guix packages))

(define (latest-commit-and-hash pkg)
  (unless (eq? git-fetch (origin-method (package-source pkg)))
    (raise-exception "package doesn't use git-fetch\n"))
  (define-values (checkout ref)
    (with-store store
      (latest-repository-commit
       store
       (git-checkout-url (git-reference->git-checkout
                          (origin-uri (package-source pkg)))))))
  (list
   ref
   (bytevector->nix-base32-string
    (file-hash* checkout #:recursive? #true #:select? (const #true)))))

(define (mk-pkg-commit-hash el)
  (with-exception-handler
      (lambda (e)
        (format #t "error for package: ~A\nexception: ~A\n" (car el) e)
        #f)
    (lambda ()
      (let ((sym (car el))
            (pkg (variable-ref (cdr el))))
        (cons sym (latest-commit-and-hash pkg))))
    #:unwind? #t))

(define-public (latest-emacs-xyz file)
  (let* ((emacs-packages
          (module-map (lambda (sym var) (cons sym var))
                      (resolve-module '(gnu packages emacs-xyz))))
         ;(pkg-data (filter-map mk-pkg-commit-hash (take emacs-packages 10)))
         (pkg-data (filter-map mk-pkg-commit-hash emacs-packages))
         (pkg-data-s (sort pkg-data (lambda (a b)
                                      (string<? (symbol->string (car a))
                                                (symbol->string(car b)))))))
    (with-atomic-file-output file
      (lambda (port) (format port "(\n~{~s\n~})" pkg-data-s)))))

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
  (with-exception-handler
      (lambda (e)
        (format #t "error for package: ~A\nexception: ~A\n" sym e)
        #f)
    (lambda ()
      (let* ((pkg (module-ref %emacs-xyz-module sym))
             (pkg-latest (package-commit pkg commit checksum)))
        (cons sym (cons pkg pkg-latest))))
      #:unwind? #t))

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
