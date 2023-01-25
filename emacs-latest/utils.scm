(define-module (emacs-latest utils)
  #:use-module (srfi srfi-1)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module (guix packages)
  #:use-module (emacs-latest commits))

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
        (append `(latest ,sym) (latest-commit-and-hash pkg))))
    #:unwind? #t))

(define %emacs-xyz-latest-module-def
  "(define-module (emacs-latest commits)
  #:use-module (emacs-latest utils))

(define-public %replacements '())
")

(define-public (write-latest-emacs-xyz file)
  (let* ((emacs-packages
          (module-map (lambda (sym var) (cons sym var))
                      (resolve-module '(gnu packages emacs-xyz))))
         ;(pkg-data (filter-map mk-pkg-commit-hash (take emacs-packages 10)))
         (pkg-data (filter-map mk-pkg-commit-hash emacs-packages))
         (pkg-data-s (sort pkg-data (lambda (a b)
                                      (string<? (symbol->string (cadr a))
                                                (symbol->string (cadr b)))))))
    (with-atomic-file-output file
      (lambda (port)
        (put-string port %emacs-xyz-latest-module-def)
        (format port "~{~s\n~}" pkg-data-s)))))

(define-public (package-commit pkg commit checksum)
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

(define-public (package-it sym commit checksum)
  (with-exception-handler
      (lambda (e)
        (format #t "error for package: ~A\nexception: ~A\n" sym e)
        #f)
    (lambda ()
      (let* ((pkg (module-ref %emacs-xyz-module sym))
             (sym-latest (symbol-append sym '-latest))
             (pkg-latest (package-commit pkg commit checksum)))
        ;(module-add! (current-module) sym-latest (make-variable pkg-latest))
        ;(module-export! (current-module) (list sym-latest))
        (set! %replacements
              (cons (cons pkg pkg-latest)
                    %replacements))
        pkg-latest))
      #:unwind? #t))

(define-syntax latest
  (syntax-rules ()
    ((_ name commit checksum)
     (package-it 'name commit checksum))))

;;(define-syntax latest
;;  (lambda (x)
;;    (syntax-case x ()
;;      ((_ name commit checksum)
;;       (let ((sym (syntax->datum #'name)))
;;         #`(define-public #,(datum->syntax x (symbol-append sym '-latest))
;;             (package-it 'name commit checksum)))))))

(export latest)
