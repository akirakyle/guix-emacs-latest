(define-module (emacs-latest utils)
  #:use-module (srfi srfi-1)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module (guix packages)
  #:use-module (emacs-latest emacs-xyz-latest)
  #:export (write-latest-emacs-xyz
            package-latest))

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
        (append `(package-latest ,sym) (latest-commit-and-hash pkg))))
    #:unwind? #t))

(define %emacs-xyz-latest-module-def
  "(define-module (emacs-latest emacs-xyz-latest)
  #:use-module (emacs-latest utils))

(define-public %emacs-xyz-latest-replacements '())
")

(define (write-latest-emacs-xyz file)
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

(define (package-commit pkg commit checksum)
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

(define (package-latest sym commit checksum)
  "Return a package variant using the given commit and sha256."
  (with-exception-handler
      (lambda (e)
        (format #t "error for package: ~A\nexception: ~A\n" sym e)
        #f)
    (lambda ()
      (let* ((pkg (module-ref %emacs-xyz-module sym))
             (pkg-latest (package-commit pkg commit checksum)))
        (set! %emacs-xyz-latest-replacements
              (cons (cons pkg pkg-latest)
                    %emacs-xyz-latest-replacements))))
      #:unwind? #t))

;;(define (package-commit pkg commit checksum)
;;  (format #t "package-commit ~a ~a ~a\n" pkg commit checksum)
;;  "blah")
;;
;;(define emacs-a "I am emacs-a")

;;(define-syntax package-latest-macro
;;  (lambda (x)
;;    (syntax-case x ()
;;      ((_ name commit checksum)
;;       (let ((sym (syntax->datum #'name)))
;;         #`(define-public #,(datum->syntax x (symbol-append sym '-latest))
;;             (package-commit name commit checksum)))))))

;;(package-latest emacs-a "93e5ed8c495794d1ba3c04b43041b95ce01079b1" "17wxgssx5myvmxxjwd455sl47sb9wblh8npm5wg199j1d8z097w9")

;;(module-map (lambda (sym var) (format #t "~a ~a\n" sym var))
;;            (current-module))


;;(define %commits-file (string-append (dirname (current-filename))
;;                                     file-name-separator-string
;;                                     "commits.data"))
;;
;;(define %pkgs (filter-map (lambda (x) (apply package-from-data x))
;;                  (call-with-input-file %commits-file read)))

;;(define-public %emacs-xyz-latest-replacements (map cdr %pkgs))
;;
;;(define-public with-emacs-xyz-latest
;;  (package-input-rewriting %emacs-xyz-latest-replacements))
;;
;;(define-public (emacs-xyz-latest-only sym)
;;  (cdr (assq-ref %pkgs sym)))
;;
;;(define-public (emacs-xyz-latest sym)
;;  (with-emacs-xyz-latest (cdr (assq-ref %pkgs sym))))
