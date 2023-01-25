(define-module (emacs-latest utils)
  #:use-module (srfi srfi-1)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix git)
  #:use-module (guix git-download)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module (guix packages)
  #:export (latest
            package-it
            find-replacement
            update-replacement
            with-emacs-xyz-latest
            package-commit
            emacs-xyz-latest
            write-latest-emacs-xyz))

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

(define %replacements-var (make-variable '()))

(module-add! (resolve-module '(emacs-latest emacs-xyz)) '%replacements %replacements-var)

(define (emacs-xyz-latest pkg)
  (assq-ref (variable-ref %replacements-var) pkg))
 
(define (update-replacement old new)
  (variable-set! %replacements-var
                 (assq-set! (variable-ref %replacements-var) old new)))

;(define %replacements (make-variable '()))

;(define (replacement-var)
;  ;(format #t "~y\n" (module-variable (resolve-module '(emacs-latest emacs-xyz)) '%replacements))
;  (module-variable (resolve-module '(emacs-latest emacs-xyz)) '%replacements))

;(define (find-replacement pkg)
;  (assq-ref (variable-ref (replacement-var)) pkg))
; 
;(define (update-replacement old new)
;  (let* ((var (replacement-var)))
;    (variable-set! var (assq-set! (variable-ref var) old new))))

;(define %replacement-var
;  (module-variable (resolve-module '(emacs-latest emacs-xyz)) '%replacements))

;;(define (get-replacements)
;;  (module-ref (resolve-module '(emacs-latest emacs-xyz)) '%replacements))
;;
;;(define (find-replacement pkg)
;;  (assq-ref (get-replacements) pkg))
;; 

;;(define function-slot-module (define-module* '(elisp-functions) #:pure #t))
;;
;;(module-add! (resolve-module '(emacs-latest emacs-xyz))
;;             '%replacements '())
;;
;;
;;(define (update-replacement old new)
;;  (module-set! (resolve-module '(emacs-latest emacs-xyz))
;;               '%replacements (assq-set! (get-replacements) old new)))

;;(define %replacements (@ (emacs-latest emacs-xyz) %replacements))
;;
;;(define (find-replacement pkg)
;;  (assq-ref %replacements pkg))
;;
;;(define (update-replacement old new)
;;  (set! %replacements (assq-set! %replacements old new)))

(define (package-it sym commit checksum)
  (with-exception-handler
      (lambda (e)
        (format #t "error for package: ~A\nexception: ~A\n" sym e)
        #f)
    (lambda ()
      (let* ((pkg (module-ref (resolve-module '(gnu packages emacs-xyz)) sym))
             ;(sym-latest (symbol-append sym '-latest))
             (pkg-latest (package-commit pkg commit checksum)))
        ;;pkg-latest))
        (update-replacement pkg pkg-latest)))
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
