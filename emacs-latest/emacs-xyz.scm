(define-module (emacs-latest emacs-xyz)
  #:use-module (guix packages)
  ;; The order is important here!
  )
  ;#:use-module (emacs-latest utils)
  ;#:use-module (emacs-latest commits)
  ;#:use-module (emacs-latest fixed)
  ;#:re-export ((get-replacement . emacs-xyz-latest))
  ;#:declarative? #f)

;(define-public %replacements (make-variable '()))
;(define %replacements '())
;(define %replacements (make-variable '()))
;(define %replacements)

;(define with-emacs-xyz-latest
;  (package-input-rewriting %replacements))

;(define-public (emacs-xyz-latest pkg)
;  (assq-ref %replacements pkg))

;(set! %replacements
;      (map (lambda (el)
;             (cons (car el) (with-emacs-xyz-latest (cdr el))))
;           %replacements))

;;(define %fixed (@ (emacs-latest fixed) %fixed-replacements))
;;
;;(define %replacements
;;  (module-map
;;   (lambda (sym var)
;;     (cons sym var))
;;            (resolve-module '(emacs-latest commits)))
;;  )

;(eval-when (expand load eval)

(define %replacements
  (filter identity
          (module-map (lambda (sym var)
                        (variable-ref var))
                      (resolve-module '(emacs-latest commits)))))

(define %fixed-packages
  (module-map (lambda (sym var) 
                (let* ((pkg (car (variable-ref var)))
                       (pkg-fix-fn (cdr (variable-ref var)))
                       (pkg-latest (assq-ref %replacements pkg))
                       (pkg-fixed (pkg-fix-fn pkg-latest)))
                  (cons pkg pkg-fixed)))
              (resolve-module '(emacs-latest fixed))))

(for-each (lambda (el)
            (set! %replacements (assq-set! %replacements (car el) (cdr el))))
          %fixed-packages)


(define-public emacs-xyz-latest (package-input-rewriting %replacements))

(define %rewritten-replacements
  (map (lambda (el)
         (cons (car el) (with-replacements (cdr el))))
       %replacements))

(define-public (emacs-xyz-latest-only pkg)
  (assq-ref %replacements pkg))

(define-public (emacs-xyz-latest pkg)
  (format #t "emacs-xyz-latest; ~y , ~y" pkg (assq-ref %rewritten-replacements pkg))
  (assq-ref %rewritten-replacements pkg))
