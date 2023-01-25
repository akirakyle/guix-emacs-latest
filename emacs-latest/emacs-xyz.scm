(define-module (emacs-latest emacs-xyz)
  #:use-module (guix packages))

(define %replacements
  (filter identity
          (module-map (lambda (sym var)
                        (variable-ref var))
                      (resolve-module '(emacs-latest commits)))))

;;(define %fixed-packages
;;  (module-map (lambda (sym var) 
;;                (let* ((pkg (car (variable-ref var)))
;;                       (pkg-fix-fn (cdr (variable-ref var)))
;;                       (pkg-latest (assq-ref %replacements pkg))
;;                       (pkg-fixed (pkg-fix-fn pkg-latest)))
;;                  (cons pkg pkg-fixed)))
;;              (resolve-module '(emacs-latest fixed))))

(for-each (lambda (el)
            (set! %replacements (assq-set! %replacements (car el) (cdr el))))
          (module-ref (resolve-module '(emacs-latest fixed)) '%replacements))


(define-public with-emacs-xyz-latest (package-input-rewriting %replacements))

(define %rewritten-replacements
  (map (lambda (el)
         (cons (car el) (with-emacs-xyz-latest (cdr el))))
       %replacements))

(define-public (emacs-xyz-latest-only pkg)
  (assq-ref %replacements pkg))

(define-public (emacs-xyz-latest pkg)
  (assq-ref %rewritten-replacements pkg))
