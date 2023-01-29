(define-module (emacs-latest emacs-xyz)
  #:use-module (guix packages))

(define %replacements
  (filter identity
          (module-map (lambda (sym var)
                        (variable-ref var))
                      (resolve-module '(emacs-latest commits)))))

(for-each (lambda (el)
            (set! %replacements (assq-set! %replacements (car el) (cdr el))))
          (module-ref (resolve-module '(emacs-latest fixes)) '%replacements))

(define-public (emacs-xyz-latest-only pkg)
  (assq-ref %replacements pkg))

(define-public emacs-xyz-latest (package-input-rewriting %replacements))
