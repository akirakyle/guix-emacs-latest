(define-module (emacs-latest emacs-xyz)
  #:use-module (guix packages)
  ;; The order is important here!
  #:use-module (emacs-latest utils)
  #:use-module (emacs-latest commits)
  #:use-module (emacs-latest fixed)
  #:re-export (emacs-xyz-latest))

;(define-public %replacements (make-variable '()))
;(define %replacements '())
;(define %replacements (make-variable '()))
;(define %replacements)

(define-public with-emacs-xyz-latest
  (package-input-rewriting %replacements))

;(define-public (emacs-xyz-latest pkg)
;  (assq-ref %replacements pkg))

;;(define %fixed (@ (emacs-latest fixed) %fixed-replacements))
;;
;;(define %replacements
;;  (module-map
;;   (lambda (sym var)
;;     (cons sym var))
;;            (resolve-module '(emacs-latest commits)))
;;  )
