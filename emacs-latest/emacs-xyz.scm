(define-module (emacs-latest emacs-xyz)
  #:use-module (guix packages)
  #:use-module (emacs-latest commits)
  #:use-module (emacs-latest fixed))

;;(define-public  %emacs-xyz-latest-replacements %replacements)

(define-public with-emacs-xyz-latest
  (package-input-rewriting %replacements))

(define-public (emacs-xyz-latest pkg)
  (assq-ref %replacements pkg))
