(define-module (emacs-latest emacs)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages emacs))

(define-public emacs-next-pgtk-latest
  (package
    (inherit emacs-next-pgtk)
    (name "emacs-next-pgtk")
    (version "30.0.50-e77e9")
    (source
     (origin
       (inherit (package-source emacs-next-pgtk))
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/git/emacs.git/")
             (commit "e77e986a9b7d735c0e39198c8b80a34a29005fc5")))
       (file-name (git-file-name name version))
       (patches (cons
                 (local-file "emacs-native-comp-driver-options.patch")
                 (search-patches "emacs-exec-path.patch"
                                 "emacs-fix-scheme-indent-function.patch"
                                 "emacs-pgtk-super-key-fix.patch")))
       (sha256
        (base32
         "1gwr4jzkyf6qrf0jw874avpvwx785fah1ib33kqcixhvxfq05gj5"))))))
