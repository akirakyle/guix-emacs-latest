(define-module (emacs-latest fixed)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz))

(define-public %replacements '())

(define (get-latest-pkg sym)
  (cdr (module-ref (resolve-module '(emacs-latest commits)) sym)))

(define (update-replacement old new)
  (set! %replacements (cons (cons old new) %replacements)))

(define emacs-org-latest (get-latest-pkg 'emacs-org))
(define emacs-org-fixed
  (package
    (inherit emacs-org-latest)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-org-latest)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'fix-tests
              (lambda* (#:key inputs #:allow-other-keys)
                ;; XXX: Running tests updates ID locations.  The process expects
                ;; a file to be writeable in "~/.emacs.d/".
                (setenv "HOME" (getcwd))
                (mkdir-p ".emacs.d")
                ;; These files are modified during testing.
                (with-directory-excursion "testing/examples"
                  (for-each make-file-writable
                            '("babel.org"
                              "ob-awk-test.org"
                              "ob-sed-test.org"))
                  ;; Specify where sh executable is.
                  (let ((sh (search-input-file inputs "/bin/sh")))
                    (substitute* "babel.org"
                      (("/bin/sh") sh))))
                ;; XXX: Skip failing tests.
                (substitute* "testing/lisp/test-ob-shell.el"
                  (("ob-shell/remote-with-stdin-or-cmdline .*" all)
                   (string-append all "  (skip-unless nil)\n")))
                (substitute* "testing/lisp/test-org.el"
                  (("test-org/org-(encode-time|time-string-to-time) .*" all)
                   (string-append all "  (skip-unless nil)\n"))))
              )))))))
(update-replacement emacs-org emacs-org-fixed)

; TODO should all the emacsql packages be unbundled like with MELPA?
;
;(define emacs-emacsql-fixed
;  (package
;    (inherit emacs-emacsql-latest)
;    (arguments
;     (ensure-keyword-arguments (package-arguments emacs-emacsql-latest)
;       `(#:emacs ,emacs-next)))
;    (inputs (modify-inputs (package-inputs emacs-emacsql-latest)
;              (delete "emacs-minimal")))))

(define emacs-emacsql-latest (get-latest-pkg 'emacs-emacsql))

(define emacs-emacsql-sqlite-builtin
  (package
    (inherit emacs-emacsql-latest)
    (name "emacs-emacsql-sqlite-builtin")
    (build-system emacs-build-system)
    (arguments
     `(#:include '("^emacsql.el$" "^emacsql-compiler.el$" "^emacsql-sqlite-builtin.el$")
       #:emacs ,emacs-next))
    (home-page "https://github.com/skeeto/emacsql")
    (synopsis "Emacs high-level SQL database front-end")
    (description "Any readable Lisp value can be stored as a value in EmacSQL,
including numbers, strings, symbols, lists, vectors, and closures.  EmacSQL
has no concept of @code{TEXT} values; it's all just Lisp objects.  The Lisp
object @code{nil} corresponds 1:1 with @code{NULL} in the database.")
    (license license:gpl3+)))

(define emacs-org-roam-latest (get-latest-pkg 'emacs-org-roam))
(define emacs-org-roam-fixed
  (package
    (inherit emacs-org-roam-latest)
    (arguments
     (substitute-keyword-arguments
         (ensure-keyword-arguments (package-arguments emacs-org-roam-latest)
                                   `(#:emacs ,emacs-next))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'fix-package-requires
              ;; This should be patched upstream since we can just use
              ;; emcasql-sqlite-builtin
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "org-roam.el"
                  (("(emacsql-sqlite \"1.0.0\") ") "")
                  (("(require 'emacsql-sqlite)") "")))
              )))))
    (propagated-inputs (modify-inputs (package-propagated-inputs emacs-org-roam-latest)
                         (delete "emacs-emacsql-sqlite3")
                         (prepend emacs-emacsql-sqlite-builtin)))))
(update-replacement emacs-org-roam emacs-org-roam-fixed)


;; TODO fix upstream package definition to not hard-code version in
;; 'install-shared-object phase so this can be simply overriden
(define emacs-zmq-latest (get-latest-pkg 'emacs-zmq))
(define emacs-zmq-fixed
  (package
    (name "emacs-zmq")
    (version (package-version emacs-zmq-latest))
    (source (package-source emacs-zmq-latest))
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda _
             (invoke "make" "src/configure")
             (substitute* "src/configure"
               (("/bin/sh") (which "sh"))
               (("/usr/bin/file") (which "file")))
             (invoke "make")))
         (add-after 'install 'install-shared-object
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (site-lisp (string-append out "/share/emacs/site-lisp"))
                    (libdir (string-append site-lisp "/zmq-"
                                           ,(package-version this-package))))
               (copy-file "emacs-zmq.so"
                          (string-append libdir "/emacs-zmq.so"))))))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list zeromq))
    (home-page "https://github.com/nnicandro/emacs-zmq")
    (synopsis "Emacs bindings to ØMQ")
    (description "This package provides Emacs bindings to ØMQ.")
    (license (list license:gpl2+     ;zmq.el
                   license:gpl3+)))) ;src/emacs-module.h
(update-replacement emacs-zmq emacs-zmq-fixed)

(define emacs-with-editor-latest (get-latest-pkg 'emacs-with-editor))
(define emacs-with-editor-fixed
  (package
    (inherit emacs-with-editor-latest)
    (propagated-inputs (modify-inputs (package-propagated-inputs emacs-with-editor-latest)
                         (delete "emacs-async")
                         (prepend emacs-compat)))))

(update-replacement emacs-with-editor emacs-with-editor-fixed)

(define emacs-citar-org-roam-latest (get-latest-pkg 'emacs-citar-org-roam))
(define emacs-citar-org-roam-fixed
  (package
    (inherit emacs-citar-org-roam-latest)
    (arguments
     (ensure-keyword-arguments (package-arguments emacs-citar-org-roam-latest)
       `(#:emacs ,emacs-next)))))
(update-replacement emacs-citar-org-roam emacs-citar-org-roam-fixed)

(define emacs-compat-latest
  (package
    (inherit emacs-compat)
    (version "29.1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.gnu.org/packages/"
                                  "compat-" version ".tar.lz"))
              (sha256
               (base32
                "0jnk81rg5w6zhf413nf3j72i2mr8vfdms55gahrdx28727w8gfj8"))))
    (native-inputs (modify-inputs (package-native-inputs emacs-compat)
                         (prepend lzip)))))
(update-replacement emacs-compat emacs-compat-latest)
