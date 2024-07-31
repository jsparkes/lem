(defsystem "lem-lisp-mode"
  :depends-on ("alexandria"
               "trivial-types"
               "usocket"
               "micros"
               "trivia"
               "uiop"
               "lem-lisp-syntax"
               "lem"
               "lem-process"
               "lem-lsp-mode")
  :serial t
  :components ((:file "test-api")
               (:file "exporter")
               (:file "errors")
               (:file "swank-protocol")
               (:file "connections")
               (:file "message-dispatcher")
               (:file "ui-mode")
               (:file "grammar")
               (:file "implementation")
               (:file "internal-package")
               (:file "completion")
               (:file "message")
               (:file "detective")
               (:file "file-conversion")
               (:file "lisp-mode")
               (:file "message-definitions")
               (:file "repl")
               (:file "inspector")
               (:file "eval")
               (:file "sldb")
               (:file "hyperspec")
               (:file "apropos-mode")
               (:file "autodoc")
               (:file "paren-coloring")
               (:file "defstruct-to-defclass")
               (:file "quickdocs")
               (:file "package-inferred-system")
               (:file "organize-imports")
               (:file "connection-list")
               (:file "self-insert-hook")
               (:file "trace")
               (:file "class-browser")
               (:file "macroexpand")
               (:file "test-runner")
               (:file "utopian")
               (:file "highlight")
               (:file "package")))

(defsystem "lem-lisp-mode/v2"
  :depends-on ("lem-lisp-mode")
  :serial t
  :pathname "v2"
  :components ((:file "lsp-config")))
