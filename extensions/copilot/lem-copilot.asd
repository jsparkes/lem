(defsystem "lem-copilot"
  :depends-on ("lem" "lem-lsp-mode")
  :components ((:file "utils")
               (:file "logger")
               (:file "client")
               (:file "copilot")
               (:file "install")
               (:file "login")
               (:file "languages")
               (:file "test-commands")))
