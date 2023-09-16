Add to ~/.lem/init.lisp

```common-lisp
(load-library "desktop")
(lem:add-hook lem:*exit-editor-hook* 'desktop-save)
(desktop-restore)



