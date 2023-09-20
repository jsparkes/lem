Save and restore all buffers editing files.
Add to ~/.lem/init.lisp:

```common-lisp
(when (load-library "desktop")
  (lem:add-hook lem:*exit-editor-hook* 'desktop-save)
  (desktop-restore))



