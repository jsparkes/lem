(require :asdf)

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :str)

+(and (not (member :ocicl *features*)) (member :win32 *features*))
(let ((file (merge-pathnames (parse-namestring (str:concat (namestring (user-homedir-pathname)) "AppData/Local/ocicl/")) "ocicl-runtime.lisp")))
  (when (probe-file file)
    (load file)
    (pushnew (uiop:getcwd) asdf:*central-registry* :test 'equalp)))

+(and (not (member :ocicl *features*)) (not (member :win32 *features*)))
(let ((file (merge-pathnames (parse-namestring (str:concat (namestring (user-homedir-pathname)) ".local/share/ocicl/")) "ocicl-runtime.lisp")))
  (when (probe-file file)
    (load file)
    (pushnew (uiop:getcwd) asdf:*central-registry* :test 'equalp)))

#+win32
(ql:quickload :trivial-package-local-nicknames)
(ql:quickload :lem-sdl2)
(lem:init-at-build-time)

(sb-ext:save-lisp-and-die "lem"
                          :toplevel #'lem:main
                          :executable t)
