(require :asdf)

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(let* ((dir (if (member :win32 *features*)  "AppData/local/ocicl/" ".local/share/ocicl/"))
       (file (merge-pathnames (parse-namestring
		 	       (concatenate 'string
				 	    (namestring (user-homedir-pathname))
					    dir "/ocicl-runtime.lisp")))))
  (when (probe-file file)
    (load file)
    (pushnew (uiop:getcwd) asdf:*central-registry* :test 'equalp)))

(ql:quickload :str)

;; (princ (asdf:asdf-version))

#+win32
(setf (uiop:getenv "PATH")
      (concatenate 'string (uiop:getenv "PATH")
		   ";" (namestring (uiop:getcwd)) "win64"))

;; (princ (uiop:getenv "PATH"))

#+win32
(ql:quickload :trivial-package-local-nicknames)
(ql:quickload :lem-sdl2)

(lem:init-at-build-time)

(sb-ext:save-lisp-and-die "lem"
                          :toplevel #'lem:main
                          :executable t)
