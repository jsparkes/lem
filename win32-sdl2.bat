set LEM_BIN=../lem-bin
set PATH=%PATH%;%LEM_BIN%\lib
ocicl install
sbcl --no-userinit --load scripts/ocicl-sdl2.lisp
rem sbcl --noinform --no-sysinit --no-userinit --load scripts/ocicl-sdl2.lisp
