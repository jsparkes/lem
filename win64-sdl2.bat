rem set LEM_BIN=../lem-bin
rem set PATH=%PATH%;win64;%LEM_BIN%\lib
ocicl install
rem ccl -n  --load scripts/ocicl-sdl2.lisp
sbcl --noinform --no-sysinit --no-userinit --load scripts/ocicl-sdl2.lisp
