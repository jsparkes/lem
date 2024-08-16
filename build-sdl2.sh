#!/bin/sh 
ocicl install
# sbcl --no-userinit --load scripts/ocicl-sdl2.lisp
sbcl --noinform --no-sysinit --no-userinit --load scripts/ocicl-sdl2.lisp
