#!/bin/sh
make distclean
./configure --with-url=$HOME/work/gnu/url/lisp \
	--with-w3=$HOME/work/gnu/w3/lisp
make
