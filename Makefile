# $Id$

SHELL=/bin/bash
SRC_DIRS=lisp site-lisp init.d
EMACS=/opt/emacs/24.3.93/bin/emacs


all:
	@echo -n "Bytecompiling: $< ... "
	EMACS_INSTALL=1 $(EMACS) --batch -l init.el 2> compile.log

clean:
	find . -name \*~ -delete

distclean: clean
	for d in $(SRC_DIRS) ; do          \
	  find $$d -name \*.elc -delete ; \
	done
