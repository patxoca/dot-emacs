# $Id$

SHELL=/bin/bash
DIRS=. lisp site-lisp init.d

EMACS=/opt/emacs/24.3/bin/emacs
EMACSLIB=$(HOME)/emacslib
PKGS=$(shell find $(HOME)/.emacs.d/elpa -name \*-pkg.el -exec dirname '{}' ';')
LISPDIRS=-L $(EMACSLIB)/site-lisp -L $(EMACSLIB)/site-lisp/eproject -L $(EMACSLIB)/lisp $(foreach dir, $(PKGS), -L $(dir))
ELS = $(foreach dir, $(DIRS), $(wildcard $(dir)/*.el))
ELCS = $(foreach dir, $(DIRS), $(patsubst %.el,%.elc,$(wildcard $(dir)/*.el)))
TILDES = $(foreach dir, $(DIRS), $(dir)/*~)
LOGS = $(foreach dir, $(DIRS), $(dir)/*.log)

all: $(ELCS)

%.elc : %.el
	@echo -n "Bytecompiling: $< ... "
	@($(EMACS) --no-site-file 									\
	           --batch 											\
	           $(LISPDIRS)                                      \
	           --funcall batch-byte-compile "$<" 2> "$<.log" ; 	\
	 if [ $$? -eq 0 ] ; then 									\
	     echo -e " \E[32mok\E[0m." ; 							\
		 rm "$<.log" ; 											\
	 else 														\
	     echo -e " \E[31mfail\E[0m." ; 							\
	fi )

clean:
	@rm -f $(TILDES) $(LOGS)

distclean: clean
	@rm -f $(ELCS) TAGS

TAGS: $(ELS)
	@etags $(ELS)
