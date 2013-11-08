# $Id$

SHELL=/bin/bash
DIRS=. my-lisp site-lisp init.d

EMACS=/opt/emacs/24.3/bin/emacs
EMACSLIB=$(HOME)/emacslib
LISPDIRS=-L $(EMACSLIB)/site-lisp -L $(EMACSLIB)/my-lisp
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
