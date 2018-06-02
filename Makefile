# $Id:$

SHELL := /bin/bash

.PHONY: help
help:
	@echo "clean:"
	@echo "  neteja configuracio i força ..."
	@echo
	@echo "config-update:"
	@echo "  actualitza la configuració des del repositori svn."
	@echo
	@echo "elpa-backup:"
	@echo "  fa una copia de seguretat del directori 'elpa'"
	@echo
	@echo "emacs-upgrade:"
	@echo "  recompila quan s'actualitza emacs"


.PHONY: clean
clean:
	rm settings.el settings.sh


.PHONY: config-update
config-update:
	./scripts/update_conf_from_svn


.PHONY: emacs-upgrade
emacs-upgrade: elpa-backup
	(																				\
		. scripts/config ;															\
		load_path=$$(get_load_path): ;												\
		EMACSLOADPATH=$$load_path emacs --batch										\
			--eval "(progn															\
                      (package-initialize)											\
                      (byte-recompile-directory package-user-dir nil 'force))" ;	\
	)

.PHONY: elpa-backup
elpa-backup:
	(														\
		cd .. ;												\
		tar -cjf backups/elpa-$$(date +%Y%m%d).tar.bz2 elpa	\
	)
