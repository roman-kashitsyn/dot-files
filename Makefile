MV := mv
LN := ln -s
CP := cp -R

SHELL := bash
QUIET := @

EMACS_HOME := ~/.emacs.d
TIMESTAMP  := $(shell date +"%Y%m%d")

define echo-exit-status
  $(QUIET)if [ $$? -eq 0 ]; \
  then                      \
    echo "Done.";           \
  else                      \
    echo "Failed!";         \
  fi
endef

define rename-old-emacs-home
  $(QUIET)if [ -a $(EMACS_HOME) ];                               \
  then                                                           \
    echo "Moving $(EMACS_HOME) to $(EMACS_HOME).$(TIMESTAMP)..." \
    $(MV) $(EMACS_HOME) $(EMACS_HOME).$(TIMESTAMP);              \
  fi

endef

install:
	$(QUIET)$(rename-old-emacs-home)
	$(QUIET)echo "Making symbolic link to .emacs.d directory..."
	$(QUIET)$(LN) $$PWD/.emacs.d $(EMACS_HOME)
	$(QUIET)$(echo-exit-status)

install-copy:
	$(QUIET)$(rename-old-emacs-home)
	$(QUIET)echo "Recursively copying emacs.d directory..."
	$(QUIET)$(CP) .emacs.d $(EMACS_HOME)
	$(QUIET)$(echo-exit-status)
