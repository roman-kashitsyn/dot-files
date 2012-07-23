MV     := mv
LN     := ln -s
CP     := cp -R
ECHO   := echo
UNLINK := unlink

SHELL := bash
QUIET := @

TIMESTAMP  := $(shell date +"%Y%m%d")
resources = .emacs.d .vim .vimrc

define mk-link
  $(ECHO) "Linking $$PWD/$1 -> ~/$1"; \
  $(LN) $$PWD/$1 ~/$1;
endef

define copy-resource
  $(ECHO) "Copying $1 to $$HOME/$1 recursively"; \
  $(CP) $$PWD/$1 ~/$1
endef

define rename-resource
  if [ -L $1 ];                          \
  then                                   \
    echo "Removing symbolic link $1";    \
    $(UNLINK) $1;                        \
  fi;                                    \
  if [ -f $1 -o -d $1 ];                 \
  then                                   \
    echo "Moving $1 to $1.$(TIMESTAMP)"; \
    $(MV) $1 $1.$(TIMESTAMP);            \
  fi;
endef

install:
	$(QUIET)$(ECHO) "Intalling $(resources)"
	$(QUIET)$(foreach rsc,$(resources),$(call rename-resource,~/$(rsc)))
	$(QUIET)$(ECHO) "Making symbolic links..."
	$(QUIET)$(foreach rsc,$(resources),$(call mk-link,$(rsc)))

install-copy:
	$(QUIET)$(foreach rsc,$(resources),$(call rename-resource,~/$(rsc)))
	$(QUIET)$(foreach rsc,$(resources),$(call copy-resource,$(rsc)))
