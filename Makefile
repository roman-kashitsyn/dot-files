SHELL := bash
QUIET := @

MV     := mv
LN     := ln -s
CP     := cp -R
RM     := rm -rf
CD     := cd
ECHO   := echo
UNLINK := unlink
WGET   := wget
PUSHD  := pushd
POPD   := popd
UNTAR  := tar xf
MAKE   := make

resources = .emacs.d .vim .vimrc .yi .xmonad .xmodmaprc .tmux.conf .nanorc

TIMESTAMP  := $(shell date +"%Y%m%d")
TMPDIR := /tmp

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
	$(ECHO) "Intalling $(resources)"
	$(QUIET)$(foreach rsc,$(resources),$(call rename-resource,~/$(rsc)))
	$(ECHO) "Making symbolic links..."
	$(QUIET)$(foreach rsc,$(resources),$(call mk-link,$(rsc)))

install-copy:
	$(QUIET)$(foreach rsc,$(resources),$(call rename-resource,~/$(rsc)))
	$(QUIET)$(foreach rsc,$(resources),$(call copy-resource,$(rsc)))
