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

CEDET_VERSION := 1.1

cedet-dir := cedet-$(CEDET_VERSION)
resources = .emacs.d .vim .vimrc .yi .xmonad .xmodmap

TIMESTAMP  := $(shell date +"%Y%m%d")
TMPDIR := /tmp
CEDET_TARGET := $(HOME)/$(cedet-dir)

define mk-link
  $(ECHO) "Linking $$PWD/$1 -> ~/$1"; \
  $(LN) $$PWD/$1 ~/$1;
endef

define setup-cedet
  $(PUSHD) $(TMPDIR);                                                              \
  $(ECHO) Downloading CEDET-$(CEDET_VERSION);                                      \
  $(WGET) http://ignum.dl.sourceforge.net/project/cedet/cedet/$(cedet-dir).tar.gz; \
  $(ECHO) Unpacking CEDET-$(CEDET_VERSION);                                        \
  $(UNTAR) $(cedet-dir).tar.gz;                                                    \
  $(RM)    $(cedet-dir).tar.gz;                                                    \
  $(ECHO) Building CEDET-$(CEDET_VERSION);                                         \
  $(CD) $(cedet-dir);                                                              \
  $(MAKE);                                                                         \
  $(CD) ..;                                                                        \
  $(MV) $(cedet-dir) $(CEDET_TARGET);                                              \
  $(POPD);
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

install: $(CEDET_TARGET)
	$(ECHO) "Intalling $(resources)"
	$(QUIET)$(foreach rsc,$(resources),$(call rename-resource,~/$(rsc)))
	$(ECHO) "Making symbolic links..."
	$(QUIET)$(foreach rsc,$(resources),$(call mk-link,$(rsc)))

install-copy: $(CEDET_TARGET)
	$(QUIET)$(foreach rsc,$(resources),$(call rename-resource,~/$(rsc)))
	$(QUIET)$(foreach rsc,$(resources),$(call copy-resource,$(rsc)))

$(CEDET_TARGET):
	$(QUIET)$(call setup-cedet)
