Purpose
=======

This is a Emacs config I use on my computer. I use latest Emacs
version (24 at the moment) built from sources (you can get it in
[http://alpha.gnu.org/gnu/emacs/pretest/](gnu ftp repository)), so my
config may not work on your machine. Feel free to use it.

Install
=======

Just run `make` in the project root directory. It will backup your old
emacs configuration and make a `~/.emacs.d` symbolic link to directory
with this configuration (so the future updates will be pulled
automatically via `git pull`).

If you want to do sync by hand in future, run `make install-copy`. It
will copy the `.emacs.d` recursively so the future updates will not
affect your configuration.

Contacts
========

If you have any questions please contact me
mailto:roman.kashitsyn@gmail.com
