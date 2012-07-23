Purpose
=======

It is just copy of my configuration files for various utilities such
as Emacs, Vim, etc. Fill free to use them.

Install
=======

Just run `make` in the project root directory. It will backup your old
configuration and will make a `~/.<file>` symbolic link to a file or a
directory with real configuration (so the future updates will be
pulled automatically with `git pull`).

If you want to do sync by hand in future, run `make install-copy`. It
will copy the resource files recursively so the future updates will
not affect your configuration.
