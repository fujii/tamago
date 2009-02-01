dnl Process this file with autoconf to produce a configure script.
AC_INIT(egg.el)

dnl Checks for programs.
AC_PROG_INSTALL
AC_PROG_MAKE_SET

dnl Checks for libraries.

dnl Checks for header files.

dnl Checks for typedefs, structures, and compiler characteristics.

dnl Checks for library functions.

dnl check if jisx0213 is supported or not
AC_MSG_CHECKING(jisx0213)
if emacs -batch -q -no-site-file -no-init-file -l ${srcdir}/check-jisx0213.el; then
  echo "yes"; HAVE_JISX0213=""
else
  echo "no"; HAVE_JISX0213="# "
fi
AC_SUBST(HAVE_JISX0213)

lispdir=${datadir}/emacs/site-lisp
AC_SUBST(lispdir)
AC_OUTPUT(Makefile)
