AC_INIT([rminizinc], [0.0.0.99])

# Find the compiler and compiler flags used by R.
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_LANG(C++)
AC_PROG_CPP

have_mzn=no
MZN_CXXFLAGS=""
MZN_LIBS="-L/usr/local/lib/libminizinc -lmzn"

PKG_PROG_PKG_CONFIG

if test [ -n "$PKG_CONFIG" ] ; then
  AC_MSG_CHECKING([pkg-config for mzn])
  if $PKG_CONFIG --exists mzn; then
    have_mzn=yes
    MZN_CXXFLAGS=`"${PKG_CONFIG}" --cflags mzn`
    MZN_LIBS=`"${PKG_CONFIG}" --libs mzn`
  fi
  AC_MSG_RESULT([${have_mzn}])
fi


if test "x${have_mzn}" = xno; then
  AC_CHECK_HEADERS(minizinc/ast.hh, [have_mzn=yes], [AC_MSG_FAILURE([
  ---------------------------------------------
   'libminizinc' and its header files are required.

   Please install libminizinc and try again.

   If you believe this library is installed on your system but
   this script is simply unable to find it, you can specify the
   include and lib paths manually:

   R CMD INSTALL ${PACKAGE_NAME} \\
     --configure-vars='LIBS=-L/path/to/libs CPPFLAGS=-I/path/to/headers'
  ---------------------------------------------
  ])])
fi


AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS}"])
AC_SUBST([PKG_LIBS], ["${PKG_LIBS} ${MZN_LIBS}"])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
  --------------------------------------------------
  Configuration for ${PACKAGE_NAME} ${PACKAGE_VERSION}

    cppflags: ${CPPFLAGS} ${PKG_CPPFLAGS} 
    libs:     ${PKG_LIBS} 

  --------------------------------------------------
"