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

AC_ARG_WITH([mzn],
            AC_HELP_STRING([--with-mzn=PREFIX],
                           [path to where libminizinc is installed]),
            [AC_MSG_NOTICE([Custom libminizinc path provided!])],
            [AC_SUBST([DEFAULT_INCLUDE], "/usr/local/lib/libminizinc")])

AC_SUBST([MZN_INCLUDE], "-I${DEFAULT_INCLUDE}/include")
AC_SUBST([MZN_LIBS], "-L${DEFAULT_INCLUDE} -lmzn")
AC_SUBST([MZN_LIB_PATH], "${DEFAULT_INCLUDE}/libmzn.a")


AS_IF([test -e "${with_mzn}"],
            [
             MZN_INCLUDE="-I${with_mzn}/include"
             MZN_LIBS="-L${with_mzn} -lmzn"
             MZN_LIB_PATH="${with_mzn}/libmzn.a"
            ])

CPP_FLAGS="${CPP_FLAGS} ${MZN_INCLUDE}"
CXX_FLAGS="${CXX_FLAGS} ${MZN_INCLUDE}"
LDFLAGS="${LDFLAGS} ${MZN_LIBS}"

AC_CHECK_FILE("${DEFAULT_INCLUDE}/include/minizinc/ast.hh",
 				[AC_SUBST([MZN_PATH], 50)], [AC_MSG_NOTICE([files not found!])])

AC_CHECK_FILE("${MZN_LIB_PATH}", [AC_SUBST([LIB_PATH], "${DEFAULT_INCLUDE}/libmzn.a")], 
				[AC_MSG_NOTICE([libmzn.a not found at default path!])])

AS_IF([test "${LIB_PATH}" = ""],
			[
				MZN_LIBS=""
			])

AC_CONFIG_FILES([src/Makevars src/config.h])
AC_OUTPUT

echo "
  --------------------------------------------------
  Configuration for ${PACKAGE_NAME} ${PACKAGE_VERSION}

    cppflags: ${CPPFLAGS} ${PKG_CPPFLAGS} ${MZN_INCLUDE}
    libs:     ${PKG_LIBS} ${MZN_LIBS}
    mzn:      ${with_mzn} ${LIB_PATH}

  --------------------------------------------------
"