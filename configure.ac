AC_INIT([rminizinc], [0.0.2])

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

case "${OS}" in
    Windows_NT*)
        BUILD_PLATFORM=Windows
        ;;
    *)
        uname_str=`uname -s`
        case "${uname_str}" in
            cygwin*|mingw*)
                BUILD_PLATFORM=Windows
                ;;
            *)
                BUILD_PLATFORM=MAC_OR_UNIX-LIKE
                ;;
        esac
esac

AC_SUBST([DEFAULT_INCLUDE], "")
AC_SUBST([MZN_INCLUDE], "")
AC_SUBST([MZN_HEADER_PATH], "")
AC_SUBST([MZN_LIBS], "")
AC_SUBST([MZN_LIB_PATH], "")

AC_ARG_WITH(
	[mzn],
    [AC_HELP_STRING(
    	[--with-mzn=PREFIX],
        [path to where libminizinc is installed])],
            [
            	AS_IF([test "$BUILD_PLATFORM" = "Windows"], 
            	[
             		AC_MSG_NOTICE([Custom libminizinc path provided!])
					MZN_INCLUDE="-I${with_mzn}/include"
					MZN_HEADER_PATH="${with_mzn}/include/minizinc"
					MZN_LIBS="-L${with_mzn} -lmzn"
					MZN_LIB_PATH="${with_mzn}/libmzn.a"
					AC_CHECK_HEADER("${MZN_HEADER_PATH}/ast.hh",
 						[AC_SUBST([MZN_PATH], 50)], [AC_MSG_NOTICE([header file ast.hh not found at custom path!])])
					file=${with_mzn}/libmzn.a
				
					if [test -e "$file" ]; then
							AC_SUBST([LIB_PATH], "${with_mzn}/libmzn.a")
					else 
  							AC_MSG_NOTICE([libmzn.a not found at custom path!])
					fi 

            	],
            	[
            		AC_MSG_NOTICE([Custom libminizinc path provided!])
					MZN_INCLUDE="-I${with_mzn}/include"
					MZN_HEADER_PATH="${with_mzn}/include/minizinc"
					MZN_LIBS="-L${with_mzn} -lmzn"
					MZN_LIB_PATH="${with_mzn}/libmzn.a"
					
					AC_CHECK_HEADER("${MZN_HEADER_PATH}/ast.hh",
 						[AC_SUBST([MZN_PATH], 50)], [AC_MSG_NOTICE([header file ast.hh not found at custom path!])])

					file=${with_mzn}/libmzn.a
					
					if [test -e "$file" ]; then
    					AC_SUBST([LIB_PATH], "${with_mzn}/libmzn.a")
					else 
    					AC_MSG_NOTICE([libmzn.a not found at custom path!])
					fi
            	]
            )],
            [
            	AS_IF([test "$BUILD_PLATFORM" = "Windows"], 
            	[
            		AC_MSG_NOTICE([Custom libminizinc path not provided!])
             		AC_SUBST([DEFAULT_INCLUDE], "C:/Program Files/libminizinc")
					MZN_INCLUDE="-I${DEFAULT_INCLUDE}/include"
					MZN_HEADER_PATH="${DEFAULT_INCLUDE}/include/minizinc"
					MZN_LIBS="-L${DEFAULT_INCLUDE} -lmzn"
					MZN_LIB_PATH="${DEFAULT_INCLUDE}/libmzn.a"
					
					file=${DEFAULT_INCLUDE}/libmzn.a

					if [test -e "$file" ]; then
  							AC_SUBST([LIB_PATH], "${DEFAULT_INCLUDE}/libmzn.a")
                            AC_CHECK_HEADER("${MZN_HEADER_PATH}/ast.hh",
                            [AC_SUBST([MZN_PATH], 50)], [AC_MSG_NOTICE([header file ast.hh not found at default path!])])
					else 
  							AC_MSG_NOTICE([libmzn.a not found at default path!])
					fi 
            	],
            	[
            		AC_MSG_NOTICE([Custom libminizinc path not provided!])
             		AC_SUBST([DEFAULT_INCLUDE], "/usr/local/lib/libminizinc")
					MZN_INCLUDE="-I${DEFAULT_INCLUDE}/include"
					MZN_HEADER_PATH="${DEFAULT_INCLUDE}/include/minizinc"
					MZN_LIBS="-L${DEFAULT_INCLUDE} -lmzn"
					MZN_LIB_PATH="${DEFAULT_INCLUDE}/libmzn.a"

 					file=${DEFAULT_INCLUDE}/libmzn.a
					
					if [test -e "$file" ]; then
    					AC_SUBST([LIB_PATH], "${DEFAULT_INCLUDE}/libmzn.a")
                        AC_CHECK_HEADER("${MZN_HEADER_PATH}/ast.hh",
                        [AC_SUBST([MZN_PATH], 50)], [AC_MSG_NOTICE([header file ast.hh not found at default path!])])
					else 
    					AC_MSG_NOTICE([libmzn.a not found at default path!])
					fi 
            	]
            )]
    )


AC_ARG_WITH(
	[bin],
    [AC_HELP_STRING(
    	[--with-bin=PREFIX],
        [path to the solver binaries. DON'T USE BACKSLASH AT THE END OF THE PATH])],
        [
        	AC_SUBST([BIN_PATH], ${with_bin})
            AS_IF([test "$MZN_PATH" = "50"], 
                [
                    AC_SUBST([SLV_BIN], 50)        
                ],
                [
                    AC_MSG_NOTICE([Solver binaries are present but libminizinc is not present!])
                ] )
        	
        	AC_CONFIG_FILES([inst/minizinc/solvers/gecode.msc])
        	AC_CONFIG_FILES([inst/minizinc/solvers/chuffed.msc])
        	AC_CONFIG_FILES([inst/minizinc/solvers/findmus.msc])
        	AC_CONFIG_FILES([inst/minizinc/solvers/gecode-gist.msc])
        	AC_CONFIG_FILES([inst/minizinc/solvers/globalizer.msc])
        ],
        [
        	AC_MSG_NOTICE([Path to the solver binaries is not provided!])
        ])

AC_SUBST([PROJ_ROOT], "$ac_abs_confdir")

AS_IF([test "${MZN_PATH}" != 50],
			[
				MZN_HEADER_PATH=""
                MZN_INCLUDE=""
                MZN_LIBS=""
			])

AS_IF([test "${LIB_PATH}" = ""],
			[
                MZN_LIBS=""
			])

CPP_FLAGS="${CPP_FLAGS} ${MZN_INCLUDE}"
CXX_FLAGS="${CXX_FLAGS} ${MZN_INCLUDE}"
LDFLAGS="${LDFLAGS} ${MZN_LIBS}"


AS_IF([test "$BUILD_PLATFORM" = "Windows"], 
[
	AC_CONFIG_FILES([src/Makevars.win])
], 
[
	AC_CONFIG_FILES([src/Makevars])

])

AC_CONFIG_FILES([src/config.h])
AC_OUTPUT

echo "
  --------------------------------------------------
  Configuration for ${PACKAGE_NAME} ${PACKAGE_VERSION}

    cppflags: ${CPPFLAGS} ${PKG_CPPFLAGS} ${MZN_INCLUDE}
    ld flags:     ${PKG_LIBS} ${MZN_LIBS}
    header file folder path:	${MZN_HEADER_PATH}
    libmzn path:      ${LIB_PATH}
    project directory:    $ac_abs_confdir
    build platform:		${BUILD_PLATFORM}
    solver binaries: 	${with_bin}

  --------------------------------------------------
"