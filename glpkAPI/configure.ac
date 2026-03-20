AC_INIT([glpkAPI],[1.3.3],[mayo.roettger@hhu.de])


dnl # --------------------------------------------------------------------
dnl # global (environment) variables
dnl # --------------------------------------------------------------------
dnl
dnl # PKG_CPPFLAGS       C preprocessor flags -I...
dnl # PKG_CFLAGS         C compiler flags
dnl # PKG_LIBS           libraries -l...


dnl # --------------------------------------------------------------------
dnl # R compiler and flags
dnl # --------------------------------------------------------------------

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
    echo "could not determine R_HOME"
    exit 1
fi

dnl # GNU R default settings
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_PROG_CC
AC_PROG_CPP


dnl # --------------------------------------------------------------------
dnl # template config.h.in
dnl # --------------------------------------------------------------------

AC_CONFIG_HEADERS([src/config.h])


dnl # --------------------------------------------------------------------
dnl # test for global variables
dnl # --------------------------------------------------------------------

ARGS_OK="FALSE"

if test -n "$PKG_CPPFLAGS" && test -n "$PKG_LIBS" ; then
    ARGS_OK="TRUE"
else
    if test -n "$PKG_CPPFLAGS" || test -n "$PKG_LIBS" ; then
        AC_MSG_ERROR([Please set both variables: PKG_CPPFLAGS AND PKG_LIBS])
    fi
fi


dnl # --------------------------------------------------------------------
dnl # get arguments
dnl # --------------------------------------------------------------------

AC_ARG_WITH(glpk-include,
            AS_HELP_STRING([--with-glpk-include=PATH],[location of glpk include directory [[/usr/local/include]]
                          ]),
            [GLPK_INCLUDE=$withval],
            [GLPK_INCLUDE=""])

AC_ARG_WITH(glpk-lib,
            AS_HELP_STRING([--with-glpk-lib=PATH],[location of GLPK callable library [[/usr/local/lib]]
                          ]),
            [GLPK_LIB=$withval],
            [GLPK_LIB=""])

AC_ARG_ENABLE(gmp,
              AS_HELP_STRING([--enable-gmp[=yes|no]],[link to GNU bignum library libgmp [[yes]]
                            ]),
              [GMP=$enableval], [GMP="yes"])

AC_ARG_ENABLE(chkargs,
              AS_HELP_STRING([--enable-chkargs[=yes|no]],[check arguments to GLPK [[yes]]
                            ]),
              [CHKARGS=$enableval], [CHKARGS="yes"])


dnl # --------------------------------------------------------------------
dnl # test arguments
dnl # --------------------------------------------------------------------

dnl # include and library directories
if test [ "$ARGS_OK" = "FALSE" ] ; then

    if test -n "$GLPK_INCLUDE" && test -n "$GLPK_LIB" ; then
    
        if test [ -d "$GLPK_INCLUDE" ] ; then
            PKG_CPPFLAGS="-I${GLPK_INCLUDE}"
        else
            AC_MSG_ERROR([directory $GLPK_INCLUDE does not exist])
        fi
    
        if test [ -d "$GLPK_LIB" ] ; then
            PKG_LIBS="-L${GLPK_LIB}"
            # PKG_LIBS="-L${GLPK_LIB} -Wl,-rpath ${GLPK_LIB}"
        else
            AC_MSG_ERROR([directory $GLPK_LIB does not exist])
        fi
    
    else
    
    dnl # if no arguments are given, try to find the glpsol executable. If is not
    dnl # found, use /usr/local and /usr
    
        AC_PREFIX_PROGRAM(glpsol)
    
        if test [ "$prefix" = "NONE" ] ; then
    
            AC_MSG_NOTICE([No prefix given, trying /usr and /usr/local])
    
            if test -d "/usr/include" || test -d "/usr/local/include" ; then
                PKG_CPPFLAGS="-I/usr/include -I/usr/local/include"
            else
                AC_MSG_ERROR([directories /usr/include and /usr/local/include do not exist])
            fi
    
            if test -d "/usr/local/lib" || test -d "/usr/local/lib64" || test -d "/usr/lib" || test -d "/usr/lib64" ; then
                PKG_LIBS=""
                if test [ -d "/usr/local/lib" ] ; then
                    PKG_LIBS="$PKG_LIBS -L/usr/local/lib"
                fi
                if test [ -d "/usr/local/lib64" ] ; then
                    PKG_LIBS="$PKG_LIBS -L/usr/local/lib64"
                fi
                if test [ -d "/usr/lib" ] ; then
                    PKG_LIBS="$PKG_LIBS -L/usr/lib"
                fi
                if test [ -d "/usr/lib64" ] ; then
                    PKG_LIBS="$PKG_LIBS -L/usr/lib64"
                fi
                # PKG_LIBS="-L/usr/local/lib -Wl,-rpath /usr/local/lib"
            else
                AC_MSG_ERROR([none of the directories /usr/local/lib, /usr/local/lib64, /usr/lib of /usr/lib64 exists])
            fi
    
        else
    
            AC_MSG_NOTICE([found executable glpsol in $prefix])
            
            if test [ -d "$prefix/include" ] ; then
                PKG_CPPFLAGS="-I$prefix/include"
            else
                AC_MSG_ERROR([directory $prefix/include does not exist])
            fi
    
            if test -d "$prefix/lib" || test -d "$prefix/lib64" ; then
                PKG_LIBS=""
                if test [ -d "$prefix/lib" ] ; then
                    PKG_LIBS="$PKG_LIBS -L$prefix/lib"
                fi
                if test [ -d "$prefix/lib64" ] ; then
                    PKG_LIBS="$PKG_LIBS -L$prefix/lib64"
                fi
            else
                AC_MSG_ERROR([neither $prefix/lib nor $prefix/lib64 exist])
            fi

dnl #            if test -d "$prefix/include" && test -d "$prefix/lib" ; then
dnl #                PKG_CPPFLAGS="-I$prefix/include"
dnl #                PKG_LIBS="-L$prefix/lib"
dnl #                # PKG_LIBS="-L$prefix/lib -Wl,-rpath $prefix/lib"
dnl #            else
dnl #                AC_MSG_ERROR([directories $prefix/include and $prefix/lib do not exist])
dnl #            fi

        fi
    fi

    if test [ "${GMP}" != "no" ] ; then
        dnl # AC_CHECK_HEADER([gmp.h], INCL="$INCL -lgmp", AC_MSG_NOTICE([Could not link to gmp.]))
        LINK="-lglpk -lgmp"
        AC_DEFINE([USE_GMP], [1], [defined, if gnu bignum is used])
    else
        LINK="-lglpk"
    fi
    
    PKG_LIBS="${PKG_LIBS} ${LINK}"
    ARGS_OK="TRUE"
fi


dnl # check arguments to CPLEX
if test [ "${CHKARGS}" != "no" ] ; then
    AC_DEFINE([CHECK_GLPK_ARGS], [1], [defined, if check arguments to GLPK])
    PKG_CPPFLAGS="-DHAVE_CONFIG_H ${PKG_CPPFLAGS}"
fi


dnl # --------------------------------------------------------------------
dnl # CFLAGS
dnl # --------------------------------------------------------------------

if test [ -z "$PKG_CFLAGS" ] ; then
    if test [ "${R_ARCH}" = "/x86_64" ] ; then
        PKG_CFLAGS="-m64"
    elif test [ "${R_ARCH}" = "/i386" ] ; then
        PKG_CFLAGS="-m32"
    else
        PKG_CFLAGS=""
    fi
fi


dnl # --------------------------------------------------------------------
dnl # check header and library
dnl # --------------------------------------------------------------------

LIBS="${PKG_LIBS}"
CFLAGS="${CFLAGS} ${PKG_CFLAGS}"
CPPFLAGS="${CPPFLAGS} ${PKG_CPPFLAGS}"

AC_CHECK_HEADER([glpk.h],,
    AC_MSG_ERROR([Could not find glpk.h:
      glpkAPI requires GLPK from http://www.gnu.org/software/glpk/
      use --with-glpk-include or GLPK_INCLUDE to specify the include path.]))


AC_SEARCH_LIBS([glp_create_prob], [glpk], ,
    AC_MSG_ERROR([Could not link to GLPK:
      use --with-glpk-lib or GLPK_LIB to specify the lib path.
      Maybe you want to set --enable-gmp=no]))

dnl # GLPK version
AC_CHECK_FUNC([glp_print_ranges], ,
    AC_MSG_ERROR([GLPK version must be >= 4.42.]))


dnl # --------------------------------------------------------------------
dnl # substitute src/Makevars
dnl # --------------------------------------------------------------------

AC_SUBST(PKG_CFLAGS)
AC_SUBST(PKG_LIBS)
AC_SUBST(PKG_CPPFLAGS)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

exit 0
