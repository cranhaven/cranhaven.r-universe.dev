AC_INIT([BRugs],[0.8])

## TODO only runs on x86 Linux - give error on other unixes.  

AC_PREFIX_DEFAULT(/usr/local)
AC_PREFIX_PROGRAM([OpenBUGS])

AC_ARG_WITH([openbugs],
	AS_HELP_STRING([--with-openbugs=PATH],[the location of the OpenBUGS installation, by default /usr/local]),
            [openbugs_path=$withval])

if test [ -n "$openbugs_path" ] ; then
   OPENBUGS=${openbugs_path}
else
   if test [ "$prefix" = "NONE" ]; then 
      AC_MSG_ERROR( [OpenBUGS 3.2.2 or later not found. Install it, or specify its location using, for example, R CMD INSTALL BRugs --configure-args='--with-openbugs=/usr/local'] )
   else
      if test [ -e ${prefix}/lib/libOpenBUGS.so ] ; then
            OPENBUGS=${prefix}
      elif test [ -e ${prefix}/lib/OpenBUGS/lib/libOpenBUGS.so ] ; then # for 3.2.1
            OPENBUGS=${prefix}	    
      fi
   fi
fi

VERSION=`echo "modelQuit()" | ${OPENBUGS}/bin/OpenBUGS | sed -ne "s/OpenBUGS version \(.*\) rev \(.*\)/\1/p"`
if test [`echo ${VERSION} |cut -d. -f1` -lt 3]; then
 AC_MSG_ERROR([Requires OpenBUGS version 3.2.2 or greater])
fi
if test [ `echo ${VERSION} |cut -d. -f1` -eq 3  -a  `echo ${VERSION} |cut -d. -f2` -lt 2 ]; then
 AC_MSG_ERROR([Requires OpenBUGS version 3.2.2 or greater])
fi
if test [ `echo ${VERSION} |cut -d. -f1` -eq 3  -a  `echo ${VERSION} |cut -d. -f2` -eq 2   -a  `echo ${VERSION} |cut -d. -f3` -lt 2 ]; then
 AC_MSG_ERROR([Requires OpenBUGS version 3.2.2 or greater])
fi

OLDFLAGS=${LDFLAGS}
LDFLAGS="-L${OPENBUGS}/lib -m32"
AC_CHECK_LIB(OpenBUGS, CmdInterpreter, [],
             [AC_MSG_ERROR("Cannot load OpenBUGS library in ${OPENBUGS}/lib. Run R CMD INSTALL BRugs --configure-args='--with-openbugs=...'")])
LDFLAGS=${OLDFLAGS}

if test [ -d "${OPENBUGS}/share/doc/openbugs-${VERSION}" ] ; then
   OPENBUGSDOC=${OPENBUGS}/share/doc/openbugs-${VERSION}
else
  if test [ -d "${OPENBUGS}/share/doc/openbugs" ] ; then
   OPENBUGSDOC=${OPENBUGS}/share/doc/openbugs
  else
   AC_MSG_ERROR([OpenBUGS documentation directory not found in ${OPENBUGS}/share/doc])
  fi
fi

OPENBUGS=${OPENBUGS}/lib
DYNLIBEXT="so"

AC_SUBST(OPENBUGS)
AC_SUBST(OPENBUGSDOC)
AC_SUBST(DYNLIBEXT)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
AC_CONFIG_FILES([R/unix/zzz.R])
AC_OUTPUT
