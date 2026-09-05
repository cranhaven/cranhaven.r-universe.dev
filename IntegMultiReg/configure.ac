AC_PREREQ([2.69])
AC_INIT([IntegMultiReg], [0.1.1])
AC_CONFIG_SRCDIR([DESCRIPTION])

## Use gsl-config to find arguments for compiler and linker flags
##
## Check for non-standard programs: gsl-config(1)
AC_PATH_PROG([GSL_CONFIG], [gsl-config])
## If gsl-config was found, let's use it
if test "${GSL_CONFIG}" != ""; then
    # Use gsl-config for header and linker arguments, including GSL's CBLAS dependency.
    GSL_CFLAGS=`${GSL_CONFIG} --cflags`
    GSL_LIBS=`${GSL_CONFIG} --libs`
else
    AC_MSG_ERROR([gsl-config not found, is GSL installed?])
fi

## Use Rscript to query Rcpp for compiler and linker flags
## link flag providing libary as well as path to library, and optionally rpath
##RCPP_LDFLAGS=`${R_HOME}/bin/Rscript -e 'Rcpp:::LdFlags()'`

# Now substitute these variables in src/Makevars.in to create src/Makevars
AC_SUBST(GSL_CFLAGS)
AC_SUBST(GSL_LIBS)
##AC_SUBST(RCPP_LDFLAGS)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
