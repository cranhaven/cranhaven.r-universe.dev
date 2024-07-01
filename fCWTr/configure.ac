AC_INIT([fcwtr],[0.2.0])



# Find the compiler and compiler flags used by R.
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS="`"${R_HOME}/bin/R" CMD config CPPFLAGS`"
LDFLAGS="`"${R_HOME}/bin/R" CMD config LDFLAGS`"
AC_LANG(C++)
AC_PROG_CPP



# extra check for macos brew
AC_CHECK_PROG(BREW,brew,yes)
if test "x${BREW}" = xyes; then
  # in order for AC_CHECK_HEADER to work
  CPPFLAGS="${CPPFLAGS} -I`brew --prefix`/include"
  PKG_CPPFLAGS="-I`brew --prefix`/include"

  LDFLAGS="${LDFLAGS} -L`brew --prefix`/lib"
  PKG_LDFLAGS="-L`brew --prefix`/lib"
fi


# Check for fftw3.h header
AC_CHECK_HEADER([fftw3.h], [
  AC_DEFINE([HAVE_FFTW3_H], [1], [Define if you have the fftw3.h header file])
], [
  AC_MSG_ERROR([fftw3.h header file not found in: $CPPFLAGS])
])

have_fftw=no
AC_SEARCH_LIBS(
  fftwf_free,
  fftw3f,
  [have_fftw=yes],
  [AC_MSG_ERROR([The fftw3 library is required.])]
)

# Check for omp.h header
AC_CHECK_HEADER([omp.h], [
  AC_DEFINE([HAVE_OMP_H], [1], [Define if you have the omp.h header file])
], [
  AC_DEFINE([HAVE_OMP_H], [0], [Define if you have the omp.h header file])
])

have_fftw_omp=no
AC_SEARCH_LIBS(
  fftwf_plan_with_nthreads,
  fftw3f_omp,
  [have_fftw_omp=yes],
  [AC_MSG_RESULT([The fftw3-omp support not found.])]
)

AC_OPENMP

# Write the flags into the src/Makevars file.
AC_SUBST([PKG_CPPFLAGS], ["${PKG_CPPFLAGS} ${FFTW_CPPFLAGS} ${FFTW_OMP_CPPFLAGS} ${OPENMP_CPPFLAGS}"])
AC_SUBST([PKG_CXXFLAGS], ["${PKG_CXXFLAGS} ${FFTW_CXXFLAGS} ${FFTW_OMP_CXXFLAGS} ${OPENMP_CXXFLAGS}"])
AC_SUBST([PKG_LDFLAGS], ["${PKG_LDFLAGS} ${OPENMP_CXXFLAGS}"])
AC_SUBST([PKG_LIBS], ["${LIBS} ${PKG_LIBS} ${FFTW_LIBS} ${FFTW_OMP_LIBS}"])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
  --------------------------------------------------
  Configuration for ${PACKAGE_NAME} ${PACKAGE_VERSION}

    cppflags: ${PKG_CPPFLAGS}
    cxxflags: ${PKG_CXXFLAGS}
    ldflags:  ${PKG_LDFLAGS}
    libs:     ${PKG_LIBS}

  --------------------------------------------------
"
