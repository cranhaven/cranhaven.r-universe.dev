#!/usr/bin/env bash
set -eo pipefail

if [[ "$DEBUG_CI" == "true" ]]; then
  set -x
fi


# Setup used/unused bindings
export ENABLE_R_BINDING=ON
export ENABLE_OCTAVE_BINDING=OFF
export ENABLE_MATLAB_BINDING=OFF
export ENABLE_PYTHON_BINDING=OFF

export MAKE_SHARED_LIBS=off
export STATIC_LIB=on

: ${R_HOME=$(R RHOME)}
if test -z "${R_HOME}"; then
   as_fn_error $? "Could not determine R_HOME." "$LINENO" 5
fi

# Static libKriging build (using libK/.ci)
cd src/libK
CI=`ls -a | grep travis-ci`
echo $CI

{
$CI/common/before_script.sh
} || {
echo "!!! Failed checking configuration !!!"
}

export CC=`${R_HOME}/bin/R CMD config CC`
export CXX=`${R_HOME}/bin/R CMD config CXX`
export FC=`${R_HOME}/bin/R CMD config FC`

# R workflow requires to use R cmd with full path.
# These declarations help to skip declaration without full path in libKriging build scripts.
export CMAKE_Fortran_COMPILER="$(${R_HOME}/bin/R CMD config FC | awk '{ print $1 }')"
export Fortran_LINK_FLAGS="$(${R_HOME}/bin/R CMD config FLIBS)"

BUILD_TEST=false \
MODE=Release \
EXTRA_CMAKE_OPTIONS="${EXTRA_CMAKE_OPTIONS:-} -DCMAKE_INSTALL_LIBDIR=lib -DBUILD_SHARED_LIBS=${MAKE_SHARED_LIBS} -DSTATIC_LIB=${STATIC_LIB} -DEXTRA_SYSTEM_LIBRARY_PATH=${EXTRA_SYSTEM_LIBRARY_PATH}" \
$CI/linux-macos/build.sh # should support '.travis-ci' or 'travis-ci'"

rm -rf ../../inst
mkdir -p ../../inst
mv build/installed/lib ../../inst/.
mv build/installed/share ../../inst/.
mv build/installed/include ../../inst/.

cd ../..

# update doc
#R -e "roxygen2::roxygenise(package.dir = '.')" # No: it will loop on install, because roxygen2 requires loading package...
# update Rccp links
${R_HOME}/bin/R -e "Rcpp::compileAttributes(pkgdir = '.', verbose = TRUE)"
