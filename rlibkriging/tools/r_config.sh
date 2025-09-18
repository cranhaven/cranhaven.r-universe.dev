#### R CONFIGURATION ####

R_ARCH_BIN=$1

CC=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CC`
echo set CC=$CC
export CC

FC=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config FC`
echo set FC=$FC
export FC
export CMAKE_Fortran_COMPILER=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config FC`

CPPFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CPPFLAGS`
CFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CFLAGS`
CPICFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CPICFLAGS`

CFLAGS="$CPPFLAGS $CPICFLAGS $CFLAGS"
echo set CFLAGS=$CFLAGS
export CFLAGS

CXX=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CXX17`
echo set CXX=$CXX
export CXX

CXXSTD=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CXX17STD`
CXXFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CXX17FLAGS`
CXXPICFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CXX17PICFLAGS`

CXXFLAGS="$CXXSTD $CPPFLAGS $CXXPICFLAGS $CXXFLAGS"
echo set CXXFLAGS=$CXXFLAGS
export CXXFLAGS

LDFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config LDFLAGS`
echo set LDFLAGS=$LDFLAGS
export LDFLAGS

if test -z "$CXX"; then
    echo >&2 "Could not detect C++ compiler with R CMD config."
fi

${R_HOME}/bin${R_ARCH_BIN}/R --vanilla -e "getRversion() > '4.0.0'" | grep TRUE > /dev/null
if [ $? -eq 0 ]; then
 	AR=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config AR`
 	AR=`which $AR`

 	RANLIB=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config RANLIB`
 	RANLIB=`which $RANLIB`
fi

