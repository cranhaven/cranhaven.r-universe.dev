# We will need this only if we decide to support the twoBitOpenExternalBptIndex
# functionality. See README.txt in this folder and
# Rtwobitlib/src/kent/NOTES.txt for more information.

# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65]) 

AC_INIT([Rtwobitlib])

[ERRORMSG="

You need openssl and its header files in order to install Rtwobitlib from
source. Please refer to the INSTALL file included in the package for how
to install openssl & header files on your system.
"]

AC_CHECK_HEADER([openssl/sha.h], ,
AC_MSG_ERROR([openssl header files not found! $ERRORMSG])
)

AC_CHECK_LIB([crypto], SHA1, ,
AC_MSG_ERROR([libcrypto (part of openssl) not found! $ERRORMSG ])
)

AC_OUTPUT
