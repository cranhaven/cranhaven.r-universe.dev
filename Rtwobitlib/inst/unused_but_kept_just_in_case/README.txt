We're not supporting the twoBitOpenExternalBptIndex functionality at the
moment. Note that supporting it would require to link the package to libcrypto
because the code in kent-core-463/src/lib/udc.c calls SHA1() from the crypto
library in openssl. See Rtwobitlib/src/kent/NOTES.txt for more information.

In addition to the steps described in Rtwobitlib/src/kent/NOTES.txt,
supporting twoBitOpenExternalBptIndex would require restoring the
configure.ac, configure, and INSTALL files, and to modify the following
files:
    Rtwobitlib/src/Makevars
    Rtwobitlib/src/Makevars.win
    Rtwobitlib/src/kent/Makefile.Rtwobitlib
    Rtwobitlib/src/kent/Makefile.Rtwobitlib.win

Finally, the SystemRequirements field in DESCRIPTION would need to be
updated to:

SystemRequirements: GNU make; openssl & header files

