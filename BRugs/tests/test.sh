#!/bin/sh

# Run package tests which can't be run by R CMD check since OpenBUGS
# library is not on CRAN

setlocal() { 
 echo "test <- $1" > local.R
}

# Reset local flag when Ctrl-C is pressed
trap 'setlocal FALSE; exit 0' 2

setlocal TRUE

for i in *.R ; do
    echo "Running tests in $i..."
    R CMD BATCH --no-save $i
done

setlocal FALSE
