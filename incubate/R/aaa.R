# mkuhn, 2025-04-11
# globals defined in the package space
# to be read in first

the <- rlang::new_environment() # package environment
TOL_NUM <- sqrt(.Machine$double.eps)
DELAY_MIN <- .Machine$double.eps #.Machine$double.xmin even smaller ##1e-9
