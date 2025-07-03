library(testthat)
library(MAGEE)
Sys.setenv(MKL_NUM_THREADS = 1)

test_check("MAGEE")
