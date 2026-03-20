# Run tests for fdars package
# Limit Rust parallelism to avoid CRAN CPU time NOTE
Sys.setenv(RAYON_NUM_THREADS = "2")

library(testthat)
library(fdars)

test_check("fdars")
