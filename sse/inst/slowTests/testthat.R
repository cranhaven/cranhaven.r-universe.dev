library(testthat)
library(sse)

test_check("sse")


## some conventions:

## if expect_error is used, separat lines allow to easily read and check the error massage manually:
## e.g.:
## expect_error(
##     powPar(n = seq(from = 20, to = 60, by = 2))  ## READ ERROR MESSAGE
## )
