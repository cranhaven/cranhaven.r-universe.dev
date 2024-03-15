library(testthat)
library(misha)

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    test_check("misha")
}
