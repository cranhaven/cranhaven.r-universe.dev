library(testthat)
library(SimSurvNMarker)

test_res_dir <- if(!dir.exists("test-res"))
  file.path("tests", "testthat", "test-res") else
    "test-res"

formals(expect_known_value)$update <- formals(expect_known_output)$update <-
  FALSE
