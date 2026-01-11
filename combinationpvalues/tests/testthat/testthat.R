Sys.setenv("R_TESTS" = "")
library(testthat)
library(combinationpvalues)

#testthat::test_dir('/Users/breyawalker/Desktop/Thesis2021/repo/Master2021/R/combinationpvalues/tests/testthat')
test_check("combinationpvalues")
