context( "AOItransitions" )

test_that( "specFile is found", {
  expect_true( T )
} )

test_that( "Function returns the correct error when AOI is contains less than two elements", {
  expect_error(
    AOItransitions( AOI = c(1)),
    regexp = some_errors$AOI_short
  )
  expect_error(
    AOItransitions( AOI = character()),
    regexp = some_errors$AOI_short
  )
} )

test_that( "Function returns the correct results", {
  results <- AOItransitions( some_Data$single_AOI_col$AOI )
  expect_equal( results, some_results$transitions )
} )
