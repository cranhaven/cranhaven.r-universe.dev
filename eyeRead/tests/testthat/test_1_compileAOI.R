context( "compileAOI" )

test_that( "specFile is found", {
  expect_true( T )
} )

test_that( "Function returns the correct error when data is no data frame", {
  expect_error(
    compileAOI( data = some_Data$multiple_AOI_col$AOI, AOI = "AOI"),
    regexp = some_errors$no_data.frame_err
  )
} )

test_that( "Function returns the correct error when AOI specifies a colname that is not in data", {
  expect_error(
    compileAOI( data = some_Data$multiple_AOI_col,
                AOI = "ABC" ),
    regexp = some_errors$missing_colname_err$AOI$multiple
  )
} )

test_that( "Function returns the correct error when AOI and labels have different lengths", {
  expect_error(
    compileAOI( data = some_Data$multiple_AOI_col,
                AOI = c( "AOI1", "AOI2", "AOI3", "AOI4", "AOI5", "AOI6", "AOI7" ),
                labels = c( "AOI1", "AOI2", "AOI3", "AOI4", "AOI5", "AOI6" ) ),
    regexp = some_errors$wrong_label_length
  )
} )

test_that( "Function returns the correct warning when more than one AOI fixated", {
  expect_warning(
    compileAOI( data = some_Data$two_AOI_Fix,
                AOI = c( "AOI1", "AOI2", "AOI3", "AOI4", "AOI5", "AOI6", "AOI7" )),
    regexp = some_warning$more_fixations
  )
} )

test_that( "Function returns the correct results, if AOI are names", {
  results <- compileAOI( data = some_Data$multiple_AOI_col,
                         AOI = c( "AOI1", "AOI2", "AOI3", "AOI4", "AOI5", "AOI6",
                                  "AOI7" ) )
  target <- factor( some_Data$single_AOI_col$AOI, levels = c( "AOI1", "AOI2",
                                                              "AOI3", "AOI4", 
                                                              "AOI5", "AOI6",
                                                              "AOI7" ) )
  expect_equal( compileAOI( data = some_Data$multiple_AOI_col,
                            AOI = c( "AOI1", "AOI2", "AOI3", "AOI4", "AOI5",
                                     "AOI6", "AOI7" ) ), 
                target )
} )

test_that( "Function returns the correct results, if AOI are numbers", {
  results <- compileAOI( data = some_Data$multiple_AOI_col,
                         AOI = 2:8 )
  target <- factor( some_Data$single_AOI_col$AOI, levels = c( "AOI1", "AOI2",
                                                              "AOI3", "AOI4", 
                                                              "AOI5", "AOI6",
                                                              "AOI7" ) )
  expect_equal( compileAOI( data = some_Data$multiple_AOI_col,
                            AOI = 2:8 ), target )
} )

test_that( "Function returns the correct results, if AOI are names and some fixations outside AOI", {
  results <- compileAOI( data = some_Data$multiple_AOI_col_out,
                         AOI = c( "AOI1", "AOI2", "AOI3", "AOI4", "AOI5", "AOI6",
                                  "AOI7" ) )
  target <- factor( some_results$compile_missing, levels = c( "0", "AOI1", "AOI2",
                                                              "AOI3", "AOI4", 
                                                              "AOI5", "AOI6",
                                                              "AOI7" ) )
  expect_equal( results, target )
} )

test_that( "Function returns the correct results, if AOI are names and with labels provided", {
  results <- compileAOI( data = some_Data$multiple_AOI_col,
                         AOI = c( "AOI1", "AOI2", "AOI3", "AOI4", "AOI5", "AOI6",
                                  "AOI7" ),
                         labels = c( "1", "2", "3", "4", "5", "6", "7" ) )
  target <- factor( some_results$compile_labels, levels = c( "1", "2", "3", "4",
                                                             "5", "6", "7" ) )
  expect_equal( results, target )
} )

test_that( "Function returns the correct results, if AOI are numbers and with labels provided", {
  results <- compileAOI( data = some_Data$multiple_AOI_col,
                         AOI = 2:8,
                         labels = c( "1", "2", "3", "4", "5", "6", "7" ) )
  target <- factor( some_results$compile_labels, levels = c( "1", "2", "3", "4",
                                                             "5", "6", "7" ) )
  expect_equal( results, target )
} )
