context( "codePasses" )

test_that( "specFile is found", {
  expect_true( T )
} )

test_that( "Function returns the correct error when data is no data frame", {
  expect_error(
    codePasses( data = some_Data$single_AOI_col$AOI, AOI = "AOI"),
    regexp = some_errors$no_data.frame_err
  )
} )

test_that( "Function returns the correct error when AOI specifies a colname that is not in data [single AOI]", {
  expect_error(
    codePasses( data = some_Data$single_AOI_col, AOI = "ABC"),
    regexp = some_errors$missing_colname_err$AOI$single
  )
} )

test_that( "Function returns the correct error when AOI specifies a colname that is not in data [multiple AOI]", {
  expect_error(
    codePasses( data = some_Data$single_AOI_col,
                AOI = c( "AOI1", "AOI2", "AOI3", "ABC", "AOI5", "AOI6", "AOI7" ) ),
    regexp = some_errors$missing_colname_err$AOI$multiple
  )
} )

test_that( "Function returns the correct error when fpx specifies a colname that is not in data", {
  expect_error(
    codePasses( data = some_Data$rereading$topLeft,
                AOI = "AOI",
                rereading = T,
                fpx = "ABC",
                fpy = "ycoord" ),
    regexp = some_errors$missing_colname_err$fpx
  )
} )

test_that( "Function returns the correct error when fpy specifies a colname that is not in data", {
  expect_error(
    codePasses( data = some_Data$rereading$topLeft,
                AOI = "AOI",
                rereading = T,
                fpx = "xcoord",
                fpy = "ABC" ),
    regexp = some_errors$missing_colname_err$fpy
  )
} )

test_that( "Function returns the correct error when fpx is non numeric", {
  someTestData <- some_Data$rereading$topLeft
  someTestData$xcoord <- as.character( someTestData$xcoord )
  expect_error(
    codePasses( data = someTestData,
                AOI = "AOI",
                rereading = T,
                fpx = "xcoord",
                fpy = "ycoord" ),
    regexp = some_errors$coords_nonum$fpx
  )
  rm( someTestData )
} )

test_that( "Function returns the correct error when fpy is non numeric", {
  someTestData <- some_Data$rereading$topLeft
  someTestData$ycoord <- as.character( someTestData$ycoord )
  expect_error(
    codePasses( data = someTestData,
                AOI = "AOI",
                rereading = T,
                fpx = "xcoord",
                fpy = "ycoord" ),
    regexp = some_errors$coords_nonum$fpy
  )
  rm( someTestData )
} )

test_that( "Function returns the correct errors when fix_min is to small", {
  expect_error(
    codePasses( data = some_Data$single_AOI_col, AOI = "AOI",
                fix_min = 0 ),
    regexp = some_errors$fix_min_err
  )
  expect_error(
    codePasses( data = some_Data$single_AOI_col, AOI = "AOI",
                fix_min = -1 ),
    regexp = some_errors$fix_min_err
  )
} )

test_that( "Function returns correct error if rereading is TRUE but fpx is not supplied", {
  expect_error(
    codePasses( data = some_Data$single_AOI_col,
                AOI = "AOI", rereading = T ),
    regexp = some_errors$missing_fpx_err
  )
} )

test_that( "Function returns correct error if rereading is TRUE but fpy is not supplied", {
  expect_error(
    codePasses( data = some_Data$rereading$topLeft,
                AOI = "AOI", rereading = T, fpx = "xcoord" ),
    regexp = some_errors$missing_fpy_err
  )
} )

test_that( "Function correctly codes fixations as first pass and second pass with a single AOI column [column name]", {
  expect_equal(
    codePasses( data = some_Data$single_AOI_col, AOI = "AOI" ),
    some_results$regular
  )
} )

test_that( "Function correctly codes fixations as first pass and second pass with a single AOI column [column number]", {
  expect_equal(
    codePasses( data = some_Data$single_AOI_col, AOI = 2 ),
    some_results$regular
  )
} )

test_that( "Function correctly codes fixations as first pass and second pass with a single AOI column and non-AOI fixation [column name]", {
  expect_equal(
    codePasses( data = some_Data$single_AOI_zeros, AOI = "AOI" ),
    some_results$zeros
  )
} )

test_that( "Function correctly codes fixations as first pass and second pass with a single AOI column and AOI column is factor [column name]", {
  dataFactor <- some_Data$single_AOI_col
  dataFactor$AOI <- factor( dataFactor$AOI )
  expect_equal(
    codePasses( data = dataFactor, AOI = "AOI" ),
    some_results$regular
  )
} )

test_that( "Function correctly codes fixations as first pass and second pass with a single AOI column and non-AOI fixation and AOI column is factor [column name]", {
  dataFactor <- some_Data$single_AOI_zeros
  dataFactor$AOI <- factor( dataFactor$AOI )
  results <- codePasses( data = dataFactor, AOI = "AOI" )
  expect_equal(
    results,
    some_results$zeros
  )
} )

test_that( "Function correctly codes fixations as first pass and second pass with multiple AOI columns [column name]", {
  result <- codePasses( data = some_Data$multiple_AOI_col,
                        AOI = c( "AOI1", "AOI2", "AOI3", "AOI4", "AOI5", "AOI6", "AOI7" ) )
  expect_equal(
    result,
    some_results$regular
  )
} )

test_that( "Function correctly codes fixations as first pass and second pass with multiple AOI column [column number]", {
  expect_equal(
    codePasses( data = some_Data$multiple_AOI_col, AOI = 2:8 ),
    some_results$regular
  )
} )

test_that( "Function correctly codes fixations as first pass and second pass with multiple AOI columns and non-AOI fixation [column name]", {
  expect_equal(
    codePasses( data = some_Data$multiple_AOI_col,
                AOI = c( "AOI1", "AOI2",  "AOI4", "AOI5", "AOI6", "AOI7" ) ),
    some_results$zeros
  )
} )

test_that( "Function correctly codes fixations as first pass and second pass with different fix_min", {
  expect_equal(
    codePasses( data = some_Data$single_AOI_col, AOI = "AOI", fix_min = 1 ),
    some_results$fix_min
  )
} )

test_that( "Function correctly codes fixations as forward and rereading first pass; origin topLeft [by default; column name]", {
  expect_equal(
    codePasses( data = some_Data$rereading$topLeft,
                AOI = "AOI",
                rereading = T,
                fpx = "xcoord",
                fpy = "ycoord",
                fix_size = 20 ),
    some_results$rereading
  )
} )

test_that( "Function correctly codes fixations as forward and rereading first pass and non-AOI fixation; origin topLeft [by default; column name]", {
  expect_equal(
    codePasses( data = some_Data$rereading$zeros,
                AOI =  c( "AOI1", "AOI3" ),
                rereading = T,
                fpx = "xcoord",
                fpy = "ycoord",
                fix_size = 20 ),
    some_results$rereading_zeros
  )
} )

test_that( "Function correctly codes fixations as forward and rereading first pass; origin topLeft [column name]", {
  expect_equal(
    codePasses( data = some_Data$rereading$topLeft,
                AOI = "AOI",
                rereading = T,
                fpx = "xcoord",
                fpy = "ycoord",
                origin = "topLeft",
                fix_size = 20 ),
    some_results$rereading
  )
} )

test_that( "Function correctly codes fixations as forward and rereading first pass; origin topLeft [column number]", {
  expect_equal(
    codePasses( data = some_Data$rereading$topLeft,
                AOI = "AOI",
                rereading = T,
                fpx = 3,
                fpy = 4,
                origin = "topLeft",
                fix_size = 20 ),
    some_results$rereading
  )
} )

test_that( "Function correctly codes fixations as forward and rereading first pass; origin bottomLeft [column name]", {
  expect_equal(
    codePasses( data = some_Data$rereading$bottomLeft,
                AOI = "AOI",
                rereading = T,
                fpx = "xcoord",
                fpy = "ycoord",
                origin = "bottomLeft",
                fix_size = 20 ),
    some_results$rereading
  )
} )

test_that( "Function correctly codes fixations as forward and rereading first pass; origin topRight [column name]", {
  expect_equal(
    codePasses( data = some_Data$rereading$topRight,
                AOI = "AOI",
                rereading = T,
                fpx = "xcoord",
                fpy = "ycoord",
                origin = "topRight",
                fix_size = 20 ),
    some_results$rereading
  )
} )

test_that( "Function correctly codes fixations as forward and rereading first pass; origin bottomRight [column name]", {
  expect_equal(
    codePasses( data = some_Data$rereading$bottomRight,
                AOI = "AOI",
                rereading = T,
                fpx = "xcoord",
                fpy = "ycoord",
                origin = "bottomRight",
                fix_size = 20 ),
    some_results$rereading
  )
} )

test_that( "Function correctly codes fixations as forward and rereading first pass; origin center [column name]", {
  expect_equal(
    codePasses( data = some_Data$rereading$center,
                AOI = "AOI",
                rereading = T,
                fpx = "xcoord",
                fpy = "ycoord",
                origin = "center",
                fix_size = 10 ),
    some_results$rereading
  )
} )

test_that( "testing the workaround for tibble anomalies", {
  theIn <- tibble::as_tibble( some_Data$single_AOI_col )
  expect_equal(
    codePasses( data = theIn, AOI = "AOI" ),
    some_results$regular
  )
} )