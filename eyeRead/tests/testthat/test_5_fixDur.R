context( "fixDur" )

test_that( "specFile is found", {
  expect_true( T )
} )

test_that( "Function returns the correct error when data is no data frame", {
  expect_error( fixDur( data = some_Data$single_AOI_col$AOI,
                        fixTime = "fixTime",
                        passes = "passes" ),
                regexp = some_errors$no_data.frame_err )
} )

test_that( "Function returns the correct error when fixTime is not in data", {
  expect_error( fixDur( data = some_Data$single_AOI_col,
                        fixTime = "ABC",
                        passes = "passes" ),
                regexp = some_errors$missing_colname_err$fixTime )
} )

test_that( "Function returns the correct error when passes in not in data", {
  expect_error( fixDur( data = some_Data$single_AOI_col,
                        fixTime = "fixTime",
                        passes = "ABC" ),
                regexp = some_errors$missing_colname_err$passes )
} )

test_that( "Function returns the correct error when AOI specifies a colname that is not in data", {
  expect_error(
    fixDur( data = some_Data$single_AOI_col,
            fixTime = "fixTime",
            passes = "passes",
            AOI = "ABC" ),
    regexp = some_errors$missing_colname_err$AOI$single
  )
} )

test_that( "Function returns the correct error when fixTime contains more than one element", {
  expect_error( fixDur( data = some_Data$single_AOI_col,
                        fixTime = c( "fixTime", "A", "B"),
                        passes = "passes" ),
                regexp = some_errors$missing_colname_err$fixTimeVec )
} )

test_that( "Function returns the correct error when passes contains more than one element", {
  expect_error( fixDur( data = some_Data$single_AOI_col,
                        fixTime = "fixTime",
                        passes = c( "passes", "A", "B" ) ),
                regexp = some_errors$missing_colname_err$passesVec )
} )

test_that( "Function correctly calculates fixation duration for first pass and second pass [column name]", {
  expect_equal( fixDur( data = some_Data$single_AOI_col,
                        fixTime = "fixTime",
                        passes = "passes" ),
                some_results$fixDur$firstSecondPass
  )
} )

test_that( "Function correctly calculates fixation duration for first pass and second pass [fixTime column number]", {
  expect_equal( fixDur( data = some_Data$single_AOI_col,
                        fixTime = 3,
                        passes = "passes" ),
                some_results$fixDur$firstSecondPass
                )
} )

test_that( "Function correctly calculates fixation duration for first pass and second pass [passes column number]", {
  expect_equal( fixDur( data = some_Data$single_AOI_col,
                        fixTime = "fixTime",
                        passes = 4 ),
                some_results$fixDur$firstSecondPass
                               )
} )

test_that( "Function correctly calculates fixation duration for first pass forward, rereading and second pass", {
  expect_equal( fixDur( data = some_Data$rereading$topLeft,
                        fixTime = "fixTime",
                        passes = "passes" ),
                some_results$fixDur$rereading
  )
} )

test_that( "Function correctly calculates fixation duration full with AOIs unfixed [factor]", {
  Dat <- some_Data$rereading$unFixed
  
  Dat$AOI <- factor( Dat$AOI, levels = c( "AOI1", "AOI2", "AOI3", "AOI4" )
  )
  
  expect_equal( fixDur( data = Dat,
                        fixTime = "fixTime",
                        passes = "passes",
                        AOI = "AOI" ),
                some_results$fixDur$unFixed
  )
} )

test_that( "Function correctly calculates fixation duration full with AOIs unfixed [Character]", {
  target <-  some_results$fixDur$unFixed
  target <- target[ -which( target$AOI == "AOI4" ), ]
  
  expect_equal( fixDur( data = some_Data$rereading$unFixed,
                        fixTime = "fixTime",
                        passes = "passes",
                        AOI = "AOI" ),
               target )
} )

test_that( "Function correctly calculates fixation duration full with AOIs unfixed [numeric]", {
  Dat <- some_Data$rereading$unFixed

  Dat$AOI <- dplyr::recode( Dat$AOI, AOI1 = 1, AOI2 = 2, AOI3 = 3 )
  
  splitted_pass <- transpose( 
                     as.data.frame( 
                      strsplit( Dat$passes, split = "_", fixed = T )
                     )
                    )
  
  splitted_pass[ , 2 ] <- dplyr::recode( splitted_pass[ , 2 ], AOI1 = 1, AOI2 = 2, AOI3 = 3 )
  
  Dat$passes <- paste( splitted_pass[ , 1 ], splitted_pass[ , 2 ], sep = "_" )
  
  target <- some_results$fixDur$unFixed
  target <- target[ -which( target$AOI == "AOI4" ), ]
  
  target$AOI <- as.character(
                  dplyr::recode( target$AOI, AOI1 = 1, AOI2 = 2, AOI3 = 3 )
                )
  
  expect_equal( fixDur( data = Dat,
                        fixTime = "fixTime",
                        passes = "passes",
                        AOI = "AOI" ),
                target
  )
} )

test_that( "Function correctly calculates fixation duration full with AOIs unfixed [column number]", {
  target <-  some_results$fixDur$unFixed
  target <- target[ -which( target$AOI == "AOI4" ), ]
  
  expect_equal( fixDur( data = some_Data$rereading$unFixed,
                        fixTime = "fixTime",
                        passes = "passes",
                        AOI = 2 ),
                target )
} )

test_that( "Function correctly calculates fixation duration with fixation outside AOI", {
  expect_equal( fixDur( data = some_Data$rereading$outFix,
                        fixTime = "fixTime",
                        passes = "passes" ),
                some_results$fixDur$outFix
  )
} )

test_that( "Function correctly calculates fixation duration for AOI's", {
  expect_equal( fixDur( data = some_Data$single_AOI_col,
                        fixTime = "fixTime",
                        passes = "AOI"),
                some_results$fixDur$AOI
  )
} )

test_that( "testing the workaround for tibble anomalies", {
  theIn <- tibble::as_tibble( some_Data$single_AOI_col )
  expect_equal( fixDur( data = theIn,
                        fixTime = "fixTime",
                        passes = "passes" ),
                some_results$fixDur$firstSecondPass
  )
} )
