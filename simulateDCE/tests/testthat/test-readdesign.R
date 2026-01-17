design_path <- system.file("extdata", "agora", "altscf_eff.ngd", package = "simulateDCE")


test_that("wrong designtype", {
  expect_error(readdesign(design = design_path, designtype = "ng"), "Invalid value for design. Please provide either NULL, 'ngene', 'spdesign'or 'idefix',  or do not use the argument 'designtype'. NULL lets us to guess the design.")
})


test_that("file does not exist", {
  expect_error(
    readdesign(design = system.file("data-raw/agora/alcf_eff.ngd", package = "simulateDCE"), designtype = "ngene"),
    "does not exist in current working directory"
  )
})

test_that("all is correct", {
  expect_no_error(readdesign(design = design_path, designtype = "ngene"))
})

# test if autodetect ngd design

test_that("expect message of guess", {
  expect_message(readdesign(design = design_path), "I guessed it is an ngene file")
})


test_that("with or without autodetct get same results for ngene", {
  t <- readdesign(design_path)
  t2 <- readdesign(design_path, designtype = "ngene")

  expect_equal(t, t2)
})


### Tests for spdesign

design_path <- system.file("extdata", "CSA", "linear", "BLIbay.RDS", package = "simulateDCE")

test_that("all is correct", {
  expect_no_error(readdesign(design = design_path, designtype = "spdesign"))
})






# Same Tests for spdesign, but detect automatically if it is spdesign

test_that("prints message for guessing", {
  expect_message(readdesign(design = design_path), "I assume it is a spdesign")
})

test_that("with or without autodetct get same results for spdesign", {
  t <- readdesign(design_path)
  t2 <- readdesign(design_path, designtype = "spdesign")

  expect_equal(t, t2)
})


## trying objects that do not work

design_path <- system.file("extdata", "testfiles", "nousefullist.RDS", package = "simulateDCE")

test_that("spdesign object is a list but does not contain the right element design", {
  expect_error(
    readdesign(design = design_path, designtype = "spdesign"),
    "list element is missing. Make sure to provide a "
  )
})


## test spdesign object containing original object

design_path <- system.file("extdata", "ValuGaps", "des1.RDS", package = "simulateDCE")

test_that("all is correct with full spdesign objects", {
  expect_no_error(readdesign(design = design_path, designtype = "spdesign"))
})



### Tests for idefix

design_idefix <- system.file("extdata", "Idefix_designs", "test_design2.RDS", package = "simulateDCE")



test_that("all is correct with full idefix objects", {
  expect_no_error(readdesign(design_idefix, designtype = "idefix"))
})

#### new tests with kind help from chatgpt


library(testthat)
library(simulateDCE)

test_that("readdesign correctly identifies and processes ngene files", {
  # Arrange
  design_ngene <- system.file("extdata", "agora", "altscf_eff.ngd", package = "simulateDCE")

  # Act
  result <- readdesign(design_ngene)

  # Assert
  expect_s3_class(result, "data.frame") # Check the result is a data frame
  expect_true("Choice.situation" %in% colnames(result)) # Verify key column presence

  # Ensure there are alternative-related columns
  alt_columns <- colnames(result)[grepl("^alt", colnames(result))]
  expect_gt(length(alt_columns), 0) # Confirm at least one alt-related column exists

  # Validate the structure of the output
  expect_gt(nrow(result), 0) # Ensure the data frame has rows
  expect_gt(ncol(result), 1) # Ensure the data frame has more than one column
  expect_message(readdesign(design_ngene), "I guessed it is an ngene file")
})

test_that("readdesign correctly identifies and processes spdesign files", {
  # Arrange
  design_sp <- system.file("extdata", "ValuGaps", "des1.RDS", package = "simulateDCE")

  # Act
  result_default <- readdesign(design_sp)
  result_explicit <- readdesign(design_sp, designtype = "spdesign")

  # Assert
  expect_s3_class(result_default, "data.frame") # Check the result is a data frame
  expect_s3_class(result_explicit, "data.frame")
  expect_identical(result_default, result_explicit) # Default and explicit designtype should match
  expect_true("Choice.situation" %in% colnames(result_default)) # Verify key column presence

  # Ensure there are alternative-related columns
  alt_columns <- colnames(result_default)[grepl("^alt", colnames(result_default))]
  expect_gt(length(alt_columns), 0) # Confirm at least one alt-related column exists

  # Validate the structure of the output
  expect_gt(nrow(result_default), 0) # Ensure the data frame has rows
  expect_gt(ncol(result_default), 1) # Ensure the data frame has more than one column
  expect_message(readdesign(design_sp), "I assume it is a spdesign.")
})

test_that("readdesign correctly identifies and processes idefix files", {
  # Arrange
  design_idefix <- system.file("extdata", "Idefix_designs", "test_design2.RDS", package = "simulateDCE")

  # Act
  result <- readdesign(design_idefix)

  # Assert
  expect_s3_class(result, "data.frame") # Check the result is a data frame
  expect_true("Choice.situation" %in% colnames(result)) # Verify key column presence

  # Ensure there are alternative-related columns
  alt_columns <- colnames(result)[grepl("^alt", colnames(result))]
  expect_gt(length(alt_columns), 0) # Confirm at least one alt-related column exists

  # Validate the structure of the output
  expect_gt(nrow(result), 0) # Ensure the data frame has rows
  expect_gt(ncol(result), 1) # Ensure the data frame has more than one column
  expect_message(readdesign(design_idefix), "I assume it is an idefix design.")
})



test_that("readdesign returns a data frame with proper structure", {
  # Arrange
  design_ngene <- system.file("extdata", "agora", "altscf_eff.ngd", package = "simulateDCE")

  # Act
  result <- readdesign(design_ngene)

  # Assert
  expect_s3_class(result, "data.frame") # Check the result is a data frame
  expect_true("Choice.situation" %in% colnames(result))

  # Ensure there are alternative-related columns
  alt_columns <- colnames(result)[grepl("^alt", colnames(result))]
  expect_gt(length(alt_columns), 0) # Confirm at least one alt-related column exists

  # Validate the structure of the output
  expect_gt(nrow(result), 0) # Ensure the data frame has rows
  expect_gt(ncol(result), 1) # Ensure the data frame has more than one column
})
