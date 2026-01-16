# ==============================================================================
# TEST SUITE 7: Edge Cases
# ==============================================================================
# Tests for unusual inputs and error handling

test_that("Empty input is handled gracefully", {
  expect_warning(is_threatened_peru(character(0)), "Empty species list")

  result <- suppressWarnings(is_threatened_peru(character(0)))
  expect_length(result, 0)
})

test_that("NA values are handled correctly", {
  input <- c("Cattleya maxima", NA, "Polylepis incana", NA)

  expect_message(
    is_threatened_peru(input),
    "2 species names were empty or NA and will be treated as 'Not threatened'"
  )

  result <- suppressMessages(is_threatened_peru(input))

  expect_length(result, 4)
  expect_equal(result[c(2, 4)], c("Not threatened", "Not threatened"))
})

test_that("Only whitespace is treated as empty", {
  input <- c("Cattleya maxima", "   ", "Polylepis incana")

  result <- suppressMessages(is_threatened_peru(input))

  expect_equal(result[2], "Not threatened")
})

test_that("Very long species lists are processed", {
  # Test with all 776 species from original database
  input <- peruflorads43:::threatenedperu$scientific_name |> unique()

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = FALSE)

  expect_length(result, length(input))
  expect_true(all(result != "Not threatened"))
})

test_that("Genus-only names generate informative message", {
 input <- "Microchilus"
 expect_message(
   is_threatened_peru(input),
   "should only include binomial names"
 )
 result <- suppressMessages(is_threatened_peru(input))
 expect_equal(result, "Not threatened")
})

test_that("Invalid source parameter throws error", {
  expect_error(
    is_threatened_peru("Cattleya maxima", source = "invalid"),
    regexp = "should be one of.*original.*updated",
    fixed  = FALSE
  )
})


test_that("Duplicate names are handled correctly", {
  input <- c("Cattleya maxima", "Cattleya maxima", "Cattleya maxima")

  result <- is_threatened_peru(input, return_details = TRUE)

  expect_equal(nrow(result), 3)
  expect_true(all(result$matched))
})

test_that("Mixed case input is standardized", {
  input <- c(
    "cattleya maxima",
    "POLYLEPIS INCANA",
    "Puya Raimondii"
  )

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)

  # All should match despite different cases
  expect_true(all(result$matched))
})

