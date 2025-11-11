# ==============================================================================
# TEST SUITE 3: Direct Matching
# ==============================================================================
# Validates direct_match() function
# Critical for preventing false positives (issue discovered with rank mismatch)

test_that("Binomial names match correctly in original database", {
  # These are real species from DS 043-2006-AG
  input <- c(
    "Cattleya maxima",
    "Polylepis incana",
    "Aphelandra cuscoensis"
  )

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)

  expect_true(all(result$matched))
  expect_equal(result$Matched.Rank, c(2L, 2L, 2L))
  expect_true(all(result$Comp.Rank))  # Rank should match
})

test_that("Trinomial names match correctly when they exist", {
  # Real trinomial from DS 043-2006-AG
  input <- "Haageocereus acranthus subsp. olowinskianus"

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)

  expect_true(result$matched)
  expect_equal(result$Matched.Rank, 3L)
  expect_true(result$Comp.Rank)
})

test_that("Rank mismatch prevents false positives", {
  # CRITICAL TEST: User inputs trinomial, but only binomial exists in database
  # Example: "Cattleya maxima var. alba" (doesn't exist)
  # Database has: "Cattleya maxima" (exists)
  # Should NOT match to avoid false positive

  input <- "Cattleya maxima var. nonexistent"

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)

  # Should not match because rank mismatch (input=3, database=2)
  expect_false(result$matched)
  expect_equal(result$Threat.Status, "Not threatened")

  # valid_rank should be FALSE or NA
  expect_true(is.na(result$valid_rank) || result$valid_rank == FALSE)
})

test_that("Database capacity is respected (infraspecies_2)", {
  # CRITICAL: Updated database doesn't support infraspecies_2
  # Should generate warning for Rank 4 names

  input <- "Haageocereus acranthus subsp. olowinskianus f. deflexispinus"

  expect_warning(
    is_threatened_peru(input, source = "updated", return_details = TRUE),
    "Rank 4 names detected; the 'updated' dataset does not support infraspecies_2; they will not be matched"
  )
})

test_that("Whitespace in database doesn't prevent matching", {
  # Issue discovered: "Deprea macrocalyx " had trailing space
  # Should match correctly after trimws() implementation

  input <- "Deprea macrocalyx"

  result <- is_threatened_peru(input,
                               source = "updated",
                               return_details = TRUE)

  expect_equal(result$Orig.Name, "Deprea macrocalyx")
  expect_equal(result$Matched.Name, "Deprea macrocalyx")
})

