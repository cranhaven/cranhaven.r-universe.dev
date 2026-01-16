# ==============================================================================
# TEST SUITE 4: Fuzzy Matching
# ==============================================================================
# Validates fuzzy matching with typos and variations

test_that("Fuzzy genus matching works with small typos", {
  # Typos with distance = 1
  input <- c(
    "Catleya maxima",      # Missing 't' in Cattleya
    "Polylepes incana",    # Wrong 'e' instead of 'i' in Polylepis
    "Puia raimondii"       # Missing 'y' in Puya
  )

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)

  # Should match despite typos
  expect_true(all(result$fuzzy_match_genus))
  expect_equal(result$fuzzy_genus_dist, c(1, 1, 1))
})

test_that("Fuzzy species matching works within matched genus", {
  input <- c(
    "Cattleya maxim",      # Missing 'a' in maxima
    "Polylepis incanus"    # Wrong ending in incana
  )
 result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)
  # Should match through fuzzy matching
  expect_true(all(!is.na(result$Matched.Species)))
})

