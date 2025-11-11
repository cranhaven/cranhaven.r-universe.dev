# =============================================================================
# Tests for Ambiguous Match Handling
# =============================================================================

test_that("get_ambiguous_matches returns NULL when no ambiguous matches", {
  skip_on_cran()

  # Use species that match unambiguously
  species_list <- c("Cattleya maxima", "Polylepis incana")
  matching_threatenedperu(species_list)
  result <- is_threatened_peru(species_list, return_details = TRUE)

  # Should return NULL and message
  expect_message(
    ambig <- get_ambiguous_matches(result, type = "genus"),
    "No ambiguous.*found"
  )
  expect_null(ambig)
})



test_that("get_ambiguous_matches validates input", {
  # Test with invalid input
  expect_error(
    get_ambiguous_matches("not a dataframe"),
    "must be a data frame"
  )

  expect_error(
    get_ambiguous_matches(list(a = 1, b = 2)),
    "must be a data frame"
  )
})

test_that("get_ambiguous_matches handles invalid type parameter", {
  species_list <- c("Cattleya maxima")
  result <- is_threatened_peru(species_list, return_details = TRUE)

  # Invalid type should error via match.arg
  expect_error(
    get_ambiguous_matches(result, type = "invalid"),
    "should be one of"
  )
})


test_that("get_ambiguous_matches handles missing output directory", {
  skip_on_cran()

  species_list <- c("Cattleya maxima")
  result <- is_threatened_peru(species_list, return_details = TRUE)

  # Non-existent directory should be created
  temp_base <- tempdir()
  new_dir <- file.path(temp_base, "new_test_dir", "nested")

  # Ensure directory doesn't exist
  if (dir.exists(new_dir)) {
    unlink(new_dir, recursive = TRUE)
  }

  # Should create directory and not error
  expect_no_error({
    get_ambiguous_matches(
      result,
      type = "genus",
      save_to_file = TRUE,
      output_dir = new_dir
    )
  })

  # Cleanup
  if (dir.exists(new_dir)) {
    unlink(new_dir, recursive = TRUE)
  }
})

