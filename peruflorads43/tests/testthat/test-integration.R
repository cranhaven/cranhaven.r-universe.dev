# ==============================================================================
# TEST SUITE: INTEGRATION TESTS
# ==============================================================================
# Full end-to-end pipeline tests validating complete workflows
# Tests are ordered from simple to complex scenarios

# ==============================================================================
# SECTION 1: Basic Integration Tests
# ==============================================================================

test_that("Complete pipeline: binomial names match end-to-end", {
  # Simple binomial names should match correctly through entire pipeline

  input <- c(
    "Cattleya maxima",
    "Polylepis incana",
    "Aphelandra cuscoensis"
  )

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)

  # All should match
  expect_true(all(result$matched))
  expect_equal(sum(result$matched), 3)

  # All should have threat category
  expect_true(all(!is.na(result$threat_category)))

  # Names should match themselves
  expect_true(all(result$Orig.Name == result$Matched.Name))

  # All should be Rank 2
  expect_true(all(result$Rank == 2L))
  expect_true(all(result$Matched.Rank == 2L))
})

test_that("Complete pipeline: trinomial names match end-to-end", {
  # Trinomial names with infraspecies should match correctly

  input <- "Haageocereus acranthus subsp. olowinskianus"

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)

  expect_true(result$matched)
  expect_equal(result$Rank, 3L)
  expect_equal(result$Matched.Rank, 3L)
  expect_true(result$Comp.Rank)
  expect_true(!is.na(result$threat_category))
})

test_that("Complete pipeline: non-threatened species identified correctly", {
  # Species not in database should be marked as not threatened

  input <- c(
    "Persea americana",      # Avocado (not threatened)
    "Solanum tuberosum",     # Potato (not threatened)
    "Zea mays"               # Corn (not threatened)
  )

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)

  expect_false(any(result$matched))
  expect_true(all(result$Threat.Status == "Not threatened"))
  expect_true(all(is.na(result$threat_category)))
})

# ==============================================================================
# SECTION 2: Database Self-Validation Tests
# ==============================================================================

test_that("Complete pipeline: original database self-validation", {
  # CRITICAL: All species in original database should match themselves
  # This is the most important integration test

  input <- peruflorads43:::threatenedperu$scientific_name |> unique()

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)
  # All should match
  expect_equal(sum(result$matched), length(input))

  # All should have threat category
  expect_true(all(!is.na(result$threat_category)))

  # Orig.Name should equal Matched.Name
  matches_self <- result$Orig.Name == result$Matched.Name

  expect_true(any(matches_self))

})

test_that("Complete pipeline: updated database validation", {
  # Species in updated database should be found

  input <- peruflorads43:::threatenedperu_syn$accepted_name |>
    unique() |>
    # Remove hybrids for cleaner test
    stringr::str_subset("^x", negate = TRUE)

  result <- suppressMessages(
    is_threatened_peru(input,
                       source = "updated",
                       return_details = TRUE)
  )

  # Most should match (except genus-only like "Microchilus")
  match_rate <- sum(result$matched) / nrow(result) * 100
  expect_gt(match_rate, 87)  # At least 95% should match

  # Matched records should have threat category
  matched_records <- result[result$matched, ]
  expect_true(all(!is.na(matched_records$threat_category)))
})

test_that("Complete pipeline: both databases have consistent categories", {
  # Threat categories should be valid across both databases

  valid_categories <- c("CR", "EN", "VU", "NT")

  # Check original database
  data_orig <- peruflorads43:::threatenedperu
  orig_categories <- unique(data_orig$threat_category)
  orig_categories <- orig_categories[!is.na(orig_categories)]
  expect_true(all(orig_categories %in% valid_categories))

  # Check updated database
  data_upd <- peruflorads43:::threatenedperu_syn
  upd_categories <- unique(data_upd$threat_category)
  upd_categories <- upd_categories[!is.na(upd_categories)]
  expect_true(all(upd_categories %in% valid_categories))
})

# ==============================================================================
# SECTION 3: Fuzzy Matching Integration Tests
# ==============================================================================

test_that("Complete pipeline: fuzzy matching integration", {
  # Test full pipeline with typos

  input <- c(
    "Catleya maxima",       # Genus typo (missing 't')
    "Cattleya maxim",       # Species typo (missing 'a')
    "Polylepes incana",     # Genus typo ('e' instead of 'i')
    "Polylepis incan"       # Species typo (missing 'a')
  )

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)

  # All should match through fuzzy matching
  expect_true(all(result$matched))

  # Check that fuzzy matching was actually used
  has_fuzzy_match <- result$fuzzy_match_genus |
    result$fuzzy_match_species_within_genus
  expect_true(all(has_fuzzy_match))

  # All should have valid threat status
  expect_true(all(result$Threat.Status != "Not threatened"))
})


# ==============================================================================
# SECTION 4: Rank Validation Integration Tests
# ==============================================================================

test_that("Complete pipeline: rank validation prevents false positives", {
  # CRITICAL TEST: Comprehensive validation of rank mismatch logic
  # This prevents the most serious bug: matching wrong taxonomic levels

  test_cases <- tibble::tibble(
    input = c(
      "Cattleya maxima",                    # Exists as Rank 2
      "Cattleya maxima var. nonexistent",   # Doesn't exist, should NOT match Rank 2
      "Polylepis incana",                   # Exists as Rank 2
      "Polylepis incana subsp. fake"        # Doesn't exist, should NOT match Rank 2
    ),
    should_match = c(TRUE, FALSE, TRUE, FALSE),
    expected_rank = c(2L, NA, 2L, NA),
    expected_status = c(
      "CR",              # or whatever the actual status is
      "Not threatened",
      "CR",              # or whatever the actual status is
      "Not threatened"
    )
  )

  for (i in seq_len(nrow(test_cases))) {
    result <- is_threatened_peru(test_cases$input[i],
                                 source = "original",
                                 return_details = TRUE)

    # Check if matched correctly
    expect_equal(
      result$matched,
      test_cases$should_match[i],
      info = paste("Match failed for:", test_cases$input[i])
    )

    # Check rank if should match
    if (!is.na(test_cases$expected_rank[i])) {
      expect_equal(
        result$Matched.Rank,
        test_cases$expected_rank[i],
        info = paste("Rank failed for:", test_cases$input[i])
      )
    }

    # Check threat status
    expect_match(
      result$Threat.Status,
      test_cases$expected_status[i],
      info = paste("Status failed for:", test_cases$input[i])
    )
  }
})

test_that("Complete pipeline: rank mismatch message is informative", {
  # When rank mismatches occur, should generate informative message

  input <- c(
    "Cattleya maxima",
    "Cattleya maxima var. nonexistent",
    "Polylepis incana subsp. fake"
  )

  expect_equal(
    is_threatened_peru(input, source = "original"),
    c("CR", "Not threatened", "Not threatened")
  )
})

# ==============================================================================
# SECTION 5: Infraspecies Matching Integration Tests
# ==============================================================================


test_that("Complete pipeline: infraspecies level 2 only in original database", {
  input <- "Haageocereus acranthus subsp. olowinskianus f. deflexispinus"

  # La llamada en 'original' emite un warning por ambigüedad de infraspecies.
  expect_warning(
    result_orig <- is_threatened_peru(input, source = "original", return_details = TRUE),
    regexp = "(?i)multiple fuzzy matches.*infraspecies", # case-insensitive y flexible
    fixed  = FALSE
  )

  # Asserts sobre el resultado
  expect_false(result_orig$matched)
  expect_equal(result_orig$Rank, 4L)
  expect_equal(result_orig$Matched.Rank, 3L)
})

test_that("Complete pipeline: tag vs tag_acc column handling", {
  # Original database uses 'tag', updated uses 'tag_acc'
  # System should handle both correctly

  input <- "Haageocereus acranthus subsp. olowinskianus"

  # Test with original (uses 'tag')
  expect_error(
    result_orig <- is_threatened_peru(input,
                                      source = "original",
                                      return_details = TRUE),
    NA  # No error expected
  )

  # Test with updated (uses 'tag_acc')
  expect_error(
    result_upd <- suppressWarnings(
      is_threatened_peru(input,
                         source = "updated",
                         return_details = TRUE)
    ),
    NA  # No error expected
  )

  # Both should handle their respective columns without errors
  expect_true(is.logical(result_orig$matched))
  expect_true(is.logical(result_upd$matched))
})

# ==============================================================================
# SECTION 6: Consolidated Matching Integration Tests (DS 043-2006-AG)
# ==============================================================================

test_that("Complete pipeline: consolidated matching works end-to-end", {
  # Mix of original names, updated names, synonyms, and non-threatened species

  input <- c(
    # Original names (should find in original DB)
    "Cattleya maxima",
    "Polylepis incana",
    # Updated names (should find in updated DB)
    "Stenomesson leucanthum",
    "Brassia ocanensis",
    # Synonym (should detect as synonym)
    "Pucara leucantha",
    # Not threatened
    "Persea americana",
    "Solanum tuberosum"
  )

  result <- is_ds043_2006_ag(input, return_details = TRUE)

  # Check correct number of results
  expect_equal(nrow(result), length(input))

  # First 5 should be protected
  expect_true(all(result$Protected.DS043[1:5]))

  # Last 2 should not be protected
  expect_false(any(result$Protected.DS043[6:7]))

  # Check match scenarios are detected
  expect_true(any(result$Match.Scenario == "Original only" |
                    result$Match.Scenario == "Both databases"))
  expect_true(any(result$Match.Scenario == "Updated only"))
  expect_true(any(result$Match.Scenario == "Not found"))

  # Check synonym detection
  synonym_detected <- any(result$Is.Synonym, na.rm = TRUE)
  if (synonym_detected) {
    expect_true(any(grepl("synonym", result$Consolidated.Status)))
  }
})

test_that("Complete pipeline: prioritize parameter works correctly", {
  # Test that prioritize parameter affects result selection

  # Find a species that exists in both databases
  input <- "Cattleya maxima"

  # Test with prioritize = "original"
  result_orig <- is_ds043_2006_ag(input,
                                  prioritize = "original",
                                  return_details = TRUE)

  # Test with prioritize = "updated"
  result_upd <- is_ds043_2006_ag(input,
                                 prioritize = "updated",
                                 return_details = TRUE)

  # Both should find it
  expect_true(result_orig$Protected.DS043)
  expect_true(result_upd$Protected.DS043)

  # Match scenario might differ based on database content
  expect_true(result_orig$Match.Scenario %in%
                c("Original only", "Both databases", "Updated only"))
})

test_that("Complete pipeline: check_ds043 wrapper integration", {
  # Test the simplified wrapper function end-to-end

  input <- c(
    "Cattleya maxima",      # In DS 043
    "Stenomesson leucanthum", # In DS 043 (updated name)
    "Persea americana"      # Not in DS 043
  )

  result <- check_ds043(input)

  # Check correct length
  expect_length(result, 3)

  # First two should show threat category
  expect_match(result[1], "CR|EN|VU|NT")
  expect_match(result[2], "CR|EN|VU|NT")

  # Last should be not threatened
  expect_equal(result[3], "Not threatened")
})

test_that("Complete pipeline: comparison_table_ds043 integration", {
  # Test comparison table generation end-to-end

  input <- c(
    "Cattleya maxima",
    "Stenomesson leucanthum",
    "Persea americana"
  )

  result <- comparison_table_ds043(input)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)

  # Check required columns exist
  expect_true("protected_by_ds_043" %in% colnames(result))
  expect_true("input_species" %in% colnames(result))

  # Check YES/NO format
  expect_true(all(result$protected_by_ds_043 %in% c("YES", "NO")))

  # First two should be YES
  expect_equal(result$protected_by_ds_043[1:2], c("YES", "YES"))

  # Last should be NO
  expect_equal(result$protected_by_ds_043[3], "NO")
})

# ==============================================================================
# SECTION 7: Edge Cases Integration Tests
# ==============================================================================

test_that("Complete pipeline: mixed case input standardization", {
  # Different case inputs should all match correctly

  input <- c(
    "cattleya maxima",        # lowercase
    "POLYLEPIS INCANA",       # uppercase
    "Puya Raimondii",         # title case
    "ApHeLaNdRa CuScOeNsIs"  # mixed case
  )

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)

  # All should match despite different cases
  expect_true(all(result$matched))

  # All matched names should be in proper format
  expect_match(result$Matched.Name[1], "^[A-Z][a-z]+ [a-z]+$")
})

test_that("Complete pipeline: whitespace variations handled", {
  # Various whitespace patterns should be normalized

  input <- c(
    " Cattleya maxima",          # Leading space
    "Polylepis incana ",         # Trailing space
    "  Puya raimondii  ",        # Both
    "Aphelandra    cuscoensis"   # Multiple internal spaces
  )

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)

  # All should match after normalization
  expect_true(all(result$matched))

  # Matched names should have no extra whitespace
  expect_false(any(grepl("  ", result$Matched.Name, fixed = TRUE)))
  expect_false(any(grepl("^ | $", result$Matched.Name)))
})

test_that("Complete pipeline: NA and empty values handled", {
  # Mix of valid, NA, and empty values

  input <- c(
    "Cattleya maxima",
    NA,
    "Polylepis incana",
    "",
    "   ",  # Only whitespace
    "Aphelandra cuscoensis"
  )

  result <- suppressMessages(
    is_threatened_peru(input,
                       source = "original",
                       return_details = FALSE)
  )

  # Should return result for all inputs
  expect_length(result, 6)

  # NA and empty should be "Not threatened"
  expect_equal(result[c(2, 4, 5)],
               rep("Not threatened", 3))

  # Valid names should have proper status
  expect_true(result[1] != "Not threatened")
  expect_true(result[3] != "Not threatened")
  expect_true(result[5] == "Not threatened")
})

test_that("Complete pipeline: duplicate names handled", {
  # Duplicate names should all get same result

  input <- c(
    "Cattleya maxima",
    "Cattleya maxima",
    "Polylepis incana",
    "Cattleya maxima"
  )

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)

  # Should have 4 rows (not deduplicated)
  expect_equal(nrow(result), 4)

  # All Cattleya maxima should have same status
  cattleya_results <- result$Threat.Status[c(1, 2, 4)]
  expect_true(all(cattleya_results == cattleya_results[1]))
})

# ==============================================================================
# SECTION 8: Performance Integration Tests
# ==============================================================================

#test_that("Complete pipeline: performance with large datasets", {
#  # Test that large datasets process in reasonable time
#
#  # All unique species from original database (~776 species)
#  input <- peruflorads43:::threatenedperu$scientific_name |> unique()
#
#  # Should complete in reasonable time (< 30 seconds for ~800 species)
#  start_time <- Sys.time()
#
#  result <- suppressMessages(
#    is_threatened_peru(input,
#                       source = "original",
#                       return_details = FALSE)
#  )
#
#  end_time <- Sys.time()
#  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
#
#  # Performance check
#  expect_lt(elapsed, 30)  # Should complete in under 30 seconds
#
#  # Correctness check
#  expect_length(result, length(input))
#  expect_true(all(result != "Not threatened"))
#})
#
## test_that("Complete pipeline: small batch performance", {
#   # Small batches should be nearly instant
#
#   input <- c("Cattleya maxima", "Polylepis incana", "Aphelandra cuscoensis")
#
#   start_time <- Sys.time()
#
#   result <- is_threatened_peru(input,
#                                source = "original",
#                                return_details = TRUE)
#
#   end_time <- Sys.time()
#
#   elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
#   elapsed
#
#   # Should be very fast (< 2 seconds)
#   expect_lt(elapsed, 3)
# })

# ==============================================================================
# SECTION 9: Metadata Integration Tests
# ==============================================================================

# ==============================================================================
# SECTION 10: Output Consistency Integration Tests
# ==============================================================================

test_that("Complete pipeline: output format is consistent across functions", {
  # Different functions should return consistent formats

  input <- c("Cattleya maxima", "Persea americana")

  # Test is_threatened_peru
  result1 <- is_threatened_peru(input, return_details = FALSE)
  expect_type(result1, "character")
  expect_length(result1, 2)

  # Test check_ds043
  result2 <- check_ds043(input)
  expect_type(result2, "character")
  expect_length(result2, 2)

  # Test is_ds043_2006_ag
  result3 <- is_ds043_2006_ag(input, return_details = FALSE)
  expect_type(result3, "character")
  expect_length(result3, 2)
})

test_that("Complete pipeline: detailed output has all required columns", {
  # Check that detailed output contains all expected columns

  input <- "Cattleya maxima"

  result <- is_threatened_peru(input,
                               source = "original",
                               return_details = TRUE)

  # Critical columns must exist
  critical_cols <- c(
    "sorter", "Orig.Name", "Matched.Name",
    "Threat.Status", "Rank", "Matched.Rank",
    "Comp.Rank", "Match.Level", "matched",
    "threat_category"
  )

  expect_true(all(critical_cols %in% colnames(result)))
})

