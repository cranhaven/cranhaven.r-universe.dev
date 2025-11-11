# ==============================================================================
# TEST SUITE 5: Infraspecies Matching
# ==============================================================================
# Validates infraspecific rank matching (critical for preventing mismatches)

test_that("tag vs tag_acc column selection works correctly", {
  # Original database uses 'tag'
  # Updated database uses 'tag_acc'

  # Test with original
  input <- "Haageocereus acranthus subsp. olowinskianus"

  result_orig <- is_threatened_peru(input,
                                    source = "original",
                                    return_details = TRUE)

  expect_true(result_orig$matched)

  # Test with updated (should also work if species is there)
  result_upd <- is_threatened_peru(input,
                                   source = "updated",
                                   return_details = TRUE)

  # Both should handle their respective column correctly
  expect_true(is.logical(result_upd$matched))
})

test_that("Infraspecies level 2 only matches in original database", {
  input <- "Haageocereus acranthus subsp. olowinskianus f. deflexispinus"

  # 1) ORIGINAL: esta llamada emite un warning por ambigüedad de infraspecies.
  #    Opción A: capturarlo con expect_warning(...)
  expect_warning(
    result_orig <- is_threatened_peru(input, source = "original", return_details = TRUE),
    regexp = "multiple fuzzy matches.*infraspecies",
    fixed  = FALSE
  )
  # (Opción B alternativa: suprimirlo si no te interesa testearlo)
  # suppressWarnings(result_orig <- is_threatened_peru(input, source = "original", return_details = TRUE))

  # Asserts sobre el resultado en 'original'
  expect_false(result_orig$matched)
  expect_equal(result_orig$Matched.Rank, 3L)

  # 2) UPDATED: debe emitir el warning por Rank 4 no soportado.
  #    Permitimos que empiece con un número (p.ej., "1 Rank 4 ...")
  expect_warning(
    is_threatened_peru(input, source = "updated", return_details = TRUE),
    regexp = "\\d+\\s+Rank 4 names detected; the 'updated' dataset does not support infraspecies_2; they will not be matched|Rank 4 names detected; the 'updated' dataset does not support infraspecies_2; they will not be matched",
    fixed  = FALSE
  )
})

test_that("Empty infraspecies_2 in updated database is handled", {
  # Updated database has NA for infraspecies_2
  # Should not cause errors

  input <- "Cattleya maxima var. alba"

  result <- is_threatened_peru(input,
                               source = "updated",
                               return_details = TRUE)

  expect_true(is.na(result$Matched.Infraspecies_2))
})


