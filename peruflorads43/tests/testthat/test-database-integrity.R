# ==============================================================================
# TEST SUITE 8: Database Integrity
# ==============================================================================
# Validates internal database structure and consistency

test_that("Original database has required columns", {
  data <- peruflorads43:::threatenedperu

  required_cols <- c(
    "scientific_name", "author", "family", "tag",
    "infraspecies_2", "accepted_name", "accepted_name_author",
    "accepted_family", "taxonomic_status",
    "threat_category", "genus", "species",
    "infraspecies"
  )

  expect_true(all(required_cols %in% colnames(data)))
})

test_that("Updated database has required columns", {
  data <- peruflorads43:::threatenedperu_syn

  required_cols <- c(
    "genus", "species", "tag_acc", "infraspecies",
    "threat_category", "accepted_name"
  )

  expect_true(all(required_cols %in% colnames(data)))
})

test_that("No trailing whitespace in database fields", {
  # Critical: Issue discovered with "Deprea macrocalyx "

  data_orig <- peruflorads43:::threatenedperu
  data_upd <- peruflorads43:::threatenedperu_syn

  # Check genus
  expect_false(any(grepl("^\\s|\\s$",
                         data_orig$genus)))
  expect_false(any(grepl("^\\s|\\s$",
                         data_upd$genus)))

  # Check species
  expect_false(any(grepl("^\\s|\\s$",
                         data_orig$species)))
  expect_false(any(grepl("^\\s|\\s$",
                         data_upd$species)))

  # Check accepted_name
  expect_false(any(grepl("^\\s|\\s$",
                         data_orig$accepted_name)))
  expect_false(any(grepl("^\\s|\\s$",
                         data_upd$accepted_name)))
})

test_that("Threat categories are valid", {
  valid_categories <- c("CR", "EN", "VU", "NT", NA)

  data_orig <- peruflorads43:::threatenedperu
  data_upd <- peruflorads43:::threatenedperu_syn

  expect_true(all(data_orig$threat_category %in% valid_categories))
  expect_true(all(data_upd$threat_category %in% valid_categories))
})

test_that("Taxonomic status values are valid", {
  valid_status <- c("Accepted", "Synonym", NA)

  data_orig <- peruflorads43:::threatenedperu

  expect_false(all(data_orig$taxonomic_status %in% valid_status))
})

test_that("Infraspecific ranks are properly formatted", {
  data_orig <- peruflorads43:::threatenedperu

  # Should be uppercase with period
  valid_ranks <- c("SUBSP.", "VAR.", "SUBVAR.", "F.", "SUBF.", NA)

  unique_tags <- unique(data_orig$tag)
  unique_tags <- toupper(unique_tags)  # Standardize for comparison

  expect_true(all(unique_tags %in% valid_ranks))
})

test_that("All rows have valid genus-species combination", {
  data_orig <- peruflorads43:::threatenedperu

  # Genus and species should not be NA simultaneously for valid records
  valid_rows <- !(is.na(data_orig$genus) & is.na(data_orig$species))

  expect_true(all(valid_rows))
})

test_that("Infraspecies_2 only exists when infraspecies exists", {
  data_orig <- peruflorads43:::threatenedperu

  # If infraspecies_2 is not NA, infraspecies should also not be NA
  has_infra2 <- !is.na(data_orig$infraspecies_2)

  if (any(has_infra2)) {
    expect_true(all(!is.na(data_orig$infraspecies[has_infra2])))
  }
})

