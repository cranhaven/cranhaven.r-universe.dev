test_that("nys_any_dataset returns a tibble, respects limits, and supports postprocess toggles", {
  skip_if_not_installed("vcr")
  skip_if_not_installed("curl")

  endpoint <- "https://data.ny.gov/resource/kwxv-fwze.json"

  vcr::use_cassette("nys_any_dataset_robust", {
    base <- nys_any_dataset(endpoint, limit = 2)
    expect_s3_class(base, "tbl_df")
    expect_true(nrow(base) >= 0 && nrow(base) <= 2)

    # clean_names TRUE default -> should have no dots or spaces
    expect_true(all(!grepl("\\.| ", names(base))))

    raw_names <- nys_any_dataset(endpoint, limit = 2, clean_names = FALSE, coerce_types = FALSE)
    expect_s3_class(raw_names, "tbl_df")

    # LOGIC FIX: Check if the RAW names needed cleaning in the first place
    if (ncol(base) > 0 && ncol(raw_names) > 0) {
      raw_is_already_clean <- identical(names(raw_names), janitor::make_clean_names(names(raw_names)))

      if (!raw_is_already_clean) {
        # If they were messy, the toggle should have made them different
        expect_false(identical(names(base), names(raw_names)))
      } else {
        # If they were already clean, they should be identical
        expect_true(identical(names(base), names(raw_names)))
      }
    }
  }) # Added missing brace for use_cassette
}) # Added missing brace for test_that
