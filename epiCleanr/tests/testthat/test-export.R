
# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

testthat::test_that("export function works correctly", {

  tmpdir <- tempfile()
  dir.create(tmpdir)

  withr::with_dir(tmpdir, {

    # Create a dummy data frame
    dummy_df <- data.frame(
      a = 1:5,
      b = letters[1:5]
    )

    # Supported formats
    formats <- c("csv", "tsv", "xlsx", "rds", "RData", "dta")

    for (fmt in formats) {
      # Define file path
      file_path <- paste0(tempfile(), ".", fmt)

      # Test: Check if the function successfully exports without error
      testthat::expect_error(epiCleanr::export(dummy_df, file_path), NA)

      # Read the data back
      imported_df <- epiCleanr::import(file_path)
      attributes(imported_df$a) <- NULL
      attributes(imported_df$b) <- NULL

      # Test: Check if the data read back is the same as the original data
      testthat::expect_equal(dummy_df, imported_df, ignore_attr = TRUE)
    }

    # Test: Check if the function raises an error for unsupported formats
    unsupported_file_path <- paste0(tempfile(), ".unsupported")
    testthat::expect_error(export(dummy_df, unsupported_file_path))
  })
})

