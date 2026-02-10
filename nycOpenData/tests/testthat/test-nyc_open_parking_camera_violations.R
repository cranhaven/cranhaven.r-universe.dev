test_that("nyc_open_parking_camera_violations returns a tibble and respects limits", {
  # This 'cassette' will be saved in tests/fixtures/nyc_311_test.yml
  vcr::use_cassette("nyc_open_parking_camera_violations_test", {

    # We use a small limit (2) to keep the recording file small
    results <- nyc_open_parking_camera_violations(limit = 2)

    # Assertions: What should be true?
    expect_s3_class(results, "tbl_df")
    expect_equal(nrow(results), 2)
  })
})



