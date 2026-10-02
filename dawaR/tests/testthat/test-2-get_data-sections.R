test_that("All sections are applicable for get_data", {
  skip_on_cran()
  for (section in available_sections(func = "get_data")) {
    expect_no_error(
      get_data(section = section, per_side = 50)
    )
  }
})
