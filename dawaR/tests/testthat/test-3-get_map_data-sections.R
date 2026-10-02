test_that("All sections are applicable for get_map_data", {
  skip_on_ci()
  skip_on_cran()
  for (section in available_sections(format = "geojson")) {
    expect_no_error(
      get_map_data(type = section, per_side = 50)
    )
  }
})
