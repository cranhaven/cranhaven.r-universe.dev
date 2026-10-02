test_that("All sections are applicable for dawa", {
  skip_on_cran()
  for (section in available_sections()) {
    expect_no_error(
      dawa(section = section, per_side = 50)
    )
  }
})
