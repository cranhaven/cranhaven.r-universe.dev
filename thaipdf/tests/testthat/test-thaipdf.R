


# Test: Thai PDF ----------------------------------------------------------


test_that("thaipdf_document() return `rmarkdown_output_format`",{

  expect_s3_class(thaipdf::thaipdf_document(), "rmarkdown_output_format")

})


# Test: Thai Book ----------------------------------------------------------


test_that("thaipdf_book() return `rmarkdown_output_format`",{

  expect_s3_class(thaipdf::thaipdf_book(), "rmarkdown_output_format")

})
