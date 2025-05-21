test_that("ocr_data works", {
  skip_on_cran()
  file <- system.file("examples", "test2.png", package = "cpp11tesseract")
  expect_type(ocr_data(file), "list")
  expect_type(ocr_data(file, engine = "eng"), "list")
  expect_error(ocr_data(file, engine = NULL))
  expect_error(ocr_data(NULL))
})
