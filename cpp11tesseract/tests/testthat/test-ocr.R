test_that("ocr works", {
  skip_on_cran()
  file <- system.file("examples", "test2.png", package = "cpp11tesseract")
  numbers <- tesseract()
  expect_match(paste(ocr(file), collapse = " "), "test.*123")
})

test_that("ocr works with whitelisted characters", {
  skip_on_cran()
  file <- system.file("examples", "test2.png", package = "cpp11tesseract")
  numbers <- tesseract(options = list(tessedit_char_whitelist = "-$.0123456789"))
  expect_match(paste(ocr(file), collapse = " "), "123")
  expect_type(engine_info_internal(numbers)$datapath, "character")
})

test_that("ocr works with raw image", {
  skip_on_cran()
  file <- system.file("examples", "test2.png", package = "cpp11tesseract")
  raw <- readBin(file, "raw", file.info(file)$size)
  expect_match(paste(ocr(file), collapse = " "), "123")
})

test_that("ocr fails with bad parameters", {
  skip_on_cran()
  file <- system.file("examples", "test2.png", package = "cpp11tesseract")
  expect_error(ocr(file, engine = "enochian"))
  expect_error(ocr(NULL))
})
