test_that("tesseract_version works", {
  expect_gte(tesseract_major_version(), 3L)
})

test_that("tesseract_params works", {
  expect_type(tesseract_params("colour"), "list")
})

test_that("tesseract_params works", {
  fout <- tempfile(fileext = ".txt")
  writeLines("colour", fout)
  expect_type(parse_params(fout), "list")
})

test_that("tesseract C++ backend", {
  expect_type(check_training_data(), "externalptr")
})
