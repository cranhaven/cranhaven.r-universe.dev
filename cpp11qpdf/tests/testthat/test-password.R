test_that("read file with password", {
  pdf_file <- 'password.pdf'
  expect_type(pdf_compress(pdf_file, tempfile(), TRUE, "userpwd"), "character")
  expect_type(pdf_compress(pdf_file, tempfile(), FALSE, "userpwd"), "character")
  expect_error(pdf_compress(pdf_file, tempfile(), TRUE, 'wrong'))
  expect_error(pdf_compress(pdf_file, tempfile(), TRUE, ""))
  expect_error(pdf_compress(pdf_file, tempfile(), TRUE, " "))
  expect_error(pdf_compress(pdf_file, tempfile(), TRUE, NA))
  expect_error(pdf_compress(pdf_file, tempfile(), TRUE, NULL))
})
