

test_that("write_thai_preamble() render template-thai-preamble to a location", {
  # Create Temp file
  tmp_file <- file.path(tempdir(), "thai-preamble.tex")
  path <- write_thai_preamble(tmp_file)
  # File Exist ?
  expect_true(fs::file_exists(path))
  unlink(path)

})
