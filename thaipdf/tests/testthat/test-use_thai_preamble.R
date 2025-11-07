

# Test: use_thai_preamble() -----------------------------------------------


test_that("use_thai_preamble() generate file in working dir (use temp file)", {
  # Set Working Dir to Temp Dir
  .old_wd <- setwd(tempdir())

  # use_thai_preamble
  path <- suppressMessages(thaipdf::use_thai_preamble("thai-preamble.tex"))

  expect_true(fs::file_exists("thai-preamble.tex"))
  expect_true(fs::file_exists(path))
  #fs::file_show("thai-preamble.tex")
  # Clean up
  unlink("thai-preamble.tex")
  setwd(.old_wd)

})


test_that("use_thai_preamble() generate file in absolute location (use temp file)", {
  # Create Temp file
  tmp_file <- file.path(tempdir(), "thai-preamble.tex")
  # use_thai_preamble
  path <- suppressMessages(thaipdf::use_thai_preamble(tmp_file))

  # Check file exist and output path works ?
  expect_true(fs::file_exists(tmp_file))
  expect_true(fs::file_exists(path))
  # fs::file_show(tempdir())
  # Clean up
  unlink(tmp_file)
})


