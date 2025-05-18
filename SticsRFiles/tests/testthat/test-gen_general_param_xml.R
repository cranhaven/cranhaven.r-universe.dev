
context("Generating xml general parameters files")

gen_general_param_xml(out_dir = tempdir())

files <- c("param_gen.xml", "param_newform.xml")

test_that("latest version", {
  expect_true(all(file.exists(file.path(tempdir(), files))))
  expect_error(gen_general_param_xml(out_dir = tempdir()))
  expect_no_error(gen_general_param_xml(out_dir = tempdir(), overwrite = TRUE))
})



test_that("other version", {
  expect_no_error(gen_general_param_xml(out_dir = tempdir(),
                                        stics_version = "V10.0",
                                        overwrite = TRUE))
  expect_true(all(file.exists(file.path(tempdir(), files))))
})
