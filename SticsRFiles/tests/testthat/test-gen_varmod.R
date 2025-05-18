library(SticsRFiles)

context("Creating a varmod file to latest version")

out_dir <- file.path(tempdir(), "varmod")
if (!dir.exists(out_dir)) dir.create(out_dir)

gen_varmod(out_dir, "lai(n)")

test_that("Create a varmod file", {
  expect_true(file.exists(file.path(out_dir, "var.mod")))
})


gen_varmod(out_dir, "hauteur", append = TRUE)

test_that("Add a new variable", {
  expect_true(
    grep(pattern = "hauteur",
         readLines(file.path(out_dir, "var.mod"))) > 0)
  expect_warning(gen_varmod(out_dir, ""))

  })
