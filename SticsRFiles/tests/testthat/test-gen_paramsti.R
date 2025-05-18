library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

context("Creating an xml station file to latest version")

workspace_path <- get_examples_path("csv", stics_version = stics_version)

test_that("Create a xml station file", {
  expect_true(gen_paramsti(workspace_path, c("par1", "par2"), c(1, 2)))
  expect_error(gen_paramsti(
    file.path(workspace_path, "xxx"),
    c("par1", "par2"),
    c(1, 2)))
  expect_false(gen_paramsti(workspace_path, c("par1", "par2"), c(1, 2, 3)))
})
