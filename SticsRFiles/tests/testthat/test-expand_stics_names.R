library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

context("Creating a file containing expanded stics names")

workspace_path <- get_examples_path("csv", stics_version = stics_version)

csv_file <- file.path(workspace_path, "inputs.csv")
out_dir <- file.path(tempdir(), "CSVexport")
if (!dir.exists(out_dir)) dir.create(out_dir)
out_csv_file <- file.path(out_dir, "inputs_xpanded.csv")
expand_stics_names(csv_file, out_csv_file)

test_that("expanded names", {
  expect_true(file.exists(file.path(out_dir, "inputs_xpanded.csv")))
})
