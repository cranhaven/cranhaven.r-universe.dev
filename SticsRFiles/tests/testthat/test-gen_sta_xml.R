library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

context("Creating an xml station file to latest version")

workspace_path <- get_examples_path("xml", stics_version = stics_version)
xl_path <- download_usm_xl(file = "inputs_stics_example.xlsx", overwrite = TRUE)
sta_param_df <- read_params_table(file = xl_path, sheet_name = "Station")
ini_param_df <- read_params_table(file = xl_path, sheet_name = "Ini")
out_dir <- file.path(tempdir(), "sta_xml")
if (!dir.exists(out_dir)) dir.create(out_dir)
gen_sta_xml(out_dir = out_dir, param_df = sta_param_df)

test_that("Create a xml station file", {
  expect_true(file.exists(file.path(out_dir, "climatex_sta.xml")))
})


test_that("Create a xml station file", {
  expect_error(gen_sta_xml(out_dir = out_dir, param_df = ini_param_df))
})
