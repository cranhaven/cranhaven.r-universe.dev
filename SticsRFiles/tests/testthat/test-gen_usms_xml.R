library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

context("Creating an xml usms file to latest version")

workspace_path <- get_examples_path("xml", stics_version = stics_version)
xl_path <- download_usm_xl(file = "inputs_stics_example.xlsx", overwrite = TRUE)
usms_param_df <- read_params_table(file = xl_path, sheet_name = "USMs")

out_dir <- file.path(tempdir(), "usm_xml")
if (!dir.exists(out_dir)) dir.create(out_dir)

gen_usms_xml(file = file.path(out_dir, "USMs.xml"), param_df = usms_param_df)

test_that("Create a xml USMs file", {
  expect_true(file.exists(file.path(out_dir, "USMs.xml")))
})
