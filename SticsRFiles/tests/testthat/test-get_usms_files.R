library(dplyr)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

studycase_path <-
  file.path(tempdir(), "data-master", "study_case_1")

if (!dir.exists(studycase_path)) {
  studycase_path <-
    download_data(
      example_dirs = "study_case_1",
      stics_version = stics_version
  )
}

workspace_path <- file.path(studycase_path, "XmlFiles")

xml_usms <- file.path(workspace_path, "usms.xml")

usms_list <- get_usms_list(xml_usms)

usms_files <- get_usms_files(workspace_path)

context("Checking usm names in list")
test_that("checking usm exists", {
  expect_true("bo96iN+" %in% names(usms_files))
})

context("Checking all list usm names against usms file")
test_that("checking usm consistency", {
  expect_true(all(usms_list %in% names(usms_files)))
})

context("checking wrong usm name")
test_that("one name", {
  expect_false("brrr" %in% names(usms_files))
})

context("testing returned object type")
test_that("is list", {
  expect_true(is.list(usms_files))
})
test_that("is data.frame", {
  usms_files <- get_usms_files(workspace_path, df_output = TRUE)
  expect_true(is.data.frame(usms_files))
})

context("Checking data.frame column types")
test_that("usm, all_exist, paths", {
  usms_files <- get_usms_files(workspace_path, df_output = TRUE)
  expect_is(usms_files$usm, "character")
  expect_is(usms_files$all_exist, "logical")
  expect_is(usms_files$paths, "character")
})
