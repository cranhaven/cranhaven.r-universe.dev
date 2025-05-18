library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

workspace_path <- get_examples_path("xml", stics_version = stics_version)

context("Exist Javasitcs ")

test_that("exist Javastics", {
  expect_error(check_java_path(workspace_path))
  expect_error(check_java_path(file.path(workspace_path, "xxx")))
})
