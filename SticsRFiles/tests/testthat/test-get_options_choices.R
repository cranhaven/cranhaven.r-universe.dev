library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

context("Get option choice")
xml_path <- file.path(get_examples_path(file_type = "xml"), "file_plt.xml")

test_that("Option choice", {
  expect_equal(
    length(unlist(get_options_choices(xml_path), use.names = FALSE)),
    93)
  expect_equal(
    length(unlist(get_options_choices(xml_path, "codetemp"),
                  use.names = FALSE)),
    2)
  expect_equal(
    length(unlist(get_options_choices(xml_path, c("codegdh", "codetemp")),
                  use.names = FALSE)),
    4)
})
