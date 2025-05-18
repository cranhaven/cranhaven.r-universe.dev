
stics_version <- get_stics_versions_compat()$latest_version

xml_plant <- file.path(
  get_examples_path("xml", stics_version = stics_version),
  "file_plt.xml"
)

cv_param_df <- get_cultivars_param(file = xml_plant)

context("Getting returned type")
test_that("type, data.frame", {
  expect_is(cv_param_df, "data.frame")
})

context("Checking presence of row names")
test_that("rownames", {
  expect_identical(! is.null(rownames(cv_param_df)), TRUE)
})
