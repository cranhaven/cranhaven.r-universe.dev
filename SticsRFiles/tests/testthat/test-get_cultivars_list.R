
stics_version <- get_stics_versions_compat()$latest_version

xml_plant <- file.path(
  get_examples_path("xml", stics_version = stics_version),
  "file_plt.xml"
)

cultivar_names <- get_cultivars_list(file = xml_plant)

context("Getting returned type")
test_that("type, character vector", {
  expect_is(cultivar_names, "character")
})

context("checking existing cultivar name")
cv_name <- "Sol"
test_that("name", {
  expect_true(cv_name %in% cultivar_names)
})

context("checking non-existing cultivar name")
cv_name <- "SolTest"
test_that("name", {
  expect_false(cv_name %in% cultivar_names)
})
