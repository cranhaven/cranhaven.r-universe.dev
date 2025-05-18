library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

xml_sols <- file.path(
  get_examples_path("xml", stics_version = stics_version),
  "sols.xml"
)

sols_names <- get_soils_list(file = xml_sols)

# Testing returned type for one or several files
context("Getting returned type")

test_that("type, character vector", {
  expect_is(sols_names, "character")
})

context("checking existing soils names, one or two files")
sol_name <- "solcanne"
test_that("name, one file", {
  expect_true(sol_name %in% sols_names)
})

context("checking non-existing soilss names, one or two files")
sol_name <- "SolTest"
test_that("name, one file", {
  expect_false(sol_name %in% sols_names)
})


context("getting soils names with partial match search")

test_that("with or without success", {
  expect_equal(
    get_soils_list(file = xml_sols, soil = "to"),
    c("soltomate", "soltousol")
  )
  expect_equal(get_soils_list(file = xml_sols, soil = "zzz"), character(0))
})
