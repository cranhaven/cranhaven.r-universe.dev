library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version


xml_usms <- file.path(
  get_examples_path("xml", stics_version = stics_version),
  "usms.xml"
)
usms_names <- get_usms_list(file = xml_usms)

# Testing returned type for one or several files
context("Getting returned type")

test_that("type, character vector", {
  expect_is(usms_names, "character")
  expect_equal(length(get_usms_list(file = xml_usms)), 44)
})


context("checking existing usms names, one or two files")
usm_name <- "SugarCane"
test_that("name, one file", {
  expect_true(usm_name %in% usms_names)
})


context("checking non-existing usms names, one or two files")
usm_name <- "UsmTest"

test_that("name, one file", {
  expect_false(usm_name %in% usms_names)
})


context("getting usms names with partial match search")

test_that("with or without success", {
  expect_equal(
    get_usms_list(file = xml_usms, usm = "to"),
    c("potato", "tomato", "proto_rice")
  )
  expect_equal(get_usms_list(file = xml_usms, usm = "zzz"), character(0))
})
