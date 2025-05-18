stics_version <- get_stics_versions_compat()$latest_version

ex_path <- get_examples_path("xml", stics_version = stics_version)

xml_usms <- file.path(ex_path, "usms.xml")

xml_sols <- file.path(ex_path, "sols.xml")

soils_names_usms <- get_soils_list(file = xml_usms)
soils_names_sols <- get_soils_list(file = xml_sols)

# Testing returned type for one or several files
context("Getting returned type")

test_that("type, character vector", {
  expect_is(soils_names_usms, "character")
  expect_is(soils_names_sols, "character")
  expect_equal(length(soils_names_usms), 26)
  expect_equal(length(soils_names_sols), 33)
})


context("checking existing soils names, one or two files")
soil_name <- "solcanne"
test_that("name, one file", {
  expect_true(soil_name %in% soils_names_usms)
  expect_true(soil_name %in% soils_names_sols)
})


context("checking non-existing usms names, one or two files")
soil_name <- "SoilTest"

test_that("name, one file", {
  expect_false(soil_name %in% soils_names_usms)
  expect_false(soil_name %in% soils_names_sols)
})


context("getting soils names with partial match search")

test_that("with or without success", {
  expect_equal(
    get_soils_list(file = xml_usms, soil = "to"),
    c("soltomate", "soltousol")
  )
  expect_equal(
    get_soils_list(file = xml_sols, soil = "z"),
    c("solcolza", "soluzerne")
  )

  expect_equal(
    get_soils_list(file = xml_usms, soil = c("soil_rice", "to")),
                   c("soil_rice", "soltomate", "soltousol")
  )

  expect_equal(get_soils_list(file = xml_usms, soil = "zzz"), character(0))
  expect_equal(get_soils_list(file = xml_sols, soil = "zzz"), character(0))
})
