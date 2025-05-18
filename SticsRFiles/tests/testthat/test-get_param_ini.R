library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

xml_path <- file.path(
  get_examples_path("xml", stics_version = stics_version),
  "file_ini.xml"
)
context("Getting initialisation param values")

test_that("single param option value ", {
  expect_equivalent(unlist(get_param_xml(xml_path, "nbplantes")), 1)
})


# unlist with different types produces a character vector
if (version_num < 10) {
  par_list <- c(
    "stade0", "lai0", "masec0", "QNplante0",
    "magrain0", "zrac0", "resperenne0"
  )
} else {
  par_list <- c(
    "stade0", "lai0", "masec0", "QNplante0",
    "magrain0", "zrac0", "restemp0"
  )
}

result <- c("snu", "0", "0", "0", "0", "0", "0")
test_that("multiple param option value", {
  val <- unlist(get_param_xml(xml_path, par_list,
    select = "plante",
    select_value = 1
  ),
  use.names = FALSE
  )


  expect_equal(val, c("snu", "0", "0", "0", "0", "0", "0"))
})


test_that("multiple param option value 2", {
  val <- unlist(get_param_xml(xml_path, "densinitial",
    select = "plante",
    select_value = 1
  ))
  expect_equivalent(val, c(0, 0, 0, 0, 0))
})

hinit <- "hinit"
init_par <- c("hinit", "NO3init", "NH4init")
if (version_num >= 10) {
  hinit <- "Hinitf"
  init_par <- c("Hinitf", "NO3initf", "NH4initf")
}

test_that("multiple values from single node", {
  val <- unlist(get_param_xml(xml_path, hinit))
  expect_equivalent(val, c(23.5, 21.6, 23.9, 27.6, 0))
})

test_that("multiple values from multiple nodes", {
  val <- unlist(get_param_xml(xml_path, init_par), use.names = FALSE)
  expect_equal(val, c(
    c(23.5, 21.6, 23.9, 27.6, 0),
    c(32, 12, 9, 0, 0), c(0, 0, 0, 0, 0)
  ))
})
