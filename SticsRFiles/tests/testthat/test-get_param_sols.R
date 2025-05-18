library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

xml_path <- file.path(
  get_examples_path("xml", stics_version = stics_version),
  "sols.xml"
)
context("Getting grounds param values")

res <- c(
  30.2, 21.0, 27.0, 39.0, 1.0, 12.2, 70.0, 22.0, 9.9, 10.2, 10.2, 17.0, 23.1,
  22.0, 27.0, 30.7, 0.1, 27.3, 25.0, 10.2, 25.0, 28.6, 36.0, 29.0, 10.2, 21.2,
  22.2, 13.0, 17.0, 15.0, 26.0, 28.2, 20.0
)

test_that("single param option value version sols -> argi", {
  expect_equivalent(get_param_xml(xml_path, "argi")[[1]]$argi, res)
  expect_equivalent(get_param_xml(xml_path, "argi")[[1]]$argi[13], 23.1)
})


li1 <- c(
  0.270, 0.100, 0.210, 0.150, 0.700, 0.110, 0.200, 0.120, 0.070, 0.160, 0.180,
  0.210, 0.140, 0.120, 0.140, 0.140, 0.070, 0.080, 0.200, 0.180, 0.100, 0.120,
  0.150, 0.120, 0.140, 0.070, 0.110, 0.100, 0.150, 0.080, 0.088, 0.088, 0.100
)

li2 <- c(
  40.0, 31.0, 27.0, 27.0, 40.0, 30.0, 50.0, 35.0, 50.0, 30.0, 30.0, 27.0, 22.0,
  35.0, 30.0, 30.0, 30.0, 30.0, 30.0, 28.0, 30.0, 40.0, 40.0, 30.0, 31.5, 30.0,
  30.0, 30.0, 15.0, 35.0, 25.0, 25.0, 20.0
)

res <- c(li1, li2)

# unname(query) because result is just numbers, it doesn't have names
test_that("multiple param option value version sols -> norg, profhum", {
  expect_equivalent(unname(unlist(get_param_xml(
    xml_path,
    c("norg", "profhum")
  ))), res)
  query <- unlist(lapply(
    get_param_xml(xml_path, c("norg", "profhum"))[[1]],
    function(x) x[[1]][1]
  ))
  expect_equivalent(unname(query), c(0.27, 40.00))
})

test_that("single option param name choice value", {
  expect_equivalent(get_param_xml(
    xml_path,
    "coderemontcap"
  )[[1]]$coderemontcap[1], 2)
})


test_that(" single column name value", {
  expect_equivalent(get_param_xml(xml_path, "epc")[[1]]$epc[2], 20)
})

test_that(
  "multiple option choice value and param name's value from choice list",
  {
    expect_equivalent(unname(unlist(lapply(
      get_param_xml(xml_path, c("codefente", "profimper"))[[1]],
      function(x) x[[1]][1]
    ))), c(2, 10))
  }
)

test_that("triple param name's value ", {
  expect_equivalent(unname(unlist(lapply(
    get_param_xml(xml_path, c("argi", "norg", "pH"))[[1]],
    function(x) x[[1]][1]
  ))), c(30.2, 0.27, 7))
})

test_that("quadruple param's name value from differents sections", {
  expect_equivalent(unname(unlist(lapply(
    get_param_xml(xml_path, c(
      "obstarac", "humcapil", "codenitrif",
      "HCCF"
    ))[[1]],
    function(x) x[[2]][1]
  ))), c(155, 10, 2, 46.40))
})
