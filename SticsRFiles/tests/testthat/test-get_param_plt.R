
stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

xml_path <- file.path(
  get_examples_path("xml", stics_version = stics_version),
  "file_plt.xml"
)
context("Getting param values")


test_that("single param option value", {
  expect_equivalent(get_param_xml(xml_path, "codetemp"), 1)
  expect_equivalent(get_param_xml(xml_path, "codegdh"), 1)
  expect_equivalent(get_param_xml(xml_path, "codephot"), 2)
})

test_that("single param value", {
  expect_equivalent(get_param_xml(xml_path, "jvcmini"), -999)
  expect_equivalent(get_param_xml(xml_path, "innsen"), -999)
  expect_equivalent(get_param_xml(xml_path, "efcroiveg"), 4.25000)
})

test_that("two param option values, and order", {
  r <- unlist(get_param_xml(xml_path, c("codetremp", "codegdh"))[[1]])
  names(r) <- NULL
  expect_equal(r, c(2, 1))
  r <- unlist(get_param_xml(xml_path, c("codegdh", "codetremp"))[[1]])
  names(r) <- NULL
  expect_equal(r, c(1, 2))
})


test_that("a param option value and a param value, and order", {
  r <- unlist(get_param_xml(xml_path, c("codetemp", "jvcmini"))[[1]])
  names(r) <- NULL
  expect_equal(r, c(1, -999))
  r <- unlist(get_param_xml(xml_path, c("jvcmini", "codetemp"))[[1]])
  names(r) <- NULL
  expect_equal(r, c(-999, 1))
})
