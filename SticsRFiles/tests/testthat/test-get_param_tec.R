library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

xml_path <- file.path(
  get_examples_path("xml", stics_version = stics_version),
  "file_tec.xml"
)
context("Getting tec param values")

test_that("single value from single node", {
  expect_equivalent(unlist(get_param_xml(xml_path, "irecbutoir")), 300)
})

test_that("double associated values from single node", {
  expect_equivalent(
    unname(unlist(get_param_xml(xml_path, c("julapI_or_sum_upvt", "amount")))),
    c(
      c(
        178, 185, 193, 198, 200, 204, 207, 211, 214, 218, 221, 232, 239, 249,
        257, 264
      ),
      c(20, 24, 29, 29, 28, 31, 21, 21, 23, 18, 22, 16, 16, 30, 29, 20)
    )
  )
})
