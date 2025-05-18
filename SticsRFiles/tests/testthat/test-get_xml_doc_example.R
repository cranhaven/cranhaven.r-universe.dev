
stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

context("get example doc")
usm_doc <- get_xml_doc_example("usms.xml")

test_that("get example doc", {
  expect_equal(usm_doc@name, "usms.xml")
  expect_error(get_xml_doc_example("usm.xml"))
  expect_equal(length(unlist(get_xml_doc_example())), 8)
})
