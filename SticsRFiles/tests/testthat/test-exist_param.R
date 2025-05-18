stics_version <- get_stics_versions_compat()$latest_version
xml_sols <- file.path(get_examples_path(file_type = "xml"), "sols.xml")

xml_doc <- xmldocument(xml_sols)

exists_param(xml_doc, "cfes")
exists_param(xml_doc, c("cfes", "mulchbat"))
context("Exist param ")

test_that("exist parameter", {
  expect_true(exists_param(xml_doc, "cfes"))
  expect_vector(exists_param(xml_doc, c("cfes", "mulchbat")),
                ptype = NULL, size = 2)

})
