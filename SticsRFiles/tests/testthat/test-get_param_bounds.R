library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version


context("Reaserching parameter bounds")

xml_file <- file.path(get_examples_path("xml", stics_version = stics_version),
                      "sols.xml")
xml_doc <- xmldocument(xml_file)
test_that("Researching parameter bounds", {
   expect_equal(length(get_param_bounds(xml_doc, "profhum", "min")), 2)
   expect_equal(length(
      suppressWarnings(get_param_bounds(xml_doc, "profhum"))), 3)
   expect_warning(length(get_param_bounds(xml_doc, "profhum")))
   expect_equal(
     length(
        suppressWarnings(
           get_param_bounds(xml_doc, "profhum", c("min", "max")))
        ),
     3)
})
