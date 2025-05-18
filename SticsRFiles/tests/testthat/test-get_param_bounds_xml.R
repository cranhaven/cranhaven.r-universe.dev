library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version


context("Researching parameter bounds")

xml_file <- file.path(
   get_examples_path("xml", stics_version = stics_version),
   "sols.xml")
plt_file <- file.path(
   get_examples_path("xml", stics_version = stics_version),
   "file_plt.xml")
test_that("Researching parameter bounds", {
   expect_equal(length(
      suppressWarnings(get_param_bounds_xml(xml_file, "profhum", "min"))), 2)
   expect_equal(
      length(suppressWarnings(get_param_bounds_xml(xml_file, "profhum"))), 3)

   expect_equal(length(
      suppressWarnings(get_param_bounds_xml(c(xml_file, plt_file),
                           c("profhum", "codemonocot")))),
      2)
   expect_warning(length(get_param_bounds_xml(xml_file, "profhum")))
   expect_equal(length(
      suppressWarnings(
         get_param_bounds_xml(xml_file, "profhum", c("min", "max")))), 3
   )
})
