library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

context("Reaserching option names")

xml_path <- file.path(get_examples_path(file_type = "xml"), "file_plt.xml")

test_that("Reaserching option names", {
   expect_equal(length(get_options_names(xml_path)), 45)
   expect_equal(
     length(get_options_names(xml_path, c("codemonocot", "codlainet"))),
     2)
})
