stics_version <- get_stics_versions_compat()$latest_version

workspace <- get_examples_path(file_type = "xml", stics_version = stics_version)
xml_plt <- file.path(get_examples_path(file_type = "xml"), "file_plt.xml")
xsl_file <- file.path(get_examples_path(file_type = "xsl"), "xml2txt.xsl")
out_dir <- file.path(tempdir(), "Test_Convert")
if (!dir.exists(out_dir)) dir.create(out_dir)
convert_xml2txt_int(xml_file = xml_plt,
                                  style_file = xsl_file,
                                  file.path(out_dir, "ficplt.txt"))

context("Convert file type")

test_that("Convert xml file to txt file", {
  expect_true(file.exists(file.path(out_dir, "ficplt.txt")))
})
