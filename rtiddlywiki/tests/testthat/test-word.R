
test_that("Word to Markdown conversion without embedding images works", {

    skip_if_not(rmarkdown::pandoc_available())
    skip_if_not(.Platform$OS.type == "windows")
    # Define paths for the test Word file and output file
    test_word_file <- test_path("test-word-to-markdown.docx")
    output_file <- tempfile(fileext = ".md")

    # Check if the Word file exists
    expect_true(file.exists(test_word_file), "Test Word file should exist")

    expect_no_error(word_to_md(test_word_file, output_file, embed_images = FALSE, overwrite = TRUE))
    md <- readLines(output_file)
    expect_equal(md[1], "# Test word to markdown")
    unlink(output_file)
    expect_no_error(word_to_md(test_word_file, output_file, embed_images = TRUE, overwrite = TRUE))
    md <- readLines(output_file)
    expect_true(grepl("image/png;base64", md[11]))
    unlink(output_file)

})
