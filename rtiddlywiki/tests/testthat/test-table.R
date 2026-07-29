
# Define test cases
test_that("read_table correctly parses a table with a header", {
    table <- "|!Col1 |!Col2 |\n|A |B |\n|C |D |"
    df <- read_table(table, header = TRUE)

    expect_equal(colnames(df), c("Col1", "Col2"))
    expect_equal(nrow(df), 2)
    expect_equal(df[1, 1], c("A"))
    expect_equal(df[2, 1], c("C"))


    table <- "This is an extra line\n|!Col1 |!Col2 |\n|A |B |\n|C |D |\nthis is another line"
    df <- read_table(table, header = TRUE)

    expect_equal(colnames(df), c("Col1", "Col2"))
    expect_equal(nrow(df), 2)
    expect_equal(df[1, 1], c("A"))
    expect_equal(df[2, 1], c("C"))

})

test_that("read_table correctly parses a table without a header", {
    table <- "|A |B |\n|C |D |"
    df <- read_table(table, header = FALSE)

    expect_equal(nrow(df), 2)
    expect_equal(ncol(df), 2)
    expect_equal(df[1, 1], c("A"))
    expect_equal(df[2, 1], c("C"))
})

test_that("read_table handles empty input", {
    table <- ""
    expect_error(df <- read_table(table, header = TRUE))
})


test_that("kable_html converts data frame to HTML table", {
    df <- data.frame(Col1 = c("A", "B"), Col2 = c("C", "D"))
    html_table <- kable_html(df)

    expect_true(grepl("<table", html_table))
    expect_true(grepl("<th.*> *Col1 *</th>", html_table))
    expect_true(grepl("<td.*> *B *</td>", html_table))
    expect_true(grepl("<td.*> *C *</td>", html_table))
    expect_error(kable_html("not a data frame"))
})
