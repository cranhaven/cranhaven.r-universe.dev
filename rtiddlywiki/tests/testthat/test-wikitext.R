test_that("wikitext", {
    x <- c("AAA", "BBB")
    expect_equal(wikitext_backticks_return(x), "`AAA`, `BBB`")
})


