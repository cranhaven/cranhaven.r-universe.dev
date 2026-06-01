moy <- function(x, y) {
    return((x + y) / 2)
}


test_that("moy works", {
    expect_identical(moy(2, 2), 2)
    expect_identical(moy(2, 0), 1)
})



test_that("moy doesn't work", {
    expect_error(moy("a", 2))
    expect_error(moy(2))
    expect_error(moy(2, "2"))
})
