context("Time methods")

test_that("timestamp or julienday", {
    expect_equal(is.timestamp(125), FALSE)
    expect_equal(is.juliendate(125), TRUE)
    expect_equal(is.timestamp(31536000), TRUE)
    expect_equal(is.juliendate(31536000), FALSE)
    expect_equal(is.timestamp("1971-01-01"),FALSE)
})

test_that("string is date", {
    expect_equal(is.StringDate("1971-01-01T00:00:00"), TRUE)
    expect_equal(is.StringDate("19710101"), FALSE)
})

test_that("timestamp to date", {
    expect_equal(getDate(31536000), "1971-01-01T00:00:00")
    expect_equal(getDate("1971-01-01"), FALSE)
})

test_that("date to timestamp", {
    expect_equal(getTimestamp("1971-01-01T00:00:00"), 31536000)
    expect_equal(getTimestamp(c("1971-01-01T00:00:00","1971-01-01T00:00:00")), c(31536000,31536000) )
    expect_equal(is.na(getTimestamp("NA")), TRUE)
})