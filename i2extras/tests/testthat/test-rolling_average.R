library(incidence2)
#skip("needs fixing")
context("rolling_average")

dates <- Sys.Date() + 1:10
groups <- sample(letters[1:3], length(dates), replace = TRUE)
dat <- data.frame(dates, groups)
x <- incidence(dat, date_index = "dates", groups = "groups")


dates <- rep(c(Sys.Date() + 1:4, Sys.Date() + 4), 3)
groups <- rep(paste0("groups", 1:3), each = 5)

dat <- data.frame(dates, groups)
x <- incidence(dat, date_index = "dates", groups = "groups")
ra <- add_rolling_average(x)

test_that("add_rolling_average works as expected", {
    expected <- rep(c(NA, NA, 1, 4/3),each=3)
    expect_equal(ra$rolling_average,  expected)
})


