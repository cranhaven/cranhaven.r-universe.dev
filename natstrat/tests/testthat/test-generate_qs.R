z <- c(rep(0, 15), rep(1, 5))
data <- data.frame(color = c(rep("Red", 5), rep("White", 2), rep("Blue", 5), rep("White", 4), rep("Red", 4)),
                   number = 1:20,
                   category = c("1", "1", "1", rep(c( "2", "3"), 6), "1", rep("2", 2), "3", "1"))
data$number[c(1, 5, 11, 16)] <- NA
constraints <- suppressWarnings(generate_constraints(list(color + number ~ 2 * category), z, data = data,
                                                     autogen_missing = 4))
strata_dist <- matrix(c(0, 2, 1, 2, 0, 1, 1, 1, 0), byrow = TRUE, ncol = 3, dimnames = list(c("1", "2", "3"), c("1", "2", "3")))
q_s <- generate_qs(z, st = data$category, ratio = 2.5, max_ratio = NULL, max_extra_s = NULL, strata_dist = strata_dist)

test_that("EMD qs are correct", {
  expect_equal(as.numeric(q_s[1, ]), c(3, 5, 4))
})
