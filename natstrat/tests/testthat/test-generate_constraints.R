z <- c(rep(0, 10), rep(1, 5))
data <- data.frame(color = c(rep("Red", 5), rep("White", 2), rep("Blue", 3), rep("White", 2), rep("Red", 3)),
                   number = 1:15,
                   category = c(rep(c("1", "2"), 5), "1", rep("2", 3), "1"))
data$number[c(1, 5, 11)] <- NA

test_that("constraint for numeric covariate in population looks good", {
  X <- suppressWarnings(generate_constraints(list(color + number ~ category), z, data = data, treated = 1)$X)
  expect_equal(X[, "number_1"],
               c(NA, 1.549193,  2.323790,  3.098387, NA,  4.647580,
                 5.422177,  6.196773,  6.971370,  7.745967,
                 NA,  9.295160, 10.069757, 10.844353, 11.618950),
               tolerance = .000001)
})

test_that("constraint for numeric covariate in stratum looks good", {
  X <- suppressWarnings(generate_constraints(list(color + number ~ category), z, data = data)$X)
  expect_equal(X[, "number_category1"],
               c(NA,  0, 2.323790  ,  0,  NA,
                 0, 5.422177  ,  0, 6.971370  ,  0,
                 NA, 0,  0,  0,  11.618950),
               tolerance = .000001)
})

test_that("constraint for missingness looks good", {
  X <- suppressWarnings(generate_constraints(list(color + number ~ category), z, data = data)$X)
  expect_equal(X[, "number_missing_1"],
               c(2.236068 , 0, 0, 0,  2.236068 ,
                 0, 0, 0, 0, 0,
                 2.236068 , 0, 0, 0, 0),
               tolerance = .000001)
})

test_that("constraint for categorical variable looks good", {
  X <- suppressWarnings(generate_constraints(list(color + number ~ category), z, data = data)$X)
  expect_equal(X[, "colorRed_1"],
               c(rep(1.825742 , 5),
                 rep(0, 7),
                 rep(1.825742 , 3)),
               tolerance = .000001)
})

test_that("constraint with no treated variance looks good", {
  X <- suppressWarnings(generate_constraints(list(color + number ~ category), z, data = data)$X)
  expect_equal(X[, "colorBlue_1"],
               c(rep(0, 7),
                 rep(2.9277, 3),
                 rep(0, 5)),
               tolerance = .000001)
})


test_that("changing to pooled variance functions", {
  X <- generate_constraints(list(color + number ~ category), z, data = data,
                                             denom_variance = 'pooled')$X
  expect_equal(X[, "number_1"],
               c(NA, 0.8909866, 1.3364798, 1.7819731 , NA, 2.6729597, 3.1184530,
                 3.5639463, 4.0094395, 4.4549328, NA, 5.3459194, 5.7914127, 6.2369060, 6.6823992),
               tolerance = .000001)
})


test_that("importances don't change constraints", {
  X <- suppressWarnings(generate_constraints(list(color + number ~ category), z, data = data)$X)
  Y <- suppressWarnings(generate_constraints(list(2 * color + 3 * number ~ 4 + 2 * category), z, data = data,
                                             weight_by_size = 1)$X)
  expect_equal(X, Y)
})

test_that("lhs importances work", {
  importances <- suppressWarnings(generate_constraints(list(color + 3 * number ~ category), z, data = data)$importances)
  expect_equal(as.numeric(importances), c(3, 150, 3, 150, 3, 150, rep(1, 9)))
})

test_that("rhs importances work", {
  importances <- suppressWarnings(generate_constraints(list(color + number ~ 2 * category), z, data = data)$importances)
  expect_equal(as.numeric(importances), c(1, 50, 2, 100, 2, 100, 1, 2, 2, 1, 2, 2, 1, 2, 2))
})

test_that("stratum size importances work", {
  importances <- suppressWarnings(generate_constraints(list(color + number ~ category),
                                                       z, data = data, weight_by_size = 1)$importances)
  expect_equal(as.numeric(importances), c(1, 50, 1.25, 62.5, .8333333, 41.6666667,
                                          1, 1.25, .8333333, 1, 1.25, .8333333, 1, 1.25, .8333333))
})

test_that("missingness importances work", {
  importances <- suppressWarnings(generate_constraints(list(color + number ~ category),
                                                       z, data = data, autogen_missing = 10)$importances)
  expect_equal(as.numeric(importances), c(1, 10, 1, 10, 1, 10, rep(1, 9)))
})

test_that("importances multiply", {
  importances <- suppressWarnings(generate_constraints(list(3 * color + number ~ 2 * category),
                                                       z, data = data, weight_by_size = 1,
                                                       autogen_missing = 10)$importances)
  expect_equal(as.numeric(importances), c(1, 10, 2.5, 25, 1.666667, 16.666667,
                                          3, 7.5, 5, 3, 7.5, 5, 3, 7.5, 5),
               tolerance = .0000001)
})

