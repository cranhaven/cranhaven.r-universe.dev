item_names <- c("A", "B", "C")

rankings <- matrix(
  c(
    1, 2, 3,
    3, 2, 1,
    2, 1, 3
  ),
  nrow = 3,
  byrow = TRUE
)
colnames(rankings) <- item_names

test_that("`aggregate.preferences` is inverse to `as.preferences`", {
  netflix <- read_preflib("../data/netflix00004-00000101.soc")
  expect_true(all(netflix == aggregate(as.preferences(netflix))))
})

test_that("Aggregating `preferences` with duplicates and `frequencies` works", {
  prefs <- preferences(rankings, format = "ranking")
  aprefs <- aggregate(rep(prefs, 2), frequencies = rep(2, 6))
  expect_true(length(aprefs$preferences) == length(prefs))
  expect_true(all(aprefs$frequencies == 4))
})

test_that("Aggregating `preferences` with no duplicates or `frequencies`", {
  prefs <- preferences(rankings, format = "ranking")
  aprefs <- aggregate(prefs)
  expect_true(length(aprefs$preferences) == length(prefs))
  expect_true(all(aprefs$frequencies == 1))
})

test_that("`aggregated_preferences` frequencies access via frequencies()", {
  prefs <- preferences(rankings, format = "ranking")
  aprefs <- aggregate(prefs, frequencies = rep(2, 3))
  expect_true(identical(aprefs$frequencies, frequencies(aprefs)))
})

test_that("`as.matrix` is the same as with prefs but with frequencies column", {
  prefs <- preferences(rankings, format = "ranking")
  aprefs <- aggregate(prefs, frequencies = rep(2, 3))
  expect_true(all(as.matrix(prefs) == as.matrix(aprefs)[, 1:3]))
  expect_true(identical(as.matrix(aprefs)[, 4], frequencies(aprefs)))
})

test_that("`rbind` on `aggregated_preferences` adds the frequencies", {
  prefs <- preferences(rankings, format = "ranking")
  aprefs <- aggregate(prefs, frequencies = rep(2, 3))
  expect_true(identical(frequencies(rbind(aprefs, aprefs)), rep(4, 3)))
})

test_that("`[.aggregated_preferences` produces valid preferences", {
  prefs <- preferences(rankings, format = "ranking", aggregate = TRUE)
  expect_true(all(prefs[] == prefs))
  expect_true(all(prefs[1:3] == prefs))
  expect_true(all(prefs[, 1] == prefs[, "A"]))
})
