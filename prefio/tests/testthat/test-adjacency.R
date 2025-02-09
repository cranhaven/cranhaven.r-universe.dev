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

test_that("`adjacency` works on `preferences`", {
  prefs <- preferences(rankings, format = "ranking")
  adj <- adjacency(prefs)
  # Make sure all entries are >= 0
  expect_true(all(adj >= 0))
  # Check some entries
  expect_true(adj["A", "B"] == 1)
  expect_true(adj["B", "A"] == 2)
})

test_that("`adjacency` also works on `aggregated_preferences`", {
  prefs <- preferences(rankings, format = "ranking")
  aprefs <- aggregate(prefs, frequencies = rep(2, 3))
  adj <- adjacency(aprefs)
  # Make sure all entries are >= 0
  expect_true(all(adj >= 0))
  # Check some entries
  expect_true(adj["A", "B"] == 2)
  expect_true(adj["B", "A"] == 4)
})
