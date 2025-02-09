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

# TODO: Improve testing for choices.
test_that("`choices` prints expected first choice.", {
  prefs <- preferences(rankings, format = "ranking")
  expect_output(print(choices(prefs)), "\\{1\\} from \\{1, 2, 3\\}")
})
