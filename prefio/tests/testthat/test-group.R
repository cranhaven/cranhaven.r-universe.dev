item_names <- c("A", "B", "C")

rankings <- matrix(
  c(
    1, 2, 3,
    1, 3, 2,
    2, 1, 3,
    2, 3, 1,
    3, 1, 2,
    3, 2, 1
  ),
  nrow = 6,
  byrow = TRUE
)
colnames(rankings) <- item_names

test_that("Can construct `grouped_preferences` from `preferences`", {
  prefs <- preferences(rankings, format = "ranking")
  gprefs <- group(prefs, rep(1:3, each = 2))
  expect_true(length(gprefs) == 3)
  expect_true(all(attr(gprefs[1], "preferences") == prefs[1:2]))
  # None are NA
  expect_true(all(gprefs == na.omit(gprefs)))
})

test_that("`preferences` can be grouped by a factor and accessed by name", {
  prefs <- preferences(rankings, format = "ranking")
  gprefs <- group(
    prefs,
    as.factor(rep(paste("Group", LETTERS[24:26]), each = 2))
  )
  expect_true(all(attr(gprefs["Group X"], "preferences") == prefs[1:2]))
  expect_true(all(attr(gprefs["Group Y"], "preferences") == prefs[3:4]))
  expect_true(all(attr(gprefs["Group Z"], "preferences") == prefs[5:6]))
})

test_that("`print.grouped_preferences` formats correctly", {
  prefs <- preferences(rankings, format = "ranking")
  gprefs <- group(
    prefs,
    as.factor(rep(paste("Group", LETTERS[24:26]), each = 2))
  )
  expect_output(print(gprefs), "Group X")
  expect_output(print(gprefs), "Group Y")
  expect_output(print(gprefs), "Group Z")
  expect_output(print(gprefs), "\\[A > B > C\\], \\[A > C > B\\]")
  expect_output(print(gprefs), "\\[B > A > C\\], \\[C > A > B\\]")
  expect_output(print(gprefs), "\\[B > C > A\\], \\[C > B > A\\]")
  expect_output(print(gprefs[, 1]), "\\[A\\], \\[A\\]")
  expect_output(print(gprefs[, NULL]), "\\[blank\\], \\[blank\\]")
})

test_that("`as.data.frame` produces one row of grouped_preferences per index", {
  prefs <- preferences(rankings, format = "ranking")
  gprefs <- group(
    prefs,
    as.factor(rep(paste("Group", LETTERS[24:26]), each = 2))
  )
  df <- as.data.frame(gprefs)
  expect_true(all(dim(df) == c(3, 1)))
  expect_true(class(df[1, ]) == "grouped_preferences")
})
