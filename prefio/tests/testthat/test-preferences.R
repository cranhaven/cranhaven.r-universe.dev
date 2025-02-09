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

long <- data.frame(
  id = rep(1:3, each = 3),
  item = LETTERS[rep(1:3, 3)],
  rank = c(1, 2, 3, 3, 2, 1, 2, 1, 3)
)

ord <- as.data.frame(
  rbind(
    list("A", "B", "C"),
    list("C", "B", "A"),
    list("B", "A", "C")
  )
)

test_that("`preferences` can be constructed from all formats and are equal", {
  p_rank <- preferences(rankings,
    format = "ranking",
    item_names = item_names
  )
  p_long <- preferences(long,
    format = "long",
    id = "id",
    item = "item",
    rank = "rank"
  )
  p_ord <- preferences(ord,
    format = "ordering"
  )
  expect_true(all(p_rank == p_long))
  expect_true(all(p_rank == p_ord))
})

test_that("`as.preferences` is equivalent to `preferences`", {
  prefs1 <- as.preferences(ord,
    format = "ordering"
  ) # Without item_names
  prefs2 <- preferences(ord,
    format = "ordering",
    item_names = item_names
  )
  prefs3 <- as.preferences(long,
    format = "long",
    id = "id",
    rank = "rank",
    item = "item"
  ) # Without item_names
  prefs4 <- preferences(long,
    format = "long",
    id = "id",
    rank = "rank",
    item = "item",
    item_names = item_names
  )
  prefs5 <- as.preferences(rankings,
    format = "ranking"
  ) # Without item_names
  prefs6 <- preferences(rankings,
    format = "ranking",
    item_names = item_names
  )
  expect_true(all(prefs1 == prefs2))
  expect_true(all(prefs1 == prefs3))
  expect_true(all(prefs1 == prefs4))
  expect_true(all(prefs1 == prefs5))
  expect_true(all(prefs1 == prefs6))
})

test_that("`preferences` can be joined by rbind", {
  r2 <- rbind(rankings, rankings)
  p2 <- preferences(r2, format = "ranking")
  p1 <- preferences(rankings, format = "ranking")
  expect_true(all(rbind(p1, p1) == p2))

  # Permute names and rbind.
  names(p2) <- c("C", "A", "B")
  p <- rbind(p1, p2)
  expect_true(all(p[1:3] == p1))
})

test_that("Constructing `preferences` with `aggregate=T` aggregates output", {
  r2 <- rbind(rankings, rankings)
  apref <- preferences(r2, format = "ranking", aggregate = TRUE)
  expect_true("aggregated_preferences" %in% class(apref))
  expect_true(all(apref$frequencies == 2))
})

test_that("Constructing `preferences` with `frequencies` aggregates output", {
  apref <- preferences(rankings, format = "ranking", frequencies = rep(2, 3))
  expect_true("aggregated_preferences" %in% class(apref))
  expect_true(all(apref$frequencies == 2))
})

test_that("`[.preferences` as orderings is inverse to `as.preferences`", {
  prefs <- preferences(rankings, format = "ranking")
  prefs_as_ord <- prefs[, as.ordering = TRUE]
  prefs_as_ord_as_prefs <- as.preferences(prefs_as_ord, format = "ordering")
  expect_true(all(prefs == prefs_as_ord_as_prefs))
})

test_that("`[.preferences` produces valid preferences when subsetting", {
  prefs <- preferences(rankings, format = "ranking")
  expect_true(all(prefs[] == prefs))
  expect_true(all(prefs[1:3] == prefs))
  expect_true(all(as.preferences(prefs[1:3, as.ordering = TRUE],
    format = "ordering"
  ) == prefs))
  expect_true(all(prefs[, 1] == prefs[, "A"]))
})

test_that("`preferences` with altered `item_names` are not equal", {
  prefs <- preferences(rankings, format = "ranking")
  prefs_new <- prefs
  names(prefs_new) <- LETTERS[4:6]
  expect_false(any(prefs == prefs_new))
})

test_that("Equality and inequality work for `preferences`", {
  prefs <- preferences(rankings, format = "ranking")
  prefs_new <- prefs
  names(prefs_new) <- LETTERS[4:6]
  expect_false(any(prefs == prefs_new))
  expect_true(all(prefs != prefs_new))
})

test_that("`print.preference` formats correctly", {
  prefs <- preferences(rankings, format = "ranking")
  expect_output(print(prefs), "\\[A > B > C\\]")
  expect_output(print(prefs), "\\[C > B > A\\]")
  expect_output(print(prefs), "\\[B > A > C\\]")
  expect_output(print(prefs[, 1]), "\\[A\\] \\[A\\] \\[A\\]")
  expect_output(
    print(prefs[, NULL]),
    "\\[blank\\] \\[blank\\] \\[blank\\]"
  )
})

test_that("Some valid examples of `preferences` are not `na`", {
  expect_true(
    !any(is.na(
      read_preflib("../data/aspen00016-00000001.toc")$preferences
    )) &&
      !any(is.na(
        read_preflib("../data/glasgow00008-00000003.soi")$preferences
      )) &&
      !any(is.na(
        read_preflib("../data/netflix00004-00000101.soc")$preferences
      )) &&
      !any(is.na(
        read_preflib("../data/berkley00017-00000001.toi")$preferences
      ))
  )
})

toc <- preferences(
  matrix(c(1, 2, NA, NA),
    nrow = 1,
    dimnames = list(NULL, LETTERS[1:4])
  ),
  format = "ranking"
)
test_that("Incomplete preferences equal themselves despite containing NAs", {
  expect_true(toc == toc)
})

test_that("Loading preferences from long format doesn't permute item names", {
  longdf <- data.frame(
    id = c(rep(1:4, each = 4)),
    item = c(
      "B", "A", "C", "D",
      "C", "B", "A", "D",
      "A", "B", "C", "D",
      "D", "B", "C", "A"
    ),
    rank = rep(1:4, 4)
  )
  prefs <- preferences(longdf,
    format = "long",
    id = "id",
    item = "item",
    rank = "rank"
  )
  expect_true(all(prefs[1, as.ordering = TRUE] == c("B", "A", "C", "D")))
  expect_true(all(prefs[2, as.ordering = TRUE] == c("C", "B", "A", "D")))
  expect_true(all(prefs[3, as.ordering = TRUE] == c("A", "B", "C", "D")))
  expect_true(all(prefs[4, as.ordering = TRUE] == c("D", "B", "C", "A")))
})

test_that("Subsetting preferences with `by.rank = TRUE` succeeds", {
  x <- matrix(
    c(
      1, 1, 1,
      1, 2, 3,
      2, 3, 1,
      3, 2, 1,
      1, 2, 3
    ),
    ncol = 3,
    byrow = TRUE
  )
  prefs <- preferences(x, format = "ranking", item_names = LETTERS[1:3])
  expect_equal(prefs[1, 1, by.rank = TRUE], prefs[1])
  expect_equal(prefs[2, 1, by.rank = TRUE], prefs[5, 1, by.rank = TRUE])
  expect_equal(prefs[3, 3, by.rank = TRUE], prefs[4, 2, by.rank = TRUE])
})

test_that("Using `frequencies` argument with long-format data yields warning", {
  expect_warning(
    preferences(
      long,
      format = "long",
      id = "id",
      item = "item",
      rank = "rank",
      frequencies = rep(2, 6)
    )
  )
})

test_that("Loading \"ranking\" format with no names yields warning", {
  expect_warning(preferences(unname(rankings), format = "ranking"))
})

test_that("Calling `preferences` with nonsense format raises error", {
  expect_error(preferences(rankings, format = "nonsense"))
})

test_that("Calling `preferences` with `item_names` missing item raises error", {
  expect_error(
    preferences(
      long,
      format = "long",
      id = "id",
      item = "item",
      rank = "rank",
      item_names = LETTERS[1:2]
    )
  )
  expect_error(
    preferences(rankings, format = "ranking", item_names = LETTERS[1:2])
  )
  expect_error(
    preferences(ordering, format = "ordering", item_names = LETTERS[1:2])
  )
})

test_that("preferences from long format without id item or rank throws error", {
  expect_error(
    preferences(long, format = "long", item = "item", id = "id")
  )
  expect_error(
    preferences(long, format = "long", item = "item", rank = "rank")
  )
  expect_error(
    preferences(long, format = "long", rank = "rank", id = "id")
  )
})

test_that("Empty preferences can be created by `preferences`", {
  expect_success({
    prefs <- preferences(
      matrix(ncol = 4, nrow = 0),
      format = "ranking",
      item_names = LETTERS[1:4]
    )
    expect_true(length(prefs) == 0)
  })
  expect_success({
    prefs <- preferences(
      matrix(ncol = 3, nrow = 0, dimnames = list(NULL, c("id", "itm", "rnk"))),
      format = "long",
      id = "id",
      item = "itm",
      rank = "rnk",
      item_names = LETTERS[1:4]
    )
    expect_true(length(prefs) == 0)
  })
})

test_that("Formatting of empty preferences object shows `preferences(0)`", {
  prefs <- preferences(
    matrix(ncol = 4, nrow = 0, dimnames = list(NULL, LETTERS[1:4])),
    format = "ranking"
  )
  expect_output(print(prefs), "preferences\\(0\\)")
})

e <- character()
test_that("Constructing preferences from orderings with index and name works", {
  prefs <- preferences(
    as.data.frame(
      rbind(
        list("A", "B", e),
        list(1, 2, e)
      )
    ),
    format = "ordering",
    item_names = c("A", "B")
  )
  # Both rows are evaluated to be equal and then aggregated to one entry with
  # frequency 2
  expect_true(aggregate(prefs)$frequencies == 2)
})
