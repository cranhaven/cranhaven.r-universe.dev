library(testthat)

  Set <- R6Class("Set",
    public = list(
      initialize = function(x) private$.x <- x,
      add = function(x) {
        private$.x <- c(private$.x, x)
        invisible(self)
      },
      values = function(x) {
        unlist(private$.x)
      }
    ),
    private = list(.x = list())
  )
test_that("untyped construction", {
  # empty
  expect_silent(Dictionary$new())
  # elements same types
  expect_silent(Dictionary$new(x = list(a = 1, b = 1)))
  # elements different types
  expect_silent(Dictionary$new(x = list(a = 1, b = 2L, c = "c")))
  # not unique
  expect_error(Dictionary$new(x = list(a = 1, a = 2)), "unique")
})

test_that("typed construction", {
  # empty
  expect_silent(Dictionary$new(types = "numeric"))
  # elements same types
  expect_silent(Dictionary$new(x = list(a = 1, b = 1), types = "numeric"))
  expect_error(Dictionary$new(x = list(a = 1, b = 1), types = "integer"), "integer")
  # elements different types
  expect_error(Dictionary$new(x = list(a = 1, b = "b"), types = "numeric"),
               "All elements")
  expect_silent(Dictionary$new(x = list(a = 1, b = "b"), types = c("numeric",
                                                               "character")))
  # custom types
  expect_silent(Dictionary$new(x = list(r = Set$new(1), b = Set$new(2)),
                               types = "Set"))
})

test_that("add untyped", {
  d_untyped <- Dictionary$new(x = list(a = 1, b = 2))

  expect_silent(d_untyped$add(list(c = 3)))
  expect_error(d_untyped$add(list(c = 4)), "Some or all")
  expect_error(d_untyped$add(list(a = 3)), "Some or all")
  expect_silent(d_untyped$add(list(d = "a")))

  expect_equal_dictionary(
    Dictionary$new(x = list(a = 1, b = 2))$add(list(c = 3, d = 4)),
    Dictionary$new(x = list(a = 1, b = 2, c = 3, d = 4))
  )

  expect_equal_dictionary(
    Dictionary$new(x = list(a = 1, b = 2))$add(keys = c("c", "d"),
                                               values = 3:4),
    Dictionary$new(x = list(a = 1, b = 2, c = 3, d = 4))
  )

  expect_error(Dictionary$new()$add(), "Either")
})

test_that("add typed", {
  d_typed <- Dictionary$new(x = list(a = 1, b = 2), types = "numeric")

  expect_silent(d_typed$add(list(c = 3)))
  expect_error(d_typed$add(list(c = 4)), "Some or all")
  expect_error(d_typed$add(list(a = 3)), "Some or all")
  expect_error(d_typed$add(list(d = "a")), "numeric")

  expect_equal_dictionary(
    Dictionary$new(x = list(a = 1, b = 2), types = "numeric")$add(list(c = 3,
                                                                   d = 4)),
    Dictionary$new(x = list(a = 1, b = 2, c = 3, d = 4), types = "numeric")
  )

  d <- Dictionary$new(x = list(a = 1, b = 2), types = "numeric")
  d[c("c", "d")] <- c(3, 4)

  expect_equal_dictionary(
    d,
    Dictionary$new(x = list(a = 1, b = 2, c = 3, d = 4), types = "numeric")
  )
})

test_that("remove", {
  d_typed <- Dictionary$new(x = list(a = 1, b = 2), types = "numeric")
  d_untyped <- Dictionary$new(x = list(a = 1, b = 2))

  expect_error(d_typed$remove("c"), "does not exist")
  expect_error(d_untyped$remove("c"), "does not exist")

  expect_equal_dictionary(d_typed$remove(letters[1:2]),
               Dictionary$new(types = "numeric"))
  expect_equal_dictionary(d_untyped$remove(letters[1:2]), Dictionary$new())

  expect_silent(d_untyped$add(list(a = 1, b = 2)))
  expect_equal_dictionary(
    d_untyped$remove("a"),
    Dictionary$new(x = list(b = 2))
  )
})

test_that("get", {
  d_untyped <- Dictionary$new(x = list(a = 1, b = "a"))
  expect_equal(d_untyped$get("a"), 1)
  expect_equal(d_untyped$get("b"), "a")
  expect_error(d_untyped$get("c"), "value for")
  expect_error(d_untyped$get(letters[1:2]), "length")

  d_typed <- Dictionary$new(x = list(a = 1, b = 2), types = "numeric")
  expect_equal(d_typed$get("a"), 1)
  expect_equal(d_typed$get("b"), 2)
  expect_equal(d_typed$get(letters[1:2]), c(1, 2))
  expect_error(d_typed$get("c"), "value for")

  d <- Dictionary$new(x = list(a = Set$new(1), b = 2))
  expect_equal(d$get("a")$values(), 1)
  expect_equal(d$get("b"), 2)

  d <- Dictionary$new(x = list(a = Set$new(1), b = Set$new(2)), types = "Set")
  expect_equal(lapply(d$get(c("a", "b")), function(x) x$values()),
               list(a = 1, b = 2))

  ## don't clone - changes s
  s <- Set$new(1)
  d <- Dictionary$new(a = s)
  d$get("a", FALSE)$add(2)
  expect_equal(s, Set$new(c(1, 2)))
  expect_equal(d$get("a")$values(), Set$new(c(1, 2))$values())

  ## clone - doesn't changes s
  s <- Set$new(1)
  d <- Dictionary$new(a = s)
  d$get("a")$add(2)
  expect_equal(s$values(), Set$new(1)$values())
  expect_equal(d$get("a")$values(), Set$new(1)$values())
  expect_equal(d$get("a")$add(2)$values(), Set$new(c(1, 2))$values())

  d <- Dictionary$new(x = list(a = Set$new(1), b = Set$new(2)), types = "Set")
  d$get(c("a", "b"), FALSE)[[1L]]$add(2)
  expect_equal(d$get("a")$values(), Set$new(c(1, 2))$values())

  d <- Dictionary$new(x = list(a = Set$new(1), b = Set$new(2)), types = "Set")
  d$get(c("a", "b"), TRUE)[[1L]]$add(2)
  expect_equal(d$get("a")$values(), Set$new(1)$values())
})

test_that("get_list", {
  d_untyped <- Dictionary$new(x = list(a = 1, b = 2))

  expect_error(d_untyped$get_list("c"), "value for")
  expect_equal(d_untyped$get_list("a"), list(a = 1))
  expect_equal(d_untyped$get_list(c("a", "b")), list(a = 1, b = 2))
  expect_equal(d_untyped[c("a", "b")], list(a = 1, b = 2))

  d <- Dictionary$new(x = list(a = Set$new(1), b = 2))
  expect_equal(d$get_list(c("a", "b"))[[1]]$values(), 1)
  expect_equal(d$get_list(c("a", "b"))[[2]], 2)
})

test_that("has", {
  d_untyped <- Dictionary$new(x = list(a = 1, b = 2))
  expect_true(d_untyped$has("a"))
  expect_false(d_untyped$has("c"))
  expect_equal(d_untyped$has(c("a", "c", "b")),
               c(a = TRUE, c = FALSE, b = TRUE))
})

test_that("has_value", {
  d_untyped <- Dictionary$new(x = list(a = 1, b = 2))
  expect_true(d_untyped$has_value(1))
  expect_false(d_untyped$has_value(3))
  expect_equal(d_untyped$has_value(1:3), c(TRUE, TRUE, FALSE))
})

test_that("rekey", {
  d_untyped <- Dictionary$new(x = list(a = 1, b = 2))
  expect_silent(d_untyped$rekey("a", "c"))
  expect_mapequal(d_untyped$items, list(c = 1, b = 2))
  expect_error(d_untyped$rekey("a", "d"), "Not all keys")
  expect_error(d_untyped$rekey("c", "b"), "already exists")
})

test_that("active", {
  d_typed <- Dictionary$new(x = list(a = 1, b = 2), types = "numeric")
  expect_setequal(d_typed$keys, c("a", "b"))
  expect_equal(d_typed$length, 2)
  expect_equal(length(d_typed), 2)

  expect_true(d_typed$typed)
  expect_false(Dictionary$new(x = list(a = 1, b = 2))$typed)

  expect_equal(d_typed$types, c("numeric"))
  expect_equal(Dictionary$new(x = list(a = 1, b = 2))$types, NULL)

  expect_setequal(d_typed$values, c(1, 2))
  expect_setequal(Dictionary$new(x = list(a = 1, b = 2))$values, list(1, 2))

  expect_mapequal(d_typed$items, list(b = 2, a = 1))
  d_typed$items$a <- 2
  expect_equal(d_typed$items, list(a = 2, b = 2))
  expect_error({ d_typed$items$a <- "c" }) # nolint
  d_typed$items$a <- NULL
  expect_equal(d_typed$items, list(b = 2))
  d_typed$items <- list(a = 3, b = 4)
  expect_mapequal(d_typed$items, list(b = 4, a = 3))

  d_typed <- Dictionary$new(x = list(a = 1, b = 2, c = 3), types = "numeric")
  d_typed$items[letters[1:2]] <- NULL
  expect_equal(d_typed$values, 3)
  d_typed$items[letters[3]] <- NULL
  expect_equal(d_typed$values, NULL)
})

test_that("print and summary", {
  d_typed <- Dictionary$new(x = list(a = 1, b = 2, c = 3), types = "numeric")
  d_untyped <- Dictionary$new(x = list(a = 1, b = 2, c = 3))

  expect_equal(as.character(d_typed), "{a: 1, b: 2, c: 3}")
  expect_equal(as.character(d_typed, 1), "{a: 1, ..., c: 3}")
  expect_equal(as.character(d_untyped, 1), "{a: 1, ..., c: 3}")

  expect_output(print(d_typed))
  expect_output(summary(d_typed))
  expect_output(summary(d_typed), "Typed dictionary of")
  expect_output(print(d_untyped))
  expect_output(summary(d_untyped))
  expect_output(summary(d_untyped), "Untyped dictionary of 3 items.")
})

test_that("concatenate", {
  a_typed <- Dictionary$new(x = list(a = 1), types = "numeric")
  b_typed <- Dictionary$new(x = list(b = 2), types = c("numeric", "integer"))
  c_typed <- Dictionary$new(x = list(c = 3), types = c("integer", "numeric"))
  d_typed <- Dictionary$new(x = list(a = 3), types = "numeric")
  e_typed <- Dictionary$new(x = list(a = 3L), types = "integer")

  a_untyped <- Dictionary$new(x = list(a = 1))
  b_untyped <- Dictionary$new(x = list(b = 2))
  c_untyped <- Dictionary$new(x = list(c = 2))
  d_untyped <- Dictionary$new(x = list(a = 2))

  expect_error(c(a_typed, b_typed), "same type")
  expect_error(c(a_typed, c_typed), "same type")
  expect_error(c(d_typed, e_typed), "same type")
  expect_error(c(a_typed, a_untyped), "typed or all")
  expect_error(c(a_typed, d_typed), "'x' must have unique")
  expect_equal_dictionary(c(b_typed, c_typed),
               Dictionary$new(x = list(b = 2, c = 3),
                              types = c("numeric", "integer")))

  expect_error(c(a_untyped, a_typed), "typed or all")
  expect_error(c(a_typed, d_typed), "'x' must have unique")
  expect_equal_dictionary(c(a_untyped, b_untyped, c_untyped),
               Dictionary$new(x = list(a = 1, b = 2, c = 2)))
})

test_that("merge", {
  a_typed <- Dictionary$new(x = list(a = 1), types = "numeric")
  c_typed <- Dictionary$new(x = list(c = 3), types = c("integer", "numeric"))
  d_typed <- Dictionary$new(x = list(a = 3), types = "numeric")

  a_untyped <- Dictionary$new(x = list(a = "b"))
  b_untyped <- Dictionary$new(x = list(b = 2))
  c_untyped <- Dictionary$new(x = list(c = 2))
  d_untyped <- Dictionary$new(x = list(a = 2))

  expect_equal(a_typed$merge(list(b_untyped, c_untyped))$items,
               list(c = 2, b = 2, a = 1))
  expect_error(a_typed$merge(a_untyped), "Some or")
  expect_error(d_typed$merge(d_untyped), "Some or")
  expect_error(a_typed$merge("a"), "Dictionary or")
})

test_that("deep clone", {
  r <- R6Class("test")
  d1 <- Dictionary$new(x = list(a = r$new(), d = 1))
  d2 <- d1$clone(deep = TRUE)
  d3 <- d1
  d2$add(list(b = 2))
  expect_equal(length(d1), length(d3))
  expect_false(length(d1) == length(d2))
})


test_that("can revalue", {
  d <- dct(a = 1)
  d$revalue("a", 2)
  expect_equal(d$get("a"), 2)

  d <- dct(a = 1, types = "numeric")
  expect_error(d$revalue("a", "b"), "inherits")
})


test_that("can add R6", {
  d <- dct(a = 1)
  d$add(keys = "b", values = Set$new(1))
  expect_setequal(d$keys, c("a", "b"))
  expect_equal(d$get("b")$values(), 1)
})
