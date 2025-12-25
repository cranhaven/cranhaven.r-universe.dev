test_that("Test break_join error handling", {
  expect_error(break_join(a, b, brk = 1:10), regexp = "must be length 1 chr")
  expect_error(break_join(a, b, brk = 1:10, foo = 3), regexp = "not found in findInterval, left_join")
  expect_error(break_join(tibble(d = 1:10, d.x = 1:10, d.x = 1:10, .name_repair = "minimal"), tibble(d = 1:10), brk = "d"), regexp = "must be unique")
  expect_error(break_join(tibble(d = 1:10, d.x = 1:10, .name_repair = "minimal"), tibble(d = 1:10, d = 1:10), brk = "d"), regexp = "duplicate")
})

test_that("Test break_join handles evil input", {
  expect_true(nrow(break_join(a, b, brk = "by")) > 0)
  expect_equal(break_join(a, b, brk = "by"), break_join(a, b, brk = "by", by = c(".bj1", ".bj2")))
  expect_equal(names(break_join(g, h, brk = "c", by = character())), c("a", "b.x", "c", "b.y"))
})

test_that("Test break_join produces expected output", {
  expect_equal(break_join(a, b, brk = "by", by = c(".bj1", ".bj2"))$.brk,
               c(rep(NA_character_, 3), rep("A", 3), rep("B", 4)))
  expect_equal(break_join(a, b, brk = "by", by = c(".bj1", ".bj2"), all.inside = TRUE)$.brk,
               rep("A", 10))
})

test_that("Test break_join empty output recognition", {
  expect_equal(break_join(a, b[0,], brk = "by")$.brk, rep(NA_character_, 10))
  expect_equal(nrow(break_join(a[0,], b, brk = "by")), 0)
})

test_that("Test break_join grouped df handling", {
  expect_equal(dplyr::ungroup(break_join(e, f, brk = "q")), break_join(dplyr::ungroup(e), dplyr::ungroup(f), brk = "q"))
})
