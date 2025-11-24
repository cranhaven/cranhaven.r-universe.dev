# Test the behavior of the smooth formula functions

test_that("Low - level matrix functions work as expected", {

  # Time
  time_levels <- c(1991, 1992, 1993, 1994)
  pen_mat_time <- sspm:::ICAR_time(c(1991, 1992, 1993, 1994))

  pen_mat_time_compare <- matrix(c(1, -1, 0, 0,
                                   -1, 2, -1, 0,
                                   0, -1, 2, -1,
                                   0, 0, -1, 1), 4, 4)
  dimnames(pen_mat_time_compare) <- list(time_levels, time_levels)

  expect_identical(pen_mat_time, pen_mat_time_compare)
  expect_true(sum(rowSums(pen_mat_time) != 0) == 0)
  expect_true(sum(colSums(pen_mat_time) != 0) == 0)

  # Space
  pen_mat_space <- sspm:::ICAR_space(borealis_patches[1:5, ], "patch_id")

  pen_mat_space_compare <- matrix(c(2, -1, -1, 0, 0,
                                    -1, 2, -1, 0, 0,
                                    -1, -1, 3, 0, -1,
                                    0, 0, 0, 0, 0,
                                    0, 0, -1, 0, 1), 5, 5)
  names_dim <- c("P1", "P2", "P3", "P4", "P5")
  dimnames(pen_mat_space_compare) <- list(names_dim, names_dim)

  expect_identical(pen_mat_space, pen_mat_space_compare)
  expect_true(sum(rowSums(pen_mat_space) != 0) == 0)
  expect_true(sum(colSums(pen_mat_space) != 0) == 0)

})

test_that("Smooths are assembles correctly", {

  expect_identical(sspm:::assemble_smooth("s", list(a = "ns", b = 3, c = list(d = "hn", e = 4))),
                   "s(a = \"ns\", b = 3, c = list(d = \"hn\", e = 4))")

})
