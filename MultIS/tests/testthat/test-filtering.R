library(testthat)

context("File: filterFunctions.R")

test_that("convert_columnwise_relative functionallity tests", {
  dat <- matrix(
    data = c(
      0, 0, 15, 5, 3,
      2, 0, 1, 5, 3,
      3, 0, 1, 3, 0,
      0, 0, 1, 10, 2
    ),
    nrow = 4, ncol = 5, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("10", "20", "30", "40", "45")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  ret <- convert_columnwise_relative(dat)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), ncol(dat))
  expect_identical(colnames(ret), colnames(dat))
  expect_identical(rownames(ret), rownames(dat))

  expect_true(all(colSums(ret) == 1 | colSums(ret) == 0))
})

test_that("filter_atTP_min functionallity tests", {
  dat <- matrix(
    data = c(
      0, 0, 15, 5, 3,
      2, 0, 1, 5, 3,
      3, 0, 1, 3, 0,
      0, 0, 1, 10, 2
    ),
    nrow = 4, ncol = 5, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("10", "20", "30", "40", "45")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  at <- "45"
  min <- 2

  ret <- filter_at_tp_min(dat = dat, at = at, min = min)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), 3)
  expect_equal(ncol(ret), ncol(dat))
  expect_identical(colnames(ret), colnames(dat))
  expect_true(all(rownames(ret) %in% rownames(dat)))

  expect_true(all(ret[, at] >= min))

  # Substring match
  at <- "4"
  min <- 10

  ret <- filter_at_tp_min(dat = dat, at = at, min = min)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), 1)
  expect_equal(ncol(ret), ncol(dat))
  expect_identical(colnames(ret), colnames(dat))
  expect_true(all(rownames(ret) %in% rownames(dat)))

  expect_true(all(rowSums(
    ret[, grep(pattern = at, x = colnames(ret)), drop = FALSE],
    na.rm = TRUE) >= min))

  # No ISs
  at <- "45"
  min <- 5

  ret <- filter_at_tp_min(dat = dat, at = at, min = min)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), 0)
  expect_equal(ncol(ret), ncol(dat))
  expect_identical(colnames(ret), colnames(dat))
  expect_true(all(rownames(ret) %in% rownames(dat)))

  expect_true(all(ret[, at] >= min))
})

test_that("filter_atTP_biggestN functionallity tests", {
  dat <- matrix(
    data = c(
      0, 0, 15, 5, 3,
      2, 0, 1, 5, 3,
      3, 0, 1, 3, 0,
      0, 0, 1, 10, 2
    ),
    nrow = 4, ncol = 5, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("10", "20", "30", "40", "45")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  at <- "45"
  n <- 2L
  ret <- filter_at_tp_biggest_n(dat = dat, at = at, n = n)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), n)
  expect_equal(ncol(ret), ncol(dat))
  expect_identical(colnames(ret), colnames(dat))
  expect_true(all(rownames(ret) %in% rownames(dat)))

  # Substring match
  at <- "4"
  n <- 2L
  ret <- filter_at_tp_biggest_n(dat = dat, at = at, n = n)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), n + 1) # A tie between "1" and "2"
  expect_equal(ncol(ret), ncol(dat))
  expect_identical(colnames(ret), colnames(dat))
  expect_true(all(rownames(ret) %in% rownames(dat)))

  # No ISs
  at <- "45"
  n <- 0L
  ret <- filter_at_tp_biggest_n(dat = dat, at = at, n = n)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), n)
  expect_equal(ncol(ret), ncol(dat))
  expect_identical(colnames(ret), colnames(dat))
  expect_true(all(rownames(ret) %in% rownames(dat)))
})

test_that("filter_nr_tp_min functionallity tests", {
  dat <- matrix(
    data = c(
      0, 0, 15, 0, 1, # 2
      2, 0, 0, 0, 0, # 1
      3, 2, 1, NA, NA, # 3
      1, NA, 1, 10, 2
    ), # 4
    nrow = 4, ncol = 5, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("10", "20", "30", "40", "45")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  min <- 2L
  ret <- filter_nr_tp_min(dat = dat, min = min)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), 3)
  expect_equal(ncol(ret), ncol(dat))
  expect_identical(colnames(ret), colnames(dat))
  expect_true(all(rownames(ret) %in% rownames(dat)))

  # Filter out NA
  min <- 4L
  ret <- filter_nr_tp_min(dat = dat, min = min)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), 1)
  expect_equal(ncol(ret), ncol(dat))
  expect_identical(colnames(ret), colnames(dat))
  expect_true(all(rownames(ret) %in% rownames(dat)))

  # No ISs
  min <- 5L
  ret <- filter_nr_tp_min(dat = dat, min = min)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), 0)
  expect_equal(ncol(ret), ncol(dat))
  expect_identical(colnames(ret), colnames(dat))
  expect_true(all(rownames(ret) %in% rownames(dat)))
})

test_that("filter_names and filter_is_names functionallity tests", {
  dat <- matrix(
    data = NA,
    nrow = 8, ncol = 5, byrow = TRUE,
    dimnames = list(
      c(as.character(1:8)),
      c("10", "20", "30", "40", "45")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  ret <- MultIS::filter_is_names(dat = dat, by = ".")

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), ncol(dat))
  expect_identical(colnames(ret), colnames(dat))
  expect_identical(rownames(ret), rownames(dat))

  # Actually split, shortest unique prefix: is 8 characters long
  rownames(dat) <- c(
    "ABBC_12dw(-)_213",
    "ABBC_12sw(-)_213",
    "ABBC_22sw(-)_21f(+)",
    "AGBC_12sw(-)_313",
    "AFBC_12sw(-)_213",
    "AHBC_12sw(-)_213",
    "AB3C_12sw(-)_213",
    "ABB3_12sw(-)_213"
  )
  ret <- MultIS::filter_is_names(dat = dat, by = ".")

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), ncol(dat))
  expect_identical(colnames(ret), colnames(dat))
  expect_true(all(startsWith(rownames(dat), rownames(ret))))

  # Actually split, shortest unique prefix: is up to the +/-
  rownames(dat) <- c(
    "ABBC_12dw(-)_213",
    "ABBC_12sw(-)_213",
    "ABBC_22sw(-)_21f(+)",
    "AGBC_12sw(-)_313",
    "AGBC_12sw(+)_213",
    "AHBC_12sw(-)_213",
    "AB3C_12sw(-)_213",
    "ABB3_12sw(-)_213"
  )
  ret <- MultIS::filter_is_names(dat = dat)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), ncol(dat))
  expect_identical(colnames(ret), colnames(dat))
  expect_true(all(startsWith(rownames(dat), rownames(ret))))

  # Actually split, a name is double, so there exits no shortest unique prefix
  rownames(dat) <- c(
    "ABBC_12dw(-)_213",
    "ABBC_12sw(-)_213",
    "ABBC_22sw(-)_21f(+)",
    "AGBC_12sw(-)_313",
    "AGBC_12sw(-)_313",
    "AHBC_12sw(-)_213",
    "AB3C_12sw(-)_213",
    "ABB3_12sw(-)_213"
  )
  ret <- MultIS::filter_is_names(dat = dat)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), ncol(dat))
  expect_identical(colnames(ret), colnames(dat))
  expect_true(all(startsWith(rownames(dat), rownames(ret))))
})

test_that("filter_match functionallity tests", {
  dat <- matrix(
    data = c(
      2, 4, 0, 3, 4, 5,
      1, 1, 0, 3, 1, 2,
      9, 3, 0, 2, 1, 0,
      3, 9, 0, 1, 1, 9
    ),
    nrow = 4, ncol = 6, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("A_1", "A_2", "B_1", "B_2", "B_3", "C_1")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  ret <- filter_match(dat = dat, match = "1")

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), length(grep("1", colnames(dat))))
  expect_true(all(colnames(ret) %in% colnames(dat)))
  expect_equal(rownames(ret), rownames(dat))


  # Filter all rows
  dat <- matrix(
    data = c(
      2, 4, 0, 3, 4, 5,
      1, 1, 0, 3, 1, 2,
      9, 3, 0, 2, 1, 0,
      3, 9, 0, 1, 1, 9
    ),
    nrow = 4, ncol = 6, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("A_1", "A_2", "B_1", "B_2", "B_3", "C_1")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  ret <- filter_match(dat = dat, match = "foobar")

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), 0)
  expect_true(all(colnames(ret) %in% colnames(dat)))
  expect_equal(rownames(ret), rownames(dat))
  expect_null(colnames(ret))

  # Filter all rows
  dat <- matrix(
    data = c(
      2, 4, 0, 3, 4, 5,
      1, 1, 0, 3, 1, 2,
      9, 3, 0, 2, 1, 0,
      3, 9, 0, 1, 1, 9
    ),
    nrow = 4, ncol = 6, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("A_1", "A_2", "B_1", "B_2", "B_3", "C_1")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  ret <- filter_match(dat = dat, match = "")

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), ncol(dat))
  expect_equal(colnames(ret), colnames(dat))
  expect_equal(rownames(ret), rownames(dat))
})

test_that("filter_measurement_names functionallity tests", {
  dat <- matrix(
    data = NA,
    nrow = 4, ncol = 5, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("A_1_Z_999", "B_2_Y_998", "C_3_X_997", "D_4_W_996", "E_5_V_995")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  ret <- filter_measurement_names(dat = dat, elems = c(1), by = "_")

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), ncol(dat))
  for (i in seq_len(length(colnames(ret)))) {
    expect_true(grepl(colnames(ret)[i], colnames(dat)[i]))
  }

  ret <- filter_measurement_names(dat = dat, elems = c(1, 2), by = "_")

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), ncol(dat))
  for (i in seq_len(length(colnames(ret)))) {
    expect_true(grepl(colnames(ret)[i], colnames(dat)[i]))
  }

  ret <- filter_measurement_names(dat = dat, elems = c(1, 3), by = "_")

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), ncol(dat))
  for (i in seq_len(length(colnames(ret)))) {
    expect_false(grepl(colnames(ret)[i], colnames(dat)[i]))
  }

  ret <- filter_measurement_names(dat = dat, elems = c(4, 2, 1), by = "_")

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), ncol(dat))
  for (i in seq_len(length(colnames(ret)))) {
    expect_false(grepl(colnames(ret)[i], colnames(dat)[i]))
  }
})

test_that("filter_combine_measurement functionallity tests", {
  dat <- matrix(
    data = c(
      2, 4, 1, 3, 4, 5,
      1, 1, 1, 1, 1, 1,
      9, 3, 8, 2, 1, 0,
      3, 9, 8, 1, 1, 9
    ),
    nrow = 4, ncol = 6, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("A_1", "A_2", "B_1", "B_2", "B_3", "C_1")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  ret <- filter_measurement_names(dat = dat, elems = c(1), by = "_")
  ret <- filter_combine_measurements(
    dat = ret,
    pre_norm = FALSE,
    post_norm = FALSE)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), 3)

  ret <- filter_measurement_names(dat = dat, elems = c(1), by = "_")
  ret <- filter_combine_measurements(
    dat = ret,
    pre_norm = TRUE,
    post_norm = FALSE)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), 3)
  expect_equal(colSums(ret), c("A" = 2, "B" = 3, "C" = 1))

  ret <- filter_measurement_names(dat = dat, elems = c(1), by = "_")
  ret <- filter_combine_measurements(
    dat = ret,
    pre_norm = FALSE,
    post_norm = TRUE)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), 3)
  expect_equal(colSums(ret), c("A" = 1, "B" = 1, "C" = 1))
})

test_that("filter_zero_rows functionallity tests", {
  dat <- matrix(
    data = c(
      2, 4, 1, 3, 4, 5,
      0, 0, 0, 0, 0, 0,
      9, 3, 8, 2, 1, 0,
      3, 9, 8, 1, 1, 9
    ),
    nrow = 4, ncol = 6, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("A_1", "A_2", "B_1", "B_2", "B_3", "C_1")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  ret <- filter_zero_rows(dat = dat)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat) - 1)
  expect_equal(ncol(ret), ncol(dat))
  expect_equal(colnames(ret), colnames(dat))
  expect_true(all(rownames(ret) %in% rownames(dat)))

  # Filter all rows
  dat <- matrix(
    data = c(
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0
    ),
    nrow = 4, ncol = 6, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("A_1", "A_2", "B_1", "B_2", "B_3", "C_1")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  ret <- filter_zero_rows(dat = dat)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), 0)
  expect_equal(ncol(ret), ncol(dat))
  expect_equal(colnames(ret), colnames(dat))
  expect_true(all(rownames(ret) %in% rownames(dat)))
  expect_null(rownames(ret))

  # Filter all rows
  dat <- matrix(
    data = c(
      2, 4, 1, 3, 4, 5,
      0, 0, 0, 0, 0, 0,
      0, 0, NA, 0, NA, NA,
      3, 9, 8, 1, 1, 9
    ),
    nrow = 4, ncol = 6, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("A_1", "A_2", "B_1", "B_2", "B_3", "C_1")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  ret <- filter_zero_rows(dat = dat)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), 2)
  expect_equal(ncol(ret), ncol(dat))
  expect_equal(colnames(ret), colnames(dat))
  expect_true(all(rownames(ret) %in% rownames(dat)))
})

test_that("filter_zero_columns functionallity tests", {
  dat <- matrix(
    data = c(
      2, 4, 0, 3, 4, 5,
      1, 1, 0, 3, 1, 2,
      9, 3, 0, 2, 1, 0,
      3, 9, 0, 1, 1, 9
    ),
    nrow = 4, ncol = 6, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("A_1", "A_2", "B_1", "B_2", "B_3", "C_1")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  ret <- filter_zero_columns(dat = dat)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), ncol(dat) - 1)
  expect_true(all(colnames(ret) %in% colnames(dat)))
  expect_equal(rownames(ret), rownames(dat))


  # Filter all rows
  dat <- matrix(
    data = c(
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0
    ),
    nrow = 4, ncol = 6, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("A_1", "A_2", "B_1", "B_2", "B_3", "C_1")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  ret <- filter_zero_columns(dat = dat)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), 0)
  expect_true(all(colnames(ret) %in% colnames(dat)))
  expect_equal(rownames(ret), rownames(dat))
  expect_null(colnames(ret))

  # Filter all rows
  dat <- matrix(
    data = c(
      2, 4, 0, NA, 4, 5,
      2, 4, 0, 0, 1, 0,
      1, 2, 0, 0, 2, 3,
      3, 9, 0, NA, 1, 9
    ),
    nrow = 4, ncol = 6, byrow = TRUE,
    dimnames = list(
      c("1", "2", "3", "4"),
      c("A_1", "A_2", "B_1", "B_2", "B_3", "C_1")
    )
  )
  class(dat) <- c("matrix", "timeseries")

  ret <- filter_zero_columns(dat = dat)

  expect_identical(class(dat), class(ret))
  expect_equal(nrow(ret), nrow(dat))
  expect_equal(ncol(ret), 4)
  expect_true(all(colnames(ret) %in% colnames(dat)))
  expect_equal(rownames(ret), rownames(dat))
})
