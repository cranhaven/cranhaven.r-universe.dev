# Count data ----

test_that("init_aberr_table for count data works", {
  num_doses <- 10
  num_aberrs <- as.numeric(5) + 1

  # Doses data frame
  data_doses <- data.frame(
    D = rep(0.0, num_doses)
  )

  # Base data frame
  data_base <- data.frame(
    matrix(
      0,
      nrow = num_doses,
      ncol = num_aberrs
    )
  ) %>%
    `colnames<-`(paste0("C", seq(0, num_aberrs - 1, 1)))

  # Full data frame
  full_data <- cbind(data_doses, data_base) %>%
    dplyr::mutate(
      D = as.numeric(.data$D)
    )

  full_data <- init_aberr_table(
    data = full_data,
    type = "count",
    aberr_module = NULL
  )

  # Expected outputs
  expect_equal(names(full_data), c("D", "N", "X", "C0", "C1", "C2", "C3", "C4", "C5", "mean", "var", "DI", "u"))
  expect_equal(unname(unlist(full_data)), rep(0, num_doses * 13))
  expect_true(all(dim(full_data) == c(num_doses, 13)))
})


# Case data ----

test_that("init_aberr_table for dicentrics case data works", {
  num_cases <- 1
  num_aberrs <- as.numeric(5) + 1

  # Base data frame
  full_data <- data.frame(
    matrix(
      0,
      nrow = num_cases,
      ncol = num_aberrs
    )
  ) %>%
    `colnames<-`(paste0("C", seq(0, num_aberrs - 1, 1)))

  full_data <- init_aberr_table(
    data = full_data,
    type = "case",
    aberr_module = "dicentrics"
  )

  # Expected outputs
  expect_equal(names(full_data), c("N", "X", "C0", "C1", "C2", "C3", "C4", "C5", "y", "y_err", "DI", "u"))
  expect_equal(unname(unlist(full_data)), rep(0, num_cases * 12))
  expect_true(all(dim(full_data) == c(num_cases, 12)))
})

test_that("init_aberr_table for translocations case data works", {
  num_cases <- 1
  num_aberrs <- as.numeric(5) + 1

  # Base data frame
  full_data <- data.frame(
    matrix(
      0,
      nrow = num_cases,
      ncol = num_aberrs
    )
  ) %>%
    `colnames<-`(paste0("C", seq(0, num_aberrs - 1, 1)))

  full_data <- init_aberr_table(
    data = full_data,
    type = "case",
    aberr_module = "translocations"
  )

  # Expected outputs
  expect_equal(names(full_data), c("N", "X", "C0", "C1", "C2", "C3", "C4", "C5", "Fp", "Fp_err", "DI", "u", "Xc", "Fg", "Fg_err"))
  expect_equal(unname(unlist(full_data)), rep(0, num_cases * 15))
  expect_true(all(dim(full_data) == c(num_cases, 15)))
})
