valid_sim_args <- function() {
  list(
    n = 100,
    t = 10,
    p = matrix(
      c(0.3, 0.4, 0.5),
      nrow = 3,
      dimnames = list(c("control", "t1", "t2"), "all")
    ),
    blocks = NULL,
    clusters = NULL,
    control_augment = 0,
    random_assign_prop = 0,
    assignment_dates = NULL,
    delayed_feedback = FALSE,
    time_model = NULL,
    period_sizes = NULL,
    prior_periods = NULL,
    discount_rate = 1,
    dt = FALSE,
    ndraws = 500,
    r = 1,
    keep_data = FALSE,
    keep_models = FALSE,
    verbose = FALSE
  )
}

test_that("check_mab_sim accepts a valid baseline call", {
  expect_no_error(do.call(check_mab_sim, valid_sim_args()))
})

test_that("check_mab_sim rejects invalid single-argument values", {
  invalid_args <- list(
    dt = list(1, "x", NA),
    keep_data = list(1, "x", NA),
    keep_models = list(1, "x", NA),
    verbose = list(1, "x", NA),
    n = list(-1, 0, 1.5, "x"),
    ndraws = list(-1, 0, 1.5),
    r = list(-1, 0, 1.5),
    control_augment = list(-0.1, 1.1, NA_real_),
    random_assign_prop = list(-0.1, 1.1, NA_real_),
    discount_rate = list(-0.1, 1.1, NA_real_),
    time_model = list("not_a_function", 5)
  )

  purrr::walk2(invalid_args, names(invalid_args), \(vals, arg_name) {
    purrr::walk(vals, \(bad_val) {
      args <- valid_sim_args()
      args[[arg_name]] <- bad_val
      expect_error(
        do.call(check_mab_sim, args)
      )
    })
  })
})

test_that("check_mab_sim rejects t > n", {
  args <- valid_sim_args()
  args$t <- args$n + 1
  expect_error(do.call(check_mab_sim, args))
})

test_that("check_mab_sim rejects period_sizes not of length t", {
  args <- valid_sim_args()
  args$period_sizes <- rep(1, args$t + 1)
  expect_error(do.call(check_mab_sim, args))
})
test_that("check_mab_sim rejects period_sizes not of sum n", {
  args <- valid_sim_args()
  args$period_sizes <- rep(15, args$t)
  expect_error(do.call(check_mab_sim, args))
  args$period_sizes <- rep(1, args$t)
  expect_error(do.call(check_mab_sim, args))
})

test_that("check_mab_sim rejects non-Date assignment_dates", {
  args <- valid_sim_args()
  args$assignment_dates <- "2024-01-01"
  expect_error(do.call(check_mab_sim, args))
})

test_that("check_mab_sim enforces delayed_feedback requirements", {
  args <- valid_sim_args()
  args$delayed_feedback <- TRUE

  # Missing a time_model function

  expect_error(do.call(check_mab_sim, args))

  args$time_model <- function(...) NULL

  # missing assignment_dates
  expect_error(do.call(check_mab_sim, args))

  args$assignment_dates <- as.Date("2024-01-01") + 0:(args$n - 1)
  expect_no_error(do.call(check_mab_sim, args))
})

test_that("check_mab_sim warns when time_model/assignment_dates given but delayed_feedback = FALSE", {
  args <- valid_sim_args()
  args$time_model <- function(...) NULL
  args$assignment_dates <- as.Date("2024-01-01") + 0:(args$n - 1)
  expect_warning(do.call(check_mab_sim, args))
})

test_that("check_mab_sim rejects malformed p matrices", {
  args <- valid_sim_args()

  args_char <- args
  args_char$p <- matrix(
    c("0.3", "0.4", "0.5"),
    nrow = 3,
    dimnames = list(c("control", "t1", "t2"), "all")
  )
  expect_error(do.call(check_mab_sim, args_char))

  args_no_rownames <- args
  dimnames(args_no_rownames$p) <- NULL
  expect_error(do.call(check_mab_sim, args_no_rownames))

  # 1.5 > 1
  args_oob <- args
  args_oob$p[1, 1] <- 1.5
  expect_error(do.call(check_mab_sim, args_oob))

  args_multicol <- args
  args_multicol$p <- cbind(args$p, args$p)
  colnames(args_multicol$p) <- c("all", "extra")
  expect_error(do.call(check_mab_sim, args_multicol))
})

test_that("check_mab_sim validates blocks/clusters against p colnames", {
  args <- valid_sim_args()
  blocks <- c(b1 = 0.5, b2 = 0.5)
  p_blocked <- matrix(
    runif(6, 0.3, 0.6),
    nrow = 3,
    dimnames = list(c("control", "t1", "t2"), names(blocks))
  )

  args$blocks <- blocks
  args$p <- p_blocked
  expect_no_error(do.call(check_mab_sim, args))

  # mismatched colnames
  args_bad <- args
  colnames(args_bad$p) <- c("b1", "wrong")
  expect_error(do.call(check_mab_sim, args_bad))

  # blocks don't sum to 1
  args_bad2 <- args
  args_bad2$blocks <- c(b1 = 0.4, b2 = 0.4)
  expect_error(do.call(check_mab_sim, args_bad2))

  # unnamed blocks
  args_bad3 <- args
  args_bad3$blocks <- unname(args$blocks)
  expect_error(do.call(check_mab_sim, args_bad3))
})

test_that("check_p_colnames rejects mismatched labels directly", {
  p <- matrix(1, nrow = 1, dimnames = list("control", "a"))
  expect_error(check_p_colnames(p, expected = "b"))
  expect_no_error(check_p_colnames(p, expected = "a"))
})
