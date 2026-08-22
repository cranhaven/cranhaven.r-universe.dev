test_that("simulate_mab: space-filling argument and design coverage", {
  arms_control <- c("control", paste0("t", 1:5))

  cl <- stats::setNames(rep(0.1, 10), paste0("c", 1:10))
  bl <- stats::setNames(rep(0.2, 5), paste0("b", 1:5))

  sim_designs <- list(
    none = list(design = "none", group_probs = NULL),
    blocking = list(design = "blocking", group_probs = bl),
    clustering = list(design = "clustering", group_probs = cl)
  )

  common_args <- list(
    n = 500,
    t = 10,
    p = make_p(arms_control, "none", NULL)
  )

  arg_sets <- list(
    default = list(),
    ucb = list(algorithm = "ucb1"),
    random_assign = list(random_assign_prop = 0.2),
    control = list(
      control_augment = 0.2,
      control_condition = "control",
      contrasts = "best"
    ),
    discount = list(
      discount_rate = 0.9
    ),
    reps = list(
      r = 2
    ),
    contrasts = list(
      contrasts = "all",
      prior_periods = 2
    ),
    mixed1 = list(
      algorithm = "ucb1",
      random_assign_prop = 0.1,
      discount_rate = 0.85,
      contrasts = "both"
    ),
    mixed2 = list(
      algorithm = "thompson",
      control_augment = 0.15,
      r = 2
    ),
    mixed3 = list(
      random_assign_prop = 0.1,
      r = 2,
      prior_periods = 1
    ),
    mixed4 = list(
      delayed_feedback = TRUE,
      assignment_dates = seq.Date(
        as.Date("2024-01-01"),
        by = "day",
        length.out = common_args$n
      ),
      time_model = \(n, ...) rep(lubridate::days(1), n),
      random_assign_prop = 0.1
    ),
    mixed5 = list(
      algorithm = "ucb1",
      random_assign_prop = 0.4,
      control_condition = "control",
      contrasts = "control"
    ),
    mixed6 = list(
      algorithm = "thompson",
      contrasts = "all",
      period_sizes = c(100, 50, 25, 25, 40, 60, 80, 80, 20, 20),
      discount_rate = 0.5
    )
  )

  purrr::iwalk(sim_designs, \(cfg, design_name) {
    common_args$p <- make_p(arms_control, cfg$design, cfg$group_probs)
    common_args$blocks <- if (design_name == "blocking") cfg$group_probs
    common_args$clusters <- if (design_name == "clustering") cfg$group_probs

    purrr::iwalk(arg_sets, \(args, arg_name) {
      test_that(paste("simulate_mab:", design_name, arg_name), {
        seed <- 123
        set.seed(seed)

        res_df <- expect_no_error(do.call(
          simulate_mab,
          c(common_args, args, list(dt = FALSE))
        ))
        set.seed(seed)
        res_dt <- expect_no_error(do.call(
          simulate_mab,
          c(common_args, args, list(dt = TRUE))
        ))

        expect_mab_equal(res_df, res_dt)
        expect_joint_equal(res_df, res_dt, seed)
      })
    })
  })
})
