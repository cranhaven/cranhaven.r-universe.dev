test_that("mab_from_rct: space-filling argument and design coverage", {
  rct_designs <- list(
    none = list(design = "none", blocks = NULL, clusters = NULL),
    blocking = list(
      design = "blocking",
      blocks = stats::setNames(rep(0.2, 5), paste0("b", 1:5)),
      clusters = NULL
    ),
    clustering = list(
      design = "clustering",
      blocks = NULL,
      clusters = stats::setNames(rep(0.1, 10), paste0("c", 1:10))
    )
  )
  names_map <- c(
    mab_condition = "arm",
    mab_success = "outcome",
    new_success_date = "succ_date",
    block = "block_col",
    cluster = "cl",
    assignment_date = "assignment_date"
  )

  purrr::iwalk(rct_designs, \(cfg, design_name) {
    set.seed(777)
    data <- generate_rct_data(cfg = cfg, delayed = TRUE) |>
      dplyr::select(tidyr::any_of(c(names(names_map), "assignment_date"))) |>
      dplyr::rename_with(
        ~ unname(names_map[.x]),
        .cols = tidyr::any_of(names(names_map))
      )
    common_args <- list(
      formula = rct_formula(
        rhs = "arm",
        lhs = "outcome",
        design = design_name
      ),
      data = data,
      period_method = "batch",
      period_length = 60
    )
    arg_sets <- list(
      default = list(),
      ucb = list(algorithm = "ucb1"),
      random_assign = list(random_assign_prop = 0.2, contrasts = "all"),
      control = list(
        control_augment = 0.2,
        control_condition = "control",
        contrasts = "both"
      ),
      discount = list(discount_rate = 0.9),
      reps = list(r = 2),
      contrasts_all = list(contrasts = "all", prior_periods = 2),
      whole_experiment = list(whole_experiment = TRUE),
      date_method = list(
        period_method = "date",
        time_unit = "day",
        period_length = 30,
        date_col = quote(assignment_date)
      ),
      individual_method = list(
        period_method = "individual",
        contrasts = "best"
      ),
      delayed = list(
        delayed_feedback = TRUE,
        date_col = quote(assignment_date),
        assignment_date_col = quote(assignment_date),
        success_date_col = quote(succ_date),
        period_method = "date",
        time_unit = "day",
        period_length = 50,
        prior_periods = 4
      ),
      mixed1 = list(
        algorithm = "ucb1",
        random_assign_prop = 0.1,
        discount_rate = 0.85,
        control_condition = "control",
        contrasts = "control"
      ),
      mixed2 = list(
        algorithm = "thompson",
        control_augment = 0.15,
        control_condition = "control",
        r = 2,
        prior_periods = 4
      ),
      mixed3 = list(
        period_method = "date",
        time_unit = "week",
        contrasts = "control",
        period_length = 4,
        date_col = quote(assignment_date),
        random_assign_prop = 0.1,
        r = 2
      )
    )

    purrr::iwalk(arg_sets, \(args, arg_name) {
      test_that(paste("mab_from_rct:", design_name, arg_name), {
        seed <- 321
        full_args_df <- utils::modifyList(common_args, args)
        full_args_dt <- full_args_df
        full_args_dt$data <- data.table::copy(data.table::as.data.table(
          full_args_df$data
        ))

        set.seed(seed)
        res_df <- expect_no_error(do.call(mab_from_rct, full_args_df))
        set.seed(seed)
        res_dt <- expect_no_error(do.call(mab_from_rct, full_args_dt))

        expect_mab_equal(res_df, res_dt)
        expect_joint_equal(res_df, res_dt, seed)
      })
    })
  })
})
