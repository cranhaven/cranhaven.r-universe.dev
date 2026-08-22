input_check_test <- function(test, pass, fails) {
  purrr::walk(pass, \(p) {
    p <- if (is.list(p)) p else list(p)
    expect_no_error(do.call(test, p))
  })
  purrr::walk(fails, \(f) {
    f <- if (is.list(f)) f else list(f)
    expect_snapshot_error(do.call(test, f))
  })
}

make_p <- function(
  arms,
  design = c("none", "blocking", "clustering"),
  group_probs = NULL
) {
  design <- match.arg(design)
  cols <- if (design == "none") "all" else names(group_probs)
  matrix(
    stats::runif(length(arms) * length(cols), 0.3, 0.6),
    nrow = length(arms),
    ncol = length(cols),
    dimnames = list(arms, cols)
  )
}

expect_mab_equal <- function(df, dt) {
  expect_equal(df$models, dt$models, ignore_attr = TRUE)
  if (df$config$args$r == 1) {
    expect_equal(df[["f_stat"]], dt[["f_stat"]])
    items <- c("new_data", "bandit", "means", "contrasts")
  } else {
    items <- c("new_data", "bandit", "means", "contrasts", "f_stat")
  }

  dt$config$call <- NULL
  dt$config$args$dt <- NULL
  df$config$args$dt <- NULL
  df$config$call <- NULL
  df$config$args$data <- NULL
  dt$config$args$data <- NULL
  expect_equal(dt$config, df$config)

  purrr::walk(items, \(item) {
    if (item == "bandit") {
      purrr::walk(c("statistic", "assignment_prob"), \(item2) {
        inner_dt_df_check(df[[item]], dt[[item]], item2)
      })
      if (!is.data.frame(df[[item]][["assignment_quant"]])) {
        expect_equal(
          df[[item]][["assignment_quant"]],
          dt[[item]][["assignment_quant"]]
        )
      } else {
        inner_dt_df_check(df[[item]], df[[item]], "assignment_quant")
      }
    } else {
      inner_dt_df_check(df, dt, item)
    }
  })
}

inner_dt_df_check <- function(df, dt, item) {
  new_df <- if (!is.null(dt[[item]])) tibble::as_tibble(dt[[item]]) else NULL
  expect_equal(new_df, df[[item]], ignore_attr = TRUE)

  new_dt <- if (!is.null(df[[item]])) {
    data.table::as.data.table(df[[item]])
  } else {
    NULL
  }
  expect_equal(new_dt, dt[[item]], ignore_attr = TRUE)
}

expect_joint_equal <- function(df, dt, seed) {
  if (df$config$args$r == 1 && dt$config$args$r == 1) {
    purrr::walk(c("bootstrap", "randomization"), \(method) {
      f <- lapply(list(df, dt), \(mab) {
        set.seed(seed)
        expect_no_error(joint_test(mab, method = method, r = 3))
      })
      expect_equal(f[[1]], f[[2]])
      expect_equal(f[[2]], f[[1]])
    })
  }
}


generate_rct_data <- function(
  arms = c("control", paste0("t", 1:5)),
  cfg,
  n = 600,
  delayed = TRUE
) {
  p <- switch(
    cfg$design,
    "blocking" = make_p(arms, design = "blocking", group_probs = cfg$blocks),
    "clustering" = make_p(
      arms,
      design = "clustering",
      group_probs = cfg$clusters
    ),
    make_p(arms, design = "none")
  )

  sim <- simulate_mab(
    n = n,
    t = 1,
    p = p,
    blocks = cfg$blocks,
    clusters = cfg$clusters,
    delayed_feedback = delayed,
    assignment_dates = if (delayed) {
      seq.Date(as.Date("2024-01-01"), by = "day", length.out = n)
    } else {
      NULL
    },
    time_model = if (delayed) \(n, ...) rep(lubridate::days(0), n) else NULL,
    r = 1,
    dt = FALSE,
    verbose = FALSE
  )
  return(sim$new_data)
}


rct_formula <- function(lhs, rhs, design) {
  if (design %in% c("blocking")) {
    rhs <- paste(rhs, "+ block(block_col)")
  }
  if (design %in% c("clustering")) {
    rhs <- paste(rhs, "+ cluster(cl)")
  }
  stats::as.formula(paste(lhs, "~", rhs))
}
