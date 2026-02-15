context("Testing 'sim_joint_data_set'")

for(f_name in c("no-B", "no-delta", "no-gamma", "one-marker", "w-all")){
  test_that(paste0("'", f_name, "' settings gives previous results"), {
    r_f_name_org <- file.path("test-data", paste0(f_name, ".R"))

    r_f_name <- file.path("..", "..", "SimSurvNMarker", r_f_name_org)
    if(!file.exists(r_f_name))
      r_f_name <- file.path("..", "..", "inst", r_f_name_org)
    if(!file.exists(r_f_name))
      r_f_name <- system.file(r_f_name_org, package = "SimSurvNMarker")

    expect_true(nchar(r_f_name) > 0)
    expect_true(file.exists(r_f_name))

    source(r_f_name, local = TRUE)
    args_env$n_obs <- 20L

    dat <- with(args_env, {
      r_n_marker <- function(id)
        rpois(1, 10) + 1L
      r_obs_time <- function(id, n_markes)
        sort(runif(n_markes, 0, 10))
      r_z <- function(id)
        as.numeric(runif(d_z) > .5)
      r_x <- function(id)
        as.numeric(runif(d_x) > .5)
      r_left_trunc <- function(id)
        rbeta(1, 1, 2) * 3
      r_right_cens <- function(id)
        rbeta(1, 2, 1) * 6 + 4

      b_func <- get_ns_spline(b_ks, do_log = TRUE)
      m_func <- get_ns_spline(m_ks, do_log = FALSE)
      g_func <- get_ns_spline(g_ks, do_log = FALSE)

      # we have to check that we get the same as w/ ns(). Otherwise, the
      # test results are not valid.
      tmps <- seq(1, 10, length.out = 15)

      if(length(b_ks) > 0)
        skip_if(!isTRUE(all.equal(unclass(ns(
          log(tmps), knots = b_ks[-c(1, length(b_ks))],
          Boundary.knots   = b_ks[ c(1, length(b_ks))], intercept = TRUE)),
          b_func(tmps), check.attributes = FALSE)))
      if(length(m_ks) > 0)
        skip_if(!isTRUE(all.equal(unclass(ns(
          tmps, knots      = m_ks[-c(1, length(m_ks))],
          Boundary.knots   = m_ks[ c(1, length(m_ks))], intercept = TRUE)),
          m_func(tmps), check.attributes = FALSE)))
      if(length(g_ks) > 0)
        skip_if(!isTRUE(all.equal(unclass(ns(
          tmps, knots      = g_ks[-c(1, length(g_ks))],
          Boundary.knots   = g_ks[ c(1, length(g_ks))], intercept = TRUE)),
          g_func(tmps), check.attributes = FALSE)))

      gl_dat <- get_gl_rule(30L)

      set.seed(1)
      sim_joint_data_set(
        n_obs = n_obs, B = B, Psi = Psi, omega = omega, delta = delta,
        alpha = alpha, sigma = sig, gamma = gamma, b_func = b_func,
        m_func = m_func, g_func = g_func, gl_dat = gl_dat, r_z = r_z,
        r_left_trunc = r_left_trunc, r_right_cens = r_right_cens,
        r_n_marker = r_n_marker, r_x = r_x, r_obs_time = r_obs_time,
        y_max = 10)
      })

    expect_known_value(
      dat$survival_data,
      file.path(test_res_dir, paste0("survival-", f_name, ".RDS")))
    expect_known_value(
      dat$marker_data,
      file.path(test_res_dir, paste0("marker_data-", f_name, ".RDS")))
  })
}
