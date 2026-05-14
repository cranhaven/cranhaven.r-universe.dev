#' Optimal Control
#'
#' Creates optimal schedule of pulses for mosquito control.
#'
#' @param counts Numeric vector of population counts.
#' @param time Numeric vector with corresponding day of year measurments.
#' Example: Jan 1st = day 1.  Must be same length as \code{counts}.
#' @param mu Numeric indicating natural population death rate.
#' @param m Numeric indicating number of lifetimes for population decay
#' between seasons
#' @param n_lam Numeric max fourier mode order to calculate.
#' @param kmax Numeric max number of dynamics fourier modes to use in calculating
#' fourier sum (different than N_lam = max emergence fourier mode set by user
#' for curve fitting portion of the code. Kmax should be an integer between
#' 2 and 200, default at 20.
#' @param global_opt Numeric set to 0 if user chooses local optimum, 1 if user
#' chooses golbal GN_DIRECT_L_RAND method, 2 if user chooses global
#' GN_ISRES method.
#' @param n_pulse Numeric number of pulses, set by user, integer between
#' 1 and 10.
#' @param rho Numeric percent knockdown (user set between .01 and .30,
#' e.g. 1\% to 30\% knockdown).
#' @param days_between Numeric minimum number of days allowed between pulses
#' set by user (integer bewtween 0 and 30 days).
#' @param max_eval Numeric maximum evaluations for optimization step.
#'
#' @return Control list of control parameters.
#'
#'@examples
#'y_in <- c(15, 40, 45, 88, 99, 145, 111, 132, 177, 97, 94, 145, 123, 111,
#'125, 115, 155, 160, 143, 132, 126, 125, 105, 98, 87, 54, 55, 8
#')
#'t_in_user <- c(93, 100, 107, 114, 121, 128, 135, 142, 149, 163, 170, 177,
#'184, 191, 198, 205, 212, 219, 226, 233, 240, 247, 254, 261,
#'267, 274, 281, 288
#')
#'control(y_in, t_in_user, global_opt = -1)
#'
#'
#' @export
control <- function(counts,
                    time,
                    mu = 1 / 14,
                    m = 3,
                    n_lam = 25,
                    kmax = 20,
                    global_opt = 0,
                    n_pulse = 4,
                    rho = .30,
                    days_between = 3,
                    max_eval = 10000) {
  assertthat::assert_that(
    length(counts) == length(time),
    msg = paste("input data mismatch")
  )

  t_in <- round(time)
  n0 <- length(counts)

  # delta_t_in <- time_difference(n0, t_in)

  n_pts <- n0 + 3
  t_dat <- numeric(n_pts)

  if (t_in[1] - m / mu > 0) {
    y_dat <- c(0, 0, counts, 0)

    t_dat[1] <- 0
    t_dat[2] <- (m - 1) / mu
    for (i in 3:(n_pts - 1)) {
      t_dat[i] <- t_in[i - 2] - t_in[1] + m / mu
    }
    t_dat[n_pts] <- t_dat[n_pts - 1] + (m) / mu

    t_dat <- matrix(t_dat, ncol = 1)
  } else {
    y_dat <- c(0, counts, 0, 0)

    t_dat[1] <- 0
    for (i in 2:(n_pts - 2)) {
      t_dat[i] <- t_in[i - 1]
    }
    t_dat[n_pts - 1] <- t_dat[n_pts - 2] + (2 * (m - 1)) / mu - t_dat[2]
    t_dat[n_pts] <- t_dat[n_pts - 1] + 1 / mu
  }

  delta_t_dat <- extend_time_diff(n_pts, t_dat)

  # tau = length of 'season' in days
  tau <- t_dat[n_pts]

  J <- j_matrix(mu, n_pts, delta_t_dat)
  M <- m_matrix(mu, n_pts, delta_t_dat)
  W <- weight_matrix(m, mu, n_pts, t_in, n_pts)

  lambda_dat <- pracma::fmincon(
    numeric(n_pts),
    create_objective_fun(J, W, M, mu, y_dat),
    gr = NULL,
    method = "SQP",
    A = (-(1 / mu) * (pracma::inv(J) %*% (M))),
    b = numeric(n_pts),
    Aeq = aeq(n_pts),
    beq = 0,
    lb = numeric(n_pts),
    ub = NULL
  )

  lam_fourier <- cal_lam_fourier(
    n_lam,
    n_pts,
    tau,
    lambda_dat,
    t_dat,
    delta_t_dat
  )

  if (global_opt < 0) {
    return(
      structure(
        list(
          pulse_times_output = NULL,
          ave_pop_un_cont = NULL,
          ave_pop_cont = NULL,
          percent_reduction = NULL,
          accuracy_measure = NULL,
          tau = tau,
          t_dat_plot = NULL,
          y_dat = y_dat
        ),
        class = "mosqcontrol"
      )
    )
  }

  if (n_pulse == 1) {
    fun1 <- function(x) sum_one_pulse(x, rho, mu, tau, kmax, lam_fourier)
    guess <- tau / 2

    if (global_opt == 0) {
      opt_out <- nloptr::cobyla(guess, fun1, lower = 0, upper = tau)

      times <- opt_out$par
      ave_pop_fourier <- opt_out$value
    } else if (global_opt == 1) { # global optimum directL

      opt_out <- nloptr::directL(fun1, lower = 0, upper = tau, randomized = TRUE)

      times <- opt_out$par
      ave_pop_fourier <- opt_out$value
    } else if (global_opt == 2) { # global optimum isres

      opt_out <- nloptr::nloptr(
        x0 = guess,
        eval_f = fun1,
        lb = 0,
        ub = tau,
        eval_g_ineq = NULL,
        eval_g_eq = NULL,
        opts = list(algorithm = "NLOPT_GN_ISRES", maxeval = max_eval)
      )

      times <- opt_out$solution
      ave_pop_fourier <- opt_out$objective
    }
  } else {
    funN <- function(x) sum_n_pulse(x, rho, n_pulse, mu, tau, kmax, lam_fourier)

    # matrix and vector for inequality constraints to enforce minimum days between
    A_mat <- matrix(0, nrow = n_pulse - 1, ncol = n_pulse)
    b_vec <- (-1) * days_between * matrix(1, n_pulse - 1, 1)

    for (i in 1:(n_pulse - 1)) {
      A_mat[i, i] <- 1
      A_mat[i, i + 1] <- -1
    }

    l_bound <- numeric(n_pulse) # column vector of zeros for pulse lower bound
    u_bound <- tau * matrix(1, n_pulse, 1) # column vector of taus for pulse upper bound

    guess <- numeric(n_pulse) # vector for initial guesses for fmincon

    for (i in 1:n_pulse) {
      if (t_in[1] - m / mu > 0) {
        guess[i] <- i * (t_dat[n_pts - 1] - t_dat[3]) / (n_pulse + 1) + t_dat[3]
      } else {
        guess[i] <- i * (t_dat[n_pts - 2] - t_dat[2]) / (n_pulse + 1) + t_dat[2]
      }
    }

    if (global_opt == 0) { # local optimum
      opt_out <- pracma::fmincon(
        guess,
        funN,
        gr = NULL,
        method = "SQP",
        A = A_mat,
        b = b_vec,
        Aeq = NULL,
        beq = NULL,
        lb = l_bound,
        ub = u_bound
      )

      times <- opt_out$par
      ave_pop_fourier <- opt_out$value
    } else if (global_opt == 1) { # global optimum directL

      opt_out <- nloptr::directL(funN, lower = l_bound, upper = u_bound, randomized = TRUE)

      times <- opt_out$par
      ave_pop_fourier <- opt_out$value
    } else if (global_opt == 2) { # global optimum isres

      ineq <- function(x) {
        (-1) * A_mat %*% x - b_vec
      }

      opt_out <- nloptr::nloptr(
        x0 = guess,
        eval_f = funN,
        lb = l_bound,
        ub = u_bound,
        eval_g_ineq = ineq,
        eval_g_eq = NULL,
        opts = list(algorithm = "NLOPT_GN_ISRES", maxeval = max_eval)
      )

      times <- opt_out$solution
      ave_pop_fourier <- opt_out$objective
    }
  }

  # shift pulse times back to original times input by user
  if (t_in[1] - (m / mu) > 0) {
    pulse_times_output <- times - t_dat[3] + t_in[1]
  } else {
    pulse_times_output <- times - t_dat[2] + t_in[1]
  }

  t_steps <- 6 * tau * 10 + 1 # ten time steps per day, six total seasons (need to integrate over many seasons to reach periodic population curves)
  t_vec <- pracma::linspace(0, 6 * tau, n = t_steps)


  y_fourier_controlled <- numeric(t_steps) # list (row vector) for controlled population values at each time step
  y_fourier_uncontrolled <- numeric(t_steps) # list (row vector) for uncontrolled population values at each time step


  # simple integrator to calculate population values
  for (i in 2:t_steps) {
    for (j in 1:n_pts - 1) {
      if (pracma::mod(t_vec[i - 1], tau) >= t_dat[j] &&
          pracma::mod(t_vec[i - 1], tau) < t_dat[j + 1]) {
        y_fourier_controlled[i] <- y_fourier_controlled[i - 1] +
          (-mu * y_fourier_controlled[i - 1] + lambda_dat$par[j + 1] *
             (pracma::mod(t_vec[i - 1], tau) - t_dat[j]) / delta_t_dat[j] +
             lambda_dat$par[j] * (
               t_dat[j + 1] - pracma::mod(t_vec[i - 1], tau)
             ) /
             delta_t_dat[j]) * (t_vec[i] - t_vec[i - 1])

        y_fourier_uncontrolled[i] <- y_fourier_uncontrolled[i - 1] +
          (-mu * y_fourier_uncontrolled[i - 1] + lambda_dat$par[j + 1] *
             (pracma::mod(t_vec[i - 1], tau) - t_dat[j]) / delta_t_dat[j] +
             lambda_dat$par[j] * (t_dat[j + 1] - pracma::mod(t_vec[i - 1], tau)) /
             delta_t_dat[j]) * (t_vec[i] - t_vec[i - 1])
      }
    }

    # impulses for the controlled population
    for (j in 1:n_pulse) {
      if (pracma::mod(t_vec[i], tau) >= times[j] &&
          pracma::mod(t_vec[i - 1], tau) < times[j]) {
        y_fourier_controlled[i] <- (1 - rho) * y_fourier_controlled[i]
      }
    }
  }

  pop_cont <- numeric(10 * tau + 1) # list for controlled population vlaues, one season long
  pop_un_cont <- numeric(10 * tau + 1) # list for uncontrolled population vlaues, one season long
  t_vec_plot <- numeric(10 * tau + 1) # list for time values, one season long

  for (i in 1:(10 * tau + 1)) {
    # take controlled population values from the integration for the final season
    pop_cont[i] <- y_fourier_controlled[i + 10 * tau * 5]

    # take uncontrolled population values from the integration for the final season
    pop_un_cont[i] <- y_fourier_uncontrolled[i + 10 * tau * 5]

    # shift plot times for population curves back to original times input by user
    if (t_in[1] - m / mu > 0) {
      t_vec_plot[i] <- t_vec[i] - t_dat[(3)] + t_in[(1)]
    } else {
      t_vec_plot[i] <- t_vec[i] - t_dat[(2)] + t_in[(1)]
    }
  }


  # shift data times back to orginal times input by user
  if (t_in[(1)] - (m / mu) > 0) {
    t_dat_plot <- t_dat - t_dat[(3)] + t_in[(1)]
  } else {
    t_dat_plot <- t_dat - t_dat[2] + t_in[1]
  }

  ave_pop_un_cont <- sfsmisc::integrate.xy(t_vec_plot, pop_un_cont) / (tau)
  ave_pop_cont <- sfsmisc::integrate.xy(t_vec_plot, pop_cont) / (tau)

  percent_reduction <- (ave_pop_un_cont - ave_pop_cont) / ave_pop_un_cont
  accuracy_measure <- (ave_pop_fourier - ave_pop_cont) / ave_pop_cont

  structure(
    list(
      pulse_times_output = pulse_times_output,
      ave_pop_un_cont = ave_pop_un_cont,
      ave_pop_cont = ave_pop_cont,
      percent_reduction = percent_reduction,
      accuracy_measure = accuracy_measure,
      tau = tau,
      t_dat_plot = t_dat_plot,
      y_dat = y_dat
    ),
    class = "mosqcontrol"
  )
}

#' @method summary mosqcontrol
#' @export
summary.mosqcontrol <- function(object, ...) {
  structure(
    list(
      tau = object$tau
    ),
    class = "summary.mosqcontrol"
  )
}

#' @method plot mosqcontrol
#' @param ... Numeric, complex, or logical vectors.  Includes t_vec_plot,
#' pop_un_cont, and pop_cont.
#' @export
plot.mosqcontrol <- function(x, ...) {
  args <- list(...)

  graphics::plot(
    x$t_dat_plot,
    x$y_dat,
    ylim = c(0, 1.5 * max(x$y_dat)),
    xlim = c(x$t_dat_plot[1], x$t_dat_plot[1] + x$tau),
    col = "blue",
    main = "Fitted Population Model",
    ylab = "Mosquito population count",
    xlab = "Day of year"
  )

  graphics::lines(args$t_vec_plot, args$pop_un_cont, col = "red")
  graphics::lines(args$t_vec_plot, args$pop_cont, col = "orange")

  graphics::legend(
    "topleft",
    c(
      "Data",
      "Uncontrolled Fourier Approximation",
      "Controlled Fourier Approximation"
    ),
    fill = c("blue", "red", "orange")
  )
}
