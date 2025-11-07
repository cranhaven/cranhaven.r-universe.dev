#' Computes Marginal Measures Using Two Observations
#'
#' Computes bivariate figures such as conditional CIFs, survival probabilities,
#' and  hazards or bivariate CIFs, densities, and survival probabilities.
#'
#' @inheritParams mmcif_logLik
#' @inheritParams mmcif_data
#' @param par numeric vector with the model parameters.
#' @param newdata a \code{data.frame} with data for the observations. It needs
#' to have two rows.
#' @param which_cond an integer with value one or two for the index of the
#' individual that is being conditioned on.
#' @param type_obs a character the type of conditional measure. It can be
#' \code{"derivative"} for the derivative of a CIF,
#' \code{"cumulative"} for a CIF or the survival probability, and
#' \code{"hazard"} for the cause-specific hazard.
#' @param type_cond a character for the type of outcome that is being
#' conditioned on.
#' \code{"derivative"} for the derivative of a CIF or
#' \code{"cumulative"} for a CIF or the survival probability.
#' @param type a 2D character vector for the type of measures for each
#' observation. The elements can be
#' \code{"derivative"} for the derivative of a CIF or
#' \code{"cumulative"} for a CIF or the survival probability.
#' @param use_log a logical for whether the returned output should be on the
#' log scale.
#'
#' @seealso
#' \code{\link{mmcif_pd_univariate}} and \code{\link{mmcif_fit}}.
#'
#' @return
#' A numeric scalar with the requested quantity.
#'
#' @examples
#' if(require(mets)){
#'   data(prt)
#'
#'   # truncate the time
#'   max_time <- 90
#'   prt <- within(prt, {
#'     status[time >= max_time] <- 0
#'     time <- pmin(time, max_time)
#'   })
#'
#'   # select the DZ twins and re-code the status
#'   prt_use <- subset(prt, zyg == "DZ") |>
#'     transform(status = ifelse(status == 0, 3L, status))
#'
#'   # Gauss Hermite quadrature nodes and weights from fastGHQuad::gaussHermiteData
#'   ghq_data <- list(
#'     node = c(-3.43615911883774, -2.53273167423279, -1.75668364929988, -1.03661082978951,
#'              -0.342901327223705, 0.342901327223705, 1.03661082978951, 1.75668364929988,
#'              2.53273167423279, 3.43615911883774),
#'     weight = c(7.6404328552326e-06, 0.00134364574678124, 0.0338743944554811, 0.240138611082314,
#'                0.610862633735326,0.610862633735326, 0.240138611082315, 0.033874394455481,
#'                0.00134364574678124, 7.64043285523265e-06))
#'
#'   # setup the object for the computation
#'   mmcif_obj <- mmcif_data(
#'     ~ country - 1, prt_use, status, time, id, max_time,
#'     2L, strata = country, ghq_data = ghq_data)
#'
#'   # previous estimates
#'   par <- c(0.727279974859164, 0.640534073288067, 0.429437766165371, 0.434367104339573,
#'            -2.4737847536253, -1.49576564624673, -1.89966050143904, -1.58881346649412,
#'            -5.5431198001029, -3.5328359024178, -5.82305147022587, -3.4531896212114,
#'            -5.29132887832377, -3.36106297109548, -6.03690322125729, -3.49516746825624,
#'            2.55000711185704, 2.71995985605891, 2.61971498736444, 3.05976391058032,
#'            -5.97173564860957, -3.37912051983482, -5.14324860374941, -3.36396780694965,
#'            -6.02337246348561, -3.03754644968859, -5.51267338700737, -3.01148582224673,
#'            2.69665543753264, 2.59359057553995, 2.7938341786374, 2.70689750644755,
#'            -0.362056555418564, 0.24088005091276, 0.124070380635372, -0.246152029808377,
#'            -0.0445628476462479, -0.911485513197845, -0.27911988106887, -0.359648419277058,
#'            -0.242711959678559, -6.84897302527358)
#'
#'   # the test data we will use
#'   test_dat <- data.frame(
#'     country = factor(c("Norway", "Norway"), levels(prt_use$country)),
#'     status = c(1L, 2L), time = c(60, 75))
#'
#'   # probability that both experience the event prior to the two times
#'   mmcif_pd_bivariate(
#'     par = par, object = mmcif_obj, newdata = test_dat, cause = status,
#'     strata = country, ghq_data = ghq_data, time = time, type =
#'       c("cumulative", "cumulative")) |>
#'     print()
#'
#'   # density that one experiences an event at the point and the other
#'   # experiences an event prior to the point
#'   mmcif_pd_bivariate(
#'     par = par, object = mmcif_obj, newdata = test_dat, cause = status,
#'     strata = country, ghq_data = ghq_data, time = time, type =
#'       c("derivative", "cumulative")) |>
#'     print()
#'
#'   # probability that both survive up to the passed points
#'   mmcif_pd_bivariate(
#'     par = par, object = mmcif_obj, newdata = test_dat, cause = c(3L, 3L),
#'     strata = country, ghq_data = ghq_data, time = time, type =
#'       c("cumulative", "cumulative")) |>
#'     print()
#'
#'   # conditional hazard given that the other experiences an event prior to time
#'   mmcif_pd_cond(
#'     par = par, object = mmcif_obj, newdata = test_dat, cause = status,
#'     strata = country, ghq_data = ghq_data, time = time, which_cond = 1L,
#'     type_cond = "cumulative", type_obs = "hazard") |>
#'     print()
#'
#'   # conditional CIF given that the other experiences an event prior to the
#'   # time
#'   mmcif_pd_cond(
#'     par = par, object = mmcif_obj, newdata = test_dat, cause = c(2L, 2L),
#'     strata = country, ghq_data = ghq_data, time = time, which_cond = 1L,
#'     type_cond = "cumulative", type_obs = "cumulative") |>
#'     print()
#'
#'   # same but given that the other experiences the event at the point
#'   mmcif_pd_cond(
#'     par = par, object = mmcif_obj, newdata = test_dat, cause = c(2L, 2L),
#'     strata = country, ghq_data = ghq_data, time = time, which_cond = 1L,
#'     type_cond = "derivative", type_obs = "cumulative") |>
#'     print()
#' }
#'
#' @export
mmcif_pd_cond <- function(
    par, object, newdata, cause, time, left_trunc = NULL,
    ghq_data = object$ghq_data, strata = NULL,
    which_cond, type_cond = "derivative", type_obs = "cumulative"){
  type_cond <- match.arg(type_cond, c("derivative", "cumulative"))
  type_obs <- match.arg(type_obs, c("derivative", "cumulative", "hazard"))
  stopifnot(is.numeric(which_cond), length(which_cond) == 1L,
            which_cond %in% 1:2)

  cause <- eval(substitute(cause), newdata, parent.frame())
  time <- eval(substitute(time), newdata, parent.frame())
  strata <- eval(substitute(strata), newdata, parent.frame())
  left_trunc <- eval(substitute(left_trunc), newdata, parent.frame())

  if(which_cond == 1L){
    # put the one being conditioned on last
    newdata <- newdata[2:1, ]
    cause <- cause[2:1]
    time <- time[2:1]
    strata <- strata[2:1]
    left_trunc <- left_trunc[2:1]
  }
  idx_cond <- 2L

  if(type_obs %in% c("derivative", "cumulative")){
    log_denominator <- eval(bquote(mmcif_pd_univariate(
      par = par, object = object, newdata = .(newdata[idx_cond, ]),
      cause = .(cause[idx_cond]), time = .(time[idx_cond]),
      left_trunc = .(left_trunc[idx_cond]), ghq_data = ghq_data,
      strata = .(strata[which_cond]), use_log = TRUE, type = .(type_cond))))

    log_numerator <- eval(bquote(mmcif_pd_bivariate(
      par = par, object = object, newdata = .(newdata), cause = .(cause),
      time = .(time), left_trunc = .(left_trunc), ghq_data = ghq_data,
      strata = .(strata), use_log = TRUE, type = .(c(type_obs, type_cond)))))

    return(exp(log_numerator - log_denominator))

  } else if(type_obs == "hazard"){
    log_dens <- eval(bquote(mmcif_pd_bivariate(
      par = par, object = object, newdata = .(newdata), cause = .(cause),
      time = .(time), left_trunc = .(left_trunc), ghq_data = ghq_data,
      strata = .(strata), use_log = TRUE,
      type = c("derivative", .(type_cond)))))

    n_causes <- object$indices$n_causes
    log_surv <- eval(bquote(mmcif_pd_bivariate(
      par = par, object = object, newdata = .(newdata),
      cause = .(c(n_causes + 1L, cause[2])), time = .(time),
      left_trunc = .(left_trunc), ghq_data = ghq_data, strata = .(strata),
      use_log = TRUE, type = c("cumulative", .(type_cond)))))

    return(exp(log_dens - log_surv))
  }

  stop("type is not implemented")
}

#' @rdname mmcif_pd_cond
#' @export
mmcif_pd_bivariate <- function(
    par, object, newdata, cause, time, left_trunc = NULL,
    ghq_data = object$ghq_data, strata = NULL, use_log = FALSE,
    type = c("cumulative", "cumulative")){
  stopifnot(is.numeric(par), inherits(object, "mmcif"), all(is.finite(par)),
            length(par) == object$indices$n_par_upper_tri,
            is.data.frame(newdata), nrow(newdata) == 2,
            is.logical(use_log), length(use_log) == 1,
            use_log %in% c(TRUE, FALSE))

  n_causes <- object$indices$n_causes
  n_strata <- object$indices$n_strata
  time_expansion <- object$time_expansion
  d_time_expansion <- object$d_time_expansion
  max_time <- object$max_time

  covs_risk <- model.matrix(object$terms, newdata)

  cause <- eval(substitute(cause), newdata, parent.frame())
  stopifnot(is.numeric(cause), length(cause) == 2, all(cause > 0L),
            all(cause <= n_causes + 1L))

  time <- eval(substitute(time), newdata, parent.frame())
  stopifnot(is.numeric(time), length(time) == 2, all(time > 0))

  strata <- eval(substitute(strata), newdata, parent.frame())
  if(!is.null(strata)){
    if(is.factor(strata)){
      stopifnot(length(levels(strata)) == n_strata)
      strata <- as.integer(strata)
    }
    stopifnot(length(strata) == 2, is.numeric(strata),
              all(strata > 0), all(strata <= n_strata))
  }

  left_trunc <- eval(substitute(left_trunc), newdata, parent.frame())
  if(is.null(left_trunc))
    left_trunc <- numeric(length(time))
  stopifnot(is.numeric(left_trunc), length(left_trunc) == 2L,
            all(left_trunc >= 0), all(is.finite(left_trunc)))

  type <- match.arg(type, c("derivative", "cumulative"), several.ok = TRUE)
  stopifnot(is.character(type), length(type) == 2L,
            all(!(type == "derivative" & cause == n_causes + 1L)))

  covs_trajectory <- eval_trajectory_covs(
    time, time_expansion, covs_risk, strata, n_causes)

  d_covs_trajectory <- eval_trajectory_covs(
    time, d_time_expansion,
    matrix(0, NROW(covs_risk), NCOL(covs_risk)), strata, n_causes)

  has_finite_trajectory_prob <- time < max_time

  covs_trajectory_delayed <-
    matrix(NaN, NROW(covs_trajectory), NCOL(covs_trajectory))
  has_delayed_entry <- which(left_trunc > 0)

  covs_trajectory_delayed[has_delayed_entry, ] <-
    eval_trajectory_covs(
      left_trunc[has_delayed_entry], time_expansion,
      covs_risk[has_delayed_entry, ],
      if(!is.null(strata)) strata[has_delayed_entry] else strata, n_causes)

  n_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
  par_pass <- c(head(par, -n_vcov), log_chol_inv(tail(par, n_vcov)))

  deriv <- type == "derivative"
  out <- mmcif_pd_bivariate_cpp(
    data_ptr = object$comp_obj, par = par_pass, ghq_data = ghq_data,
    cov_trajectory = t(covs_trajectory),
    d_cov_trajectory = t(d_covs_trajectory), cov_risk = t(covs_risk),
    has_finite_trajectory_prob = has_finite_trajectory_prob, cause = cause - 1L,
    cov_trajectory_delayed = t(covs_trajectory_delayed), derivs = deriv)

  if(use_log) out else exp(out)
}

#' Computes Marginal Measures for One Observation
#'
#' Computes the marginal cumulative incidence functions (CIF), marginal survival
#' function or the derivative of the CIF.
#'
#' @inheritParams mmcif_pd_cond
#' @param newdata a \code{data.frame} with data for the observation. It needs
#' to have one row.
#' @param type a character for the type of measures for the observation. It
#' can have value
#' \code{"derivative"} for the derivative of a CIF or
#' \code{"cumulative"} for a CIF or the survival probability.
#'
#' @seealso
#' \code{\link{mmcif_pd_bivariate}} and \code{\link{mmcif_pd_cond}}.
#'
#' @return
#' A numeric scalar with the requested quantity.
#'
#' @examples
#' if(require(mets)){
#'   data(prt)
#'
#'   # truncate the time
#'   max_time <- 90
#'   prt <- within(prt, {
#'     status[time >= max_time] <- 0
#'     time <- pmin(time, max_time)
#'   })
#'
#'   # select the DZ twins and re-code the status
#'   prt_use <- subset(prt, zyg == "DZ") |>
#'     transform(status = ifelse(status == 0, 3L, status))
#'
#'   # Gauss Hermite quadrature nodes and weights from fastGHQuad::gaussHermiteData
#'   ghq_data <- list(
#'     node = c(-3.43615911883774, -2.53273167423279, -1.75668364929988, -1.03661082978951,
#'              -0.342901327223705, 0.342901327223705, 1.03661082978951, 1.75668364929988,
#'              2.53273167423279, 3.43615911883774),
#'     weight = c(7.6404328552326e-06, 0.00134364574678124, 0.0338743944554811, 0.240138611082314,
#'                0.610862633735326,0.610862633735326, 0.240138611082315, 0.033874394455481,
#'                0.00134364574678124, 7.64043285523265e-06))
#'
#'   # setup the object for the computation
#'   mmcif_obj <- mmcif_data(
#'     ~ country - 1, prt_use, status, time, id, max_time,
#'     2L, strata = country, ghq_data = ghq_data)
#'
#'   # previous estimates
#'   par <- c(0.727279974859164, 0.640534073288067, 0.429437766165371, 0.434367104339573,
#'            -2.4737847536253, -1.49576564624673, -1.89966050143904, -1.58881346649412,
#'            -5.5431198001029, -3.5328359024178, -5.82305147022587, -3.4531896212114,
#'            -5.29132887832377, -3.36106297109548, -6.03690322125729, -3.49516746825624,
#'            2.55000711185704, 2.71995985605891, 2.61971498736444, 3.05976391058032,
#'            -5.97173564860957, -3.37912051983482, -5.14324860374941, -3.36396780694965,
#'            -6.02337246348561, -3.03754644968859, -5.51267338700737, -3.01148582224673,
#'            2.69665543753264, 2.59359057553995, 2.7938341786374, 2.70689750644755,
#'            -0.362056555418564, 0.24088005091276, 0.124070380635372, -0.246152029808377,
#'            -0.0445628476462479, -0.911485513197845, -0.27911988106887, -0.359648419277058,
#'            -0.242711959678559, -6.84897302527358)
#'
#'   # the test data we will use
#'   test_dat <- data.frame(country = factor("Norway", levels(prt_use$country)),
#'                          status = 2L)
#'
#'   # compute the CIF
#'   mmcif_pd_univariate(
#'     par = par, object = mmcif_obj, newdata = test_dat, cause = status,
#'     strata = country, ghq_data = ghq_data, time = 75, type = "cumulative") |>
#'     print()
#'
#'   # compute the derivative of the CIF
#'   mmcif_pd_univariate(
#'     par = par, object = mmcif_obj, newdata = test_dat, cause = status,
#'     strata = country, ghq_data = ghq_data, time = 75, type = "derivative") |>
#'     print()
#'
#'   # compute the survival probability
#'   mmcif_pd_univariate(
#'     par = par, object = mmcif_obj, newdata = test_dat, cause = 3L,
#'     strata = country, ghq_data = ghq_data, time = 75, type = "cumulative") |>
#'     print()
#' }
#'
#' @export
mmcif_pd_univariate <- function(
    par, object, newdata, cause, time, left_trunc = NULL,
    ghq_data = object$ghq_data, strata = NULL, use_log = FALSE,
    type = "cumulative"){
  stopifnot(is.numeric(par), inherits(object, "mmcif"), all(is.finite(par)),
            length(par) == object$indices$n_par_upper_tri,
            is.data.frame(newdata), nrow(newdata) == 1,
            is.logical(use_log), length(use_log) == 1,
            use_log %in% c(TRUE, FALSE))

  n_causes <- object$indices$n_causes
  n_strata <- object$indices$n_strata
  time_expansion <- object$time_expansion
  d_time_expansion <- object$d_time_expansion
  max_time <- object$max_time

  covs_risk <- model.matrix(object$terms, newdata)

  cause <- eval(substitute(cause), newdata, parent.frame())
  stopifnot(is.numeric(cause), length(cause) == 1, cause > 0L,
            cause <= n_causes + 1L)

  time <- eval(substitute(time), newdata, parent.frame())
  stopifnot(is.numeric(time), length(time) == 1, time > 0)

  strata <- eval(substitute(strata), newdata, parent.frame())
  if(!is.null(strata)){
    if(is.factor(strata)){
      stopifnot(length(levels(strata)) == n_strata)
      strata <- as.integer(strata)
    }
    stopifnot(length(strata) == 1, is.numeric(strata),
              strata > 0, strata <= n_strata)
  }

  left_trunc <- eval(substitute(left_trunc), newdata, parent.frame())
  if(is.null(left_trunc))
    left_trunc <- numeric(length(time))
  stopifnot(is.numeric(left_trunc), length(left_trunc) == 1L,
            left_trunc >= 0, is.finite(left_trunc))

  type <- match.arg(type, c("derivative", "cumulative"), several.ok = TRUE)
  stopifnot(is.character(type), length(type) == 1L,
            !(type == "derivative" && cause == n_causes + 1L))

  covs_trajectory <- eval_trajectory_covs(
    time, time_expansion, covs_risk, strata, n_causes)

  d_covs_trajectory <- eval_trajectory_covs(
    time, d_time_expansion,
    matrix(0, NROW(covs_risk), NCOL(covs_risk)), strata, n_causes)

  has_finite_trajectory_prob <- time < max_time

  covs_trajectory_delayed <-
    matrix(NaN, NROW(covs_trajectory), NCOL(covs_trajectory))
  has_delayed_entry <- which(left_trunc > 0)

  covs_trajectory_delayed[has_delayed_entry, ] <-
    eval_trajectory_covs(
      left_trunc[has_delayed_entry], time_expansion,
      covs_risk[has_delayed_entry, ],
      if(!is.null(strata)) strata[has_delayed_entry] else strata, n_causes)

  n_vcov <- (2L * n_causes * (2L * n_causes + 1L)) %/% 2L
  par_pass <- c(head(par, -n_vcov), log_chol_inv(tail(par, n_vcov)))

  deriv <- type == "derivative"
  out <- mmcif_pd_univariate_cpp(
    data_ptr = object$comp_obj, par = par_pass, ghq_data = ghq_data,
    cov_trajectory = covs_trajectory, d_cov_trajectory = d_covs_trajectory,
    cov_risk = covs_risk,
    has_finite_trajectory_prob = has_finite_trajectory_prob,
    cause = cause - 1L, cov_trajectory_delayed = covs_trajectory_delayed,
    deriv = deriv)

  if(use_log) out else exp(out)
}
