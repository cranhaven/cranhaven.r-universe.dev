

#' Take in dose A and dose B combinations and estimate GIA
#'
#' @param model_params named vector of parameters to be used in function
#' @param dose_A numeric vector of doses (e.g. mg/mL) of dose_A
#' @param dose_B numeric vector of doses (e.g. mg/mL) of dose_B
#' @param fn the function used to calculate GIA.  The default is base_GIA.  See ?base_GIA for more details.
#' @param fn_list additional parameters to pass to the function to estimate GIA
#' @return vector of the same size of dose_A and dose_B where each entry is the estimated GIA for the combination of dose A and dose B
#' @export
#' @examples
#' model_params <- c("beta_A" = 1, "beta_B" = 2, "gamma_A" = .5,
#' "gamma_B" = .6,  "tau_1" = 1, "tau_2" = 0)
#' dose_A <- c(0, 1, 0)
#' dose_B <- c(0, 0, 1)
#' estimate_GIA(model_params, dose_A, dose_B)
estimate_GIA <- function(model_params,
                         dose_A,
                         dose_B,
                         fn = base_GIA,
                         fn_list = NULL){

  gia <- fn(dose_A = dose_A,
            dose_B = dose_B,
            model_params = model_params,
            fn_list = NULL
            )

  return(gia)



}


#' Estimate GIA according to the base model
#'
#' @param model_params named vector of parameters to be used in function.  Specifically, the named parameters must be "beta_A", "beta_B", "gamma_A", "gamma_B", "tau_1", and "tau_2".  See details for more info.
#' @param dose_A numeric vector of doses (e.g. mg/mL) of dose_A
#' @param dose_B numeric vector of doses (e.g. mg/mL) of dose_B
#' @param fn_list NULL
#' @return estimated GIA for each combination of dose A and dose B
#' @section Details:
#' The equation is given in full as follows.  The GIA (\%) is given a as a function of the model parameters and the doses \eqn{A_i} and \eqn{B_i}, respectively.  The doses scaled by the respective ED50s \eqn{\beta_A} and \eqn{\beta_B} are denoted by \eqn{A_i^*} and \eqn{B_i^*}, respectively.  The parameters \eqn{\gamma_A} and \eqn{\gamma_B} are shape parameters.  The parameters \eqn{\tau_1} and \eqn{\tau_2} are interaction parameters.  Finally, \eqn{\lambda_i} is a weighted combination of dose A and dose B.
#' \deqn{GIA_i = 100\%(1 - e^{-\psi_i})}
#' \deqn{\psi_i = \log(2)u_i^{v_i}}
#' \deqn{u_i = A^*_i + B_i^* + \tau_1  A^*_i B^*_i}
#' \deqn{v_i = \lambda_i \gamma_A + (1-\lambda_i) \gamma_B + \tau_1 \tau_2\lambda_i (1 - \lambda_i) \gamma_A \gamma_B}
#' \deqn{\lambda_i = \frac{A_i^*}{A_i^* + B_i^*}}
#' \deqn{A_i^* = A_i / \beta_A}
#' \deqn{B_i^* = B_i / \beta_B}
#' @export
#' @examples
#' model_params <- c("beta_A" = 1, "beta_B" = 2, "gamma_A" = .5,
#' "gamma_B" = .6,  "tau_1" = 1, "tau_2" = 0)
#' dose_A <- c(0, 1, 0)
#' dose_B <- c(0, 0, 1)
#' base_GIA(model_params, dose_A, dose_B)
base_GIA <- function(model_params,
                     dose_A, dose_B,
                     fn_list = NULL){
  if(!is.null(fn_list)){
    print("base_GIA() does not use additional function arguments in fn_list")
  }


  ## Extract parameters
  beta_A <- model_params["beta_A"]
  beta_B <- model_params["beta_B"]
  gamma_A <- model_params["gamma_A"]
  gamma_B <- model_params["gamma_B"]
  tau_1 <- model_params["tau_1"]
  tau_2 <- model_params["tau_2"]


  ## Build some dummy vars
  A_star <- dose_A / beta_A
  B_star <- dose_B / beta_B
  lambda <- (A_star) / (A_star + B_star)
  lambda <- ifelse(A_star == 0 & B_star == 0, 0, lambda)

  ## Do actual calculations
  u <- (A_star + B_star + tau_1 * A_star * B_star)
  v <- gamma_A * lambda +
    gamma_B * (1 - lambda) +
    tau_1 * tau_2 * gamma_A * gamma_B * (lambda) * (1-lambda)
  psi <- log(2) * u^v
  gia_est <- 100 * (1 - exp(-psi))
  gia_est <- ifelse(is.nan(gia_est), 0, gia_est)
  if(any(is.na(gia_est) | is.nan(gia_est))){
    warning("GIA estimate has NA/NaN values")
  }




  gia_est <- ifelse(dose_A == 0 & dose_B == 0,
                0, gia_est)
  return(gia_est)
}



#' Make a grid of points
#'
#' @param n number of levels on each side (Total grid is n^2).  Default is 40
#' @param par named vector of model parameters
#' @param Amax max amount of number of ED50s.  Default is 2
#' @param Bmax max amount of number of ED50s.  Default is 2.
#' @param n_reps number of replicates to repeat entire grid/experiment.  Default is 1.
#' @return data frame with the following columns
#' \describe{
#' \item{dose_A}{unscaled dose of A}
#' \item{dose_B}{unscaled dose o B}
#' \item{rep}{replicate number}
#' }
#' @examples
#' n <- 40
#'  par <- c("beta_A" = 1, "beta_B" = 2)
#' out <- make_grid(n = 2, par = par)
#' exp_out <- data.frame(dose_A = c(0, 2, 0, 2),
#'                       dose_B = c(0, 0, 4, 4),
#'                       rep = 1)
#' @export
make_grid <- function (n = 40, par,
                       Amax = 2,
                       Bmax = 2,
                       n_reps = 1){
  beta_A <- par["beta_A"]
  beta_B <- par["beta_B"]
  seq_A <- seq(0, beta_A * Amax, length.out = n)
  seq_B <- seq(0, beta_B * Bmax, length.out = n)
  df <- expand.grid(dose_A = seq_A,
                         dose_B = seq_B)
  A_seq <- rep(df$dose_A, each = n_reps)
  B_seq <- rep(df$dose_B, each = n_reps)
  grid_df <- data.frame(dose_A = A_seq,
                        dose_B = B_seq)
  grid_df$rep <- rep(1:n_reps, each = length(A_seq))

  return(grid_df)

}



#' Calculate S generally
#'
#' @param best_pars named vector of parameters.  "tau_1" must be a name.  As must "tau_2" and "gamma_A" and "gamma_B"
#' @param S_fn function to calculate
#' @param fn_list NULL
#' @return Hewlett's S for the given model
#' @export
#' @examples
#' best_pars <- c("tau_1" = 0,
#'               "tau_2" = 1,
#'               "gamma_A" = 1,
#'                "gamma_B" = 1)
#' calc_S_base(best_pars) # should be 1
calc_S <- function(best_pars, S_fn = calc_S_base,
                     fn_list = NULL){
  S_fn(best_pars, fn_list)
}



#' Calculate S from given tau_1 for base model
#'
#' @param best_pars named vector of parameters.  "tau_1" must be a name.  As must "tau_2" and "gamma_A" and "gamma_B"
#' @param fn_list NULL
#' @return Hewlett's S for the base model.
#' @export
#' @examples
#' best_pars <- c("tau_1" = 0,
#'               "tau_2" = 1,
#'               "gamma_A" = 1,
#'                "gamma_B" = 1)
#' calc_S_base(best_pars) # should be 1
calc_S_base  <- function(best_pars,
                         fn_list = NULL){
  if(!is.null(fn_list)) print("calc_S_base() does not use additional arguments")
  tau_1 <- best_pars["tau_1"]
  tau_2 <- best_pars["tau_2"]
  gamma_A <- best_pars["gamma_A"]
  gamma_B <- best_pars["gamma_B"]
  quant <- -2 * (gamma_A + gamma_B) / (gamma_A * gamma_B * tau_1)

  if(tau_1 == 0){
    b <- .5
  } else if(tau_1 < -1){
    warning("No S when tau_1 < -1.  Returning NA")
    return(NA)
  } else if(!is.infinite(quant) & (tau_2 == quant)){ # if v is 0  S is 1
 #   warning("Any dose combination will result in a  GIA of 50%")
    b <- .5
  } else{
    b1 <- -(1 / tau_1) * ( 1 - sqrt(1 + tau_1))
    b2 <- -(1 / tau_1) * ( 1 + sqrt(1 + tau_1))
    b <- min(c(b1, b2)[c(b1, b2) > 0])
  }
  return(as.numeric(1/ (2 * b)))
}


#' Calculate the Sum of Squared Error
#'
#' @param par named vector of parameters
#' @param data
#' \itemize{
#' \item{dose_A}{dose A mg/mL}
#' \item{dose_B}{dose B mg/mL}
#' \item{GIA}{GIA}
#' }
#' @param GIA_fn function to calculate GIA
#' @param fn_list additional arguments to pass GIA_fn
#' @return sum of square error between observed and estimated
SSE_GIA <- function(par, data,
                    GIA_fn = base_GIA,
                    fn_list = NULL){
  gia_est <- GIA_fn(model_params = par,
                    dose_A = data$dose_A,
                    dose_B = data$dose_B,
                    fn_list = fn_list)
  gia_est <- ifelse(is.nan(gia_est), 0, gia_est)
  sse <- sum((data$GIA - gia_est)^2)
  if(is.na(sse)){
    stop("NA GIA estimates")
  }

  return(sse)
}






