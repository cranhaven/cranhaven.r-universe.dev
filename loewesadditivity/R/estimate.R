

#' Estimate the parameters for a given data set and model
#'
#' @param data data frame with the following columns
#' \itemize{
#' \item{dose_A}{dose A mg/mL}
#' \item{dose_B}{dose B mg/mL}
#' \item{GIA}{GIA}
#' }
#' @param init_params named vector of parameters, that correspond to those used in 'GIA_fn'.  These will be used as the initial guesses.  A default is provided.
#' @param n_boot number of boot straps to use to estimate confidence intervals of the parameters, GIA estimates, and values of S.  The default is 100.  If n_boot = 0, then no bootstraps will be run and only the point estimates will be returned.
#' @param GIA_fn function to calculate the GIA from dose_A and dose_B combinations and given set of parameters.  Default is base_GIA
#' @param S_fn Function to calculate S. Default is calc_S_base
#' @param fn_list additional arguments to pass to GIA_fn
#' @param alpha alpha level used to produce CIs.  The bootstrap will use a two-tailed method.  The default is .05 to produce a 95\% CI
#' @param verbose logical indicating whether we should print where we are in the process.  Default is FALSE.
#' @return a list with the following elements
#' \itemize{
#' \item{params_est}{a data frame of dimension # of params x 4 where each row in the data frame is a parameter and where the columns are the mean, lower, alpha/2 quantile,  and upper,100 - alpha/2 quantile}
#' \item{S_est}{ a data frame of one row  x 4 where we provide the mean, lower, and upper estimates}
#' \item{GIA_est}{the original data with additional columns of the mean, lower, and upper estimates for each dose combination}
#' \item{SSE}{Sum of Square Error for the model under the best (mean) parameters}
#'}
#' @export
#' @importFrom stats optim lm quantile rnorm
#' @examples
#'
#' df <- loewesadditivity::cyrpa_ripr
#' df$dose_A <- df$CyRPA
#' df$dose_B <- df$RIPR
#' data <- fortify_gia_data(df)
##
#' model_params <- c("beta_A" = .5, "beta_B" = .5,
#'                  "gamma_A" = .5, "gamma_B" = .5,
#'                  "tau_1" = 0, "tau_2" = 0)
#' n_boot <- 10
#' GIA_fn <- base_GIA
#' S_fn <- calc_S_base
#' fn_list <- NULL
#' alpha <- .05
#' verbose <- FALSE
#' out <- estimate_params(data = data,
#' init_params = model_params,
#' n_boot = n_boot,
#' GIA_fn = GIA_fn,
#' S_fn = S_fn,
#' fn_list = fn_list,
#' alpha = alpha,
#' verbose = verbose)
#' names(out)
estimate_params <- function(data,
                            init_params = c(
                              "beta_A" = .25,
                              "beta_B" = .25,
                              "gamma_A" = .5,
                              "gamma_B" = .5,
                              "tau_1" = 0,
                              "tau_2" = 0
                            ),
                            n_boot = 100,
                            GIA_fn = base_GIA,
                            S_fn = calc_S_base,
                            fn_list = NULL,
                            alpha = .05,
                            verbose = FALSE){
  ## Set up output
  params_est <- data.frame(param = names(init_params),
                           mean = NA,
                           lower = NA,
                           upper = NA)
  S_est <- data.frame(stat = "S", mean = NA,
                      lower = NA,
                      upper = NA)
  GIA_est <- data


  ## Get the mean/best parameters
  if(verbose){
    print("Estimating best parameters")
  }

  best_params <- stats::optim(par = init_params,
                       fn = SSE_GIA,
                       GIA_fn = GIA_fn,
                       fn_list = fn_list,
                       data = data)
  params_est$mean <- best_params$par
  SSE <- best_params$val
  S_est$mean <- S_fn(best_params$par, fn_list = fn_list)
  GIA_est$mean <- GIA_fn(model_params = best_params$par,
                         dose_A = data$dose_A,
                         dose_B = data$dose_B,
                         fn_list = fn_list)

  if(n_boot > 0){
    if(verbose){
      print("Starting the bootstrap")
    }

    boot_results <- boot_GIA(par = best_params$par,
                             gia_df = data,
                             gia_est = GIA_est$mean,
                             n_boot = n_boot,
                             GIA_fn = GIA_fn,
                             S_fn = S_fn,
                             fn_list = fn_list,
                             verbose = verbose)

    ## put boot results back in place
    ## ## PARAMS
    params_est$lower <- params_est$mean + boot_results$par_est$lower
    params_est$upper <- params_est$mean + boot_results$par_est$upper
    ## S
    S_est$lower <- S_est$mean + boot_results$S_est$lower
    S_est$upper <- S_est$mean + boot_results$S_est$upper
    ## df
    GIA_est$lower <- GIA_est$mean + boot_results$GIA_est$lower
    GIA_est$upper <- GIA_est$mean + boot_results$GIA_est$upper
  }

  out <- list(params_est = params_est,
       S_est = S_est,
       GIA_est = GIA_est,
       SSE = SSE)
  class(out) <- c("loewes_list", class(out))
  return(out)

}

