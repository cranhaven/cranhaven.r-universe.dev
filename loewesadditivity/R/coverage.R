
#' Simulate a GIA model with an assumed error structure
#'
#' @param n_sims number of coverage simulations
#' @param n_boot number of bootstraps to use in each simulation
#' @param verbose logical indicating whether we should use print statements.  Default is TRUE
#' @param experimental_grid data frame with columns 'dose_A' and 'dose_B'
#' @param model_par named vector of parameters corresponding to those used in GIA_fn()
#' @param alpha alpha level used to produce confidence intervals for each bootstrap
#' @param noise_par named vector for the noise parameter.  Must have names "a0" and "a1".  See \code{?base_gia} for more details.
#' @param GIA_fn function used to calculate GIA.  Default is base_GIA().
#' @param S_fn function to calculate S
#' @param fn_list additional parameters to pass to GIA_fn
#' @return list with the following entries
#' \describe{
#' \item{interaction_cov}{This is the percent of times 0 was in the (1-alpha)\% confidence interval for the interaction term "tau_1" from the simulated results}
#' \item{params_cov}{This is the percent of times the true model parameter (those from model_par) lies in the (marginal) 95\% confidence interval for that model parameter.}
#' \item{tau_pos}{This is the percent of times the (1-alpha)\% CI of "tau_1" was completely above 0.}
#' \item{tau_neg}{This is the percent of times (1-alpha)\% CI of "tau_1" is completely below zero}
#' }
#' @export
#' @examples
#' df <- loewesadditivity::cyrpa_ripr
#' df$dose_A <- df$CyRPA
#' df$dose_B <- df$RIPR
#' data <- fortify_gia_data(df)
##
#' model_params <- c("beta_A" = .247, "beta_B" = .224,
#'                   "gamma_A" = .734, "gamma_B" = .806,
#'                   "tau_1" = .28, "tau_2" = -.28)
#' experimental_grid <- make_grid(par = model_params,
#'                                n = 5)
#' n_boot <- 100
#' n_sims <- 10
#' GIA_fn <- base_GIA
#' S_fn <- calc_S_base
#' fn_list <- NULL
#' alpha <- .05
#' verbose <- TRUE
#' ## NOT RUN
#' ##out <- simulate_coverage(n_sims = n_sims,
#'   ##                      n_boot = n_boot,
#'    ##                     verbose = TRUE,
#'     ##                    experimental_grid = experimental_grid,
#'      ##                   model_par = model_params,
#'      ##                   alpha = .05,
#'    ##                     noise_par = c("a0" = 3, "a1" = .01),
#'    ##                     GIA_fn = base_GIA,
#'    ##                     fn_list = NULL)
#' ##out
simulate_coverage <- function(n_sims = 10,
                              n_boot = 100,
                              verbose = TRUE,
                              experimental_grid,
                              model_par,
                              alpha = .05,
                              noise_par = c("a0" = 2, "a1" = .01),
                              GIA_fn = base_GIA,
                              S_fn = calc_S_base,
                              fn_list = NULL){


  if(verbose){
    print("WARNING:  This process may take a while")
  }


  df <- experimental_grid
  L <- n_sims
  df$GIA_obs <- GIA_fn(model_params = model_par,
                   dose_A = df$dose_A, dose_B = df$dose_B,
                   fn_list = fn_list)

  gia_err <- matrix(0, nrow = nrow(df), ncol = L)
  tau_zero <- numeric(L)
  tau_pos <- numeric(L)
  tau_neg <- numeric(L)
  par_cov <- matrix(0, nrow = L, ncol = length(model_par))

  sd2 <- abs((100 - df$GIA_obs)^2 * noise_par["a1"] + noise_par["a0"])

    #    L <- 100
    for(ii in 1:L){

      if(verbose & ((ii %% 5) == 0)) print(paste("Simulation", ii))


      df$GIA <- df$GIA_obs + rnorm(n = nrow(df), mean = 0, sd = sqrt(sd2))
      out <- estimate_params(data = df,
                             init_params = model_par,
                             n_boot = n_boot,
                             GIA_fn = GIA_fn,
                             S_fn = S_fn,
                             fn_list = fn_list,
                             alpha = alpha,
                             verbose = FALSE)


      par_df <- out$params_est
      low <- par_df$lower
      names(low) <- par_df$param
      high <- par_df$upper
      names(high) <- par_df$param
      if( (0 >= low["tau_1"]) &
          (0 <= high["tau_1"])){
        tau_zero[ii] <- 1 ## zero is in the interval
      } else if(0 > high["tau_1"]){
        tau_neg[ii] <- 1 ## interval completely below tau1=0
      } else if(0 < low["tau_1"]){
        tau_pos[ii] <- 1 ## interval completely above tau1=0
      }
      for(jj in 1:length(model_par)){
        if((model_par[jj] >= low[jj]) &
           (model_par[jj] <= high[jj])){
          par_cov[ii,jj] <- 1
        }
      }
    }
  # browser()
  # print("whee")
  interaction_cov <- sum(tau_zero) / L * 100
  pos_tau <- sum(tau_pos) / L * 100
  neg_tau <- sum(tau_neg) / L * 100
  params_cov <- colSums(par_cov) / L * 100
  names(params_cov) <- out$params_est$param


  return(list(interaction_cov = interaction_cov,
              params_cov = params_cov,
              pos_tau = pos_tau,
              neg_tau = neg_tau))

}
