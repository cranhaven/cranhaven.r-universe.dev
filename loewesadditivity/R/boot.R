
#' Helper function for the bootstrap results
#'
#' @param par named vector of parameters, that correspond to those used in 'GIA_fn'.
#' @param gia_df data frame with the following columns
#' \itemize{
#' \item{dose_A}{dose A mg/mL}
#' \item{dose_B}{dose B mg/mL}
#' \item{GIA}{GIA}
#' }
#' @param gia_est estimated values of GIA (these will be used as the 'truth')
#' @param n_boot number of boot straps to use to estimate confidence intervals of the parameters, GIA estimates, and values of S.  The default is 100.  If n_boot = 0, then no bootstraps will be run and only the point estimates will be returned.
#' @param alpha value of alpha.  Default is .05
#' @param GIA_fn function to calculate the GIA from dose_A and dose_B combinations and given set of parameters.  Default is base_GIA
#' @param S_fn Function to calculate S. Default is calc_S_base
#' @param fn_list additional arguments to pass to GIA_fn
#' @param verbose logical indicating whether we should print where we are in the process.  Default is FALSE.
#' @return a list with the following elements
#' \itemize{
#' \item{params_est}{a data frame of dimension # of params x 4 where each row in the data frame is a parameter and where the columns are the mean, lower, alpha/2 quantile,and upper,100 - alpha/2 quantile}
#' \item{S_est}{ a data frame of one row  x 4 where we provide the mean, lower, and upper estimates}
#' \item{GIA_est}{the original data with additional columns of the mean, lower, and upper estimates for each dose combination}
#'
#'}
boot_GIA <- function(par, gia_df,
                     gia_est,
                     n_boot = 100,
                     alpha = .05,
                     GIA_fn = base_GIA,
                     S_fn = calc_S_base,
                     fn_list = NULL,
                     verbose = FALSE){

  if(verbose){
    print("Generating parametric error")
  }
  # browser()
  ## Get standard error
  res2 <- (gia_est - gia_df$GIA)^2
  x <- (100 - gia_df$GIA)^2
  mod <- lm(res2~x)
  sd <- sqrt(abs(mod$fit))
  obs_gia <- gia_df$GIA
  df <- gia_df
  par_mat <- matrix(0, nrow = n_boot, ncol = length(par))
  S_vec <- numeric(n_boot)
  df_mat <- matrix(0, nrow = nrow(gia_df), ncol = n_boot)
  for(ii in 1:n_boot){
    if(verbose & (ii %% 100) == 0) print(paste("Boot strap:", ii))
    df$GIA <- gia_est + rnorm(length(gia_est), mean = 0, sd = sd)
    best_pars <- optim(par = par, fn = SSE_GIA,
                       data = df,
                       GIA_fn = GIA_fn,
                       fn_list = fn_list)
    par_mat[ii,] <- best_pars$par - par  ## Changed 1/10/2020
    df_mat[,ii] <- GIA_fn(model_params = best_pars$par,
                          dose_A = df$dose_A,
                          dose_B = df$dose_B,
                          fn_list = fn_list
                          ) - df$GIA
    S_vec[ii] <- S_fn(best_pars = best_pars$par,
                      fn_list = fn_list)
  }

  S_q <- quantile(S_vec, prob = c(alpha /2, .5, 1 - alpha/2), na.rm = TRUE)
  S_est <- data.frame(lower = S_q[1]- mean(S_vec),
                      upper = S_q[3] - mean(S_vec))

  par_q <- apply(par_mat, 2, quantile, prob = c(alpha/2, .5, 1 - alpha/2), na.rm = TRUE)
  par_est <- data.frame(lower = par_q[1,],# - colMeans(par_mat),
                        upper = par_q[3,]# - colMeans(par_mat)
                        )
  GIA_q <- t(apply(df_mat, 1, quantile, prob = c(alpha/2, .5, 1 - alpha/2), na.rm = TRUE))
  GIA_est <- data.frame(lower = GIA_q[,1],
                        upper = GIA_q[,3]
  )
  return(list(par_est = par_est,
              S_est = S_est,
              GIA_est = GIA_est))




}


