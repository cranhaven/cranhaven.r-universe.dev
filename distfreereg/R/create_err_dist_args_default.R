create_err_dist_args_default <- function(err_dist_fun_formals, cov_args, reps, n,
                                         true_covariance){
  err_dist_args_default <- list()
  for(a in c("reps", "n")){
    if(a %in% err_dist_fun_formals) err_dist_args_default[[a]] <- get(a)
  }

  if("Sigma" %in% cov_args)
    err_dist_args_default[["Sigma"]] <- true_covariance[["Sigma"]]
  if("SqrtSigma" %in% cov_args)
    err_dist_args_default[["SqrtSigma"]] <- true_covariance[["SqrtSigma"]]
  if("P" %in% cov_args)
    err_dist_args_default <- c(err_dist_args_default, list(P = true_covariance[["P"]]))
  if("Q" %in% cov_args)
    err_dist_args_default <- c(err_dist_args_default, list(Q = true_covariance[["Q"]]))
  return(err_dist_args_default)
}
