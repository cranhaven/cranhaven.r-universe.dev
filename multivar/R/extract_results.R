#' @export
extract_results <- function(x){
  
  # traditional
  MSFE_mean    <- colMeans(x$MSFE)
  msfe_min_idx <- which.min(MSFE_mean)
  B <- x$beta[,,msfe_min_idx]
  #plot(MSFE_mean, col=ifelse(c(1:length(MSFE_mean))==msfe_min_idx, "red", "black"))
  
  # min + 1 se
  # find the set of lambda1s we are interested in
  # lambda_set <- floor(msfe_min_idx/ x$obj@nlambda1)
  # idx_range <-  c(((lambda_set*x$obj@nlambda1)+1):((lambda_set+1)*x$obj@nlambda1))
  # MSFE_mean_reduced <- MSFE_mean[idx_range]
  # v.mse_se <- apply(x$MSFE[,idx_range],2,sd)/sqrt(nrow(x$MSFE))
  # mse_se_la_min <- v.mse_se[msfe_min_idx %% x$obj@nlambda2]
  # msfe_val <- max(MSFE_mean_reduced[MSFE_mean_reduced < (MSFE_mean[msfe_min_idx] + mse_se_la_min)])
  # msfe_min_idx_se <- idx_range[which(MSFE_mean_reduced==msfe_val)]
  # B <- x$beta[,,msfe_min_idx_se]
  #plot(MSFE_mean, col=ifelse(c(1:length(MSFE_mean))==msfe_min_idx_se, "red", "gray"))

  mats <- breakup_transition(B, x$obj@Ak, x$obj@ndk, x$obj@intercept)
  
  
  
  return(mats)
}
