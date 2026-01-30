fc_w_par.exp_Haar <- function(y,X,sig_inv_Xt){
  p = ncol(X)
  n       = length(y)
  beta_hat = sig_inv_Xt %*% y
  rss    = sum( (y - X %*% beta_hat )^2)
  alpha_sq = rss/rchisq(1,df=n)
  alpha_sq
}