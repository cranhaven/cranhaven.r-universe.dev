Correction1Vect <- function(X, Y, te=NULL, vector, top_grill.=c(1:length(vector)), delta=0.95){
  
  beta_interm <- sapply(top_grill., top, vect = vector)
  beta_te <- rbind(rep(te[1],length(top_grill.)), rep(te[2],length(top_grill.)), beta_interm)
  yhat <- as.matrix(X %*% beta_te)
  residuals <- sweep(yhat, 1, Y)
  mse_final_top <- colMeans(residuals^2)
  ratio_mse <- c()
  for (k in 1:(length(top_grill.) - 1)) {
    ratio_mse[k] <- round(mse_final_top[k + 1]/mse_final_top[k],6)
  }
  top_ratio <- min(which(ratio_mse >= delta))
  if (is.infinite(top_ratio)) {
    opt_final_top <-  length(vector)
  }
  else {
    opt_final_top <- top_grill.[top_ratio]
  }
  
  return(round(top(vect = vector, thresh = opt_final_top), 6))
  
}
