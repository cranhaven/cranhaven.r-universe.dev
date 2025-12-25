#' @export
cv_multivar <- function(B, Z, Y, W, Ak, bk, k, d, lambda1, lambda2, ratios, t1, t2, eps,intercept=FALSE, cv, nfolds){
  
  if(cv == "rolling"){
    
    res <- cv_rolling(B, Z, Y, W, Ak, k, d, lambda1, ratios, t1, t2, eps,intercept=FALSE, cv, nfolds)
  
  } else if (cv == "blocked"){
    
    res <- cv_blocked(B, Z, Y, W, Ak, k, d, lambda1, ratios, t1, t2, eps,intercept=FALSE, cv, nfolds)
  
  } else {
    
    stop(paste0("multivar ERROR: ", cv, " is not a supported cross-validation method."))
  }
  
  return(res)
  
}