#' @rdname pcps.curve
#' @encoding UTF-8
#' @export
summary.pcpscurve <- function(object, probs = c(0.025, 0.975), ...){
  res <- list()
  res$call <- object$call
  res$curve.obs <- object$curve.obs
  if(length(probs)!=2){
    stop("\n Only two values are accepted in probs \n")
  }
  org.curve <- function(x){
    X <- sapply(seq_len(length(x)), function(i) x[[i]][,1])
    Y <- sapply(seq_len(length(x)), function(i) x[[i]][,2])
    mean.X <- apply(X, 1, mean, na.rm = TRUE)
    mean.Y <- apply(Y, 1, mean, na.rm = TRUE)
    quantile.X <- apply(X, 1, stats::quantile, na.rm = TRUE, probs = probs)
    quantile.Y <- apply(Y, 1, stats::quantile, na.rm = TRUE, probs = probs)
    res <- t(rbind(mean.X, mean.Y, quantile.X, quantile.Y))
    rownames(res) <- rownames(object$curve.obs)
    colnames(res) <- c("Mean_Cum_PCPS_Eig", "Mean_Coe_Det", paste("Cum_PCPS_Eig_IC_", probs*100, "%",sep = ""), paste("Coe_Det_IC_", probs*100, "%", sep = ""))
    return(res)
  }
  if(!is.null(object$curve.null.ts)){
    res$null.model.ts <- org.curve(object$curve.null.ts)
  }	
  if(!is.null(object$curve.null.bm)){
    res$null.model.bm <- org.curve(object$curve.null.bm)
  }
  return(res)
}