#' @rdname confint.cgaim
#' @order 2
#' 
#' @export
confint.boot.cgaim <- function(object, parm, level = 0.95, ...)
{
  #----- Header
  
  # get object
  n <- nrow(object$obs$gfit)
  ptot <- ncol(object$obs$gfit)
  p <- length(object$obs$alpha)
  
  # Setup parm. If missing get all parameters
  if (missing(parm)){
    parm <- 1:3
  } else {      
    if (is.character(parm)){
      parm <- stats::na.omit(match(parm, c("alpha", "beta", "g")))
    } else {
      parm <- parm[parm %in% 1:3]
    }
    if (!any(parm %in% 1:3)){
      stop(paste0("'parm' must be in 1:3"))
    }
  }
  
  # Setup CI level
  alims <- c((1 - level) / 2, 1 - (1 - level) / 2)
  level.labels <- sprintf("%s%%", alims * 100)
  
  #----- Compute CIs
  res <- list()
  if (1 %in% parm){
    res$alpha <- t(apply(object$boot$alpha, 1, stats::quantile, 
      alims, na.rm = TRUE))
    
    # Names
    rownames(res$alpha) <- names(unlist(object$obs$alpha))
    colnames(res$alpha) <- level.labels
  }
  if (2 %in% parm){
    res$beta <- t(apply(object$boot$beta, 1, stats::quantile, 
      alims, na.rm = TRUE))
    
    # Names
    rownames(res$beta) <- names(object$beta)
    colnames(res$beta) <- level.labels
  }
  
  # Compute CI for curves
  if (3 %in% parm){
    res$g <- array(NA, dim = c(n, ptot, 2), 
      dimnames = list(NULL, colnames(object$obs$gfit), level.labels))
    for (i in seq_len(ptot)){
      if (i %in% object$obs$index){
        zseq <- object$obs$indexfit[,i]
        gext <- suppressWarnings(
          mapply(function(x, y) stats::spline(x, y, xout = zseq)$y, 
            x = as.data.frame(object$boot$indexfit[,i,]), 
            y = as.data.frame(object$boot$gfit[,i,])))
        res$g[,i,] <- t(apply(gext, 1, stats::quantile, probs = alims))
      } else {
        zseq <- object$obs$sm_mod$Xcov[,i - p]
        res$g[,i,] <- t(apply(object$boot$gfit[,i,], 1, stats::quantile, 
          probs = alims))[order(zseq),]
      }
    }
  }
  
  #----- Return
  
  return(res)
}