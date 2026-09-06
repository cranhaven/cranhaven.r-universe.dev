
#' @rdname summary
#' @export

summary.HDBRR <- function(object, all.coef = FALSE, crit = log(4), ...){
  if(!inherits(object, "HDBRR")) stop("This function only works for objects of class 'HDBRR'\n");
  Inter <- object$intercept
  tval <- as.matrix(object$betahat/sqrt(object$varb))
  p <- abs(object$phat)
  if(is.null(p)){
    cat("\n")
    cat("c must be different to NULL to obtain the Bayes factor.\n\n")
    coefs <- as.matrix(rbind(object$betahat))
    varb <- as.matrix(rbind(sqrt(object$varb)))
    if(Inter == 1){
      files <- as.matrix(c("(Intercept)", colnames(fit$x)[-1]))
      d <- which(duplicated(files))[1]
      if(!is.na(d)){
        p <- dim(as.matrix(fit$x))[2]
        colnames(fit$x)[-1] <- paste("X",1:(p-1),sep="")
      }
      files <- as.matrix(c("(Intercept)", colnames(fit$x)[-1]))
      summary <- data.frame(t(coefs), t(varb), tval, row.names = files)
      colnames(summary) <- c("Estimate", "Std. dev", "SNR")
    }
    else{
      files <- as.matrix(colnames(object$x))
      d <- which(duplicated(files))[1]
      if(!is.na(d)){
        p <- dim(as.matrix(object$x))[2]
        colnames(object$x) <- paste("X",1:p,sep="")
      }
      files <- as.matrix(colnames(object$x))
      summary <- data.frame(t(coefs), t(varb), tval, row.names = files)
      colnames(summary) <- c("Estimate", "Std. Error", "SNR")
    }
  }
  else{
    oddvalues <- as.matrix(abs(object$phat)/(1+10E-6-abs(object$phat)))
    oddvalues <- 2*log(oddvalues)
    oddvaluessig <- as.matrix(rep(0, length(oddvalues)))
    for (j in 1:length(oddvalues)) {
      if(oddvalues[j] > 10)
      {oddvaluessig[j] <- "***"}
      else{
        if(oddvalues[j] > 6 && oddvalues[j] <= 10)
        {oddvaluessig[j] <- "**"}
        else{
          if(oddvalues[j] >= 2 && oddvalues[j] <= 6)
          {oddvaluessig[j] <- "*"}
          else{oddvaluessig[j] <- " "}
        }
      }
    }
    coefs <- as.matrix(rbind(object$betahat))
    varb <- as.matrix(rbind(sqrt(object$varb)))
    coefs_sign <- which(oddvalues >= crit)
    if(all.coef == TRUE){
      if(Inter == 1){
        files <- as.matrix(c("(Intercept)", colnames(object$x)[-1]))
        d <- which(duplicated(files))[1]
        if(!is.na(d)){
          p <- dim(as.matrix(object$x))[2]
          colnames(object$x)[-1] <- paste("X",1:(p-1),sep="")
        }
        files <- as.matrix(c("(Intercept)",colnames(object$x)[-1]))
        summary <- data.frame(t(coefs), t(varb), tval, oddvalues, oddvaluessig, row.names = files)
        colnames(summary) <- c("Estimate", "Std. dev", "SNR", "2ln(BF))", " ")
      }
      else{
        files <- as.matrix(colnames(object$x))
        d <- which(duplicated(files))[1]
        if(!is.na(d)){
          p <- dim(as.matrix(object$x))[2]
          colnames(object$x) <- paste("X",1:p,sep="")
        }
        files <- as.matrix(colnames(object$x))
        summary <- data.frame(t(coefs), t(varb), tval, oddvalues, oddvaluessig, row.names = files)
        colnames(summary) <- c("Estimate", "Std. dev", "SNR", "2ln(BF)", " ")
      }
    }
    else{
      if(Inter == 1){
        files <- as.matrix(c("(Intercept)", colnames(object$x)))[coefs_sign]
        d <- which(duplicated(files))[1]
        if(!is.na(d)){
          p <- dim(as.matrix(object$x))[2]
          colnames(object$x)[-1] <- paste("X",1:(p-1),sep="")
        }
        names <- colnames(object$x)[-1]
        files <- as.matrix(c("(Intercept)",names))[coefs_sign]
        summary <- data.frame(t(coefs)[coefs_sign], t(varb)[coefs_sign], tval[coefs_sign],
                              oddvalues[coefs_sign], oddvaluessig[coefs_sign], row.names = files)
        colnames(summary) <- c("Estimate", "Std. dev", "SNR", "2ln(BF)", " ")
      }
      else{
        files <- as.matrix(colnames(object$x)[coefs_sign])
        d <- which(duplicated(files))[1]
        if(!is.na(d)){
          p <- dim(as.matrix(object$x))[2]
          colnames(object$x) <- paste("X",1:p,sep="")
        }
        files <- as.matrix(colnames(object$x)[coefs_sign])
        summary <- data.frame(t(coefs)[coefs_sign], t(varb)[coefs_sign], tval[coefs_sign],
                              oddvalues[coefs_sign], oddvaluessig[coefs_sign], row.names = files)
        colnames(summary) <- c("Estimate", "Std. dev", "SNR", "2ln(BF)", " ")
      }
    }
  }
  model <- object$call
  lambda <- round(object$uhat/(1-object$uhat),4)
  edf <- object$edf
  res <- list(call = model,summary = summary,lambda = lambda, edf = edf)
  class(res) <- "summary.HDBRR"
  res
}

