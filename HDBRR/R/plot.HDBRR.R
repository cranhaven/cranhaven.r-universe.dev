
#' @rdname plot
#' @export
#' @importFrom graphics plot lines abline text par

plot.HDBRR <- function(x, crit = log(4), var_select = FALSE, post = FALSE, ...){
  fit <- x
  if(!inherits(fit, "HDBRR")) stop("This function only works for objects of class 'HDBRR'\n");
  n <- nrow(fit$x)
  phat <- abs(fit$phat)
  betahat <- fit$betahat
  bf <- phat/(1-phat)
  bf <- 2*log(bf)
  id.pos2 <- which(bf >= crit)
  Inter <- fit$intercept
  if(Inter == 1){
    files <- as.matrix(c("(Intercept)", colnames(fit$x)[-1]))
    d <- which(duplicated(files))[1]
    if(!is.na(d)){
      p <- dim(as.matrix(fit$x))[2]
      colnames(fit$x)[-1] <- paste("X",1:(p-1),sep="")
    }
    files <- as.matrix(c("(Intercept)", colnames(fit$x)[-1]))
  }
  else{
    files <- as.matrix(colnames(fit$x))
    d <- which(duplicated(files))[1]
    if(!is.na(d)){
      p <- dim(as.matrix(fit$x))[2]
      colnames(fit$x) <- paste("X",1:p,sep="")
    }
    files <- as.matrix(colnames(fit$x))
  }
  if(var_select == TRUE){
    par(mfrow=c(1,1))
    plot(betahat,phat, xlab = expression(paste(hat(beta))), ylab = expression(paste(hat(p))))
    points(betahat[id.pos2],phat[id.pos2],col=2)
    text(betahat[id.pos2],phat[id.pos2],files[id.pos2])
    readline(prompt="Press [enter] to continue to next graph")
  }
  par(mfrow=c(1,1))
  plot(fit$y,fit$yhat, xlab = "y observed", ylab = "y predicted")
  abline(a=0,b=1)
  text(quantile(fit$yhat)[[5]],-quantile(fit$yhat)[[5]],paste("cor=",round(cor(fit$y,fit$yhat),4)))
  readline(prompt="Press [enter] to continue to next graph")
  par(mfrow=c(1,1))
  plot(fit$betahat, ylab = "Coefficients")
  readline(prompt="Press [enter] to continue to next graph")
  par(mfrow=c(1,1))
  plot(sqrt(fit$varb),ylab="Std. dev.")
  readline(prompt="Press [enter] to continue to next graph")
  par(mfrow=c(1,1))
  plot(fit$betahat/sqrt(fit$varb),ylab="SNR")
  points(id.pos2,fit$betahat[id.pos2]/sqrt(fit$varb[id.pos2]),col="red")
  abline(h=-qt(0.975,n-1))
  abline(h=qt(0.975,n-1))
  if(post == TRUE){
    readline(prompt="Press [enter] to continue to next graph")
    par(mfrow=c(1,1))
    plot(fit$u, fit$postu, type = "l", xlab = "u", ylab = expression(paste(pi (u))), main = "Marginal posterior of u")
  }
}
