pwr.plot<-function(n=n, k=k, f=f, alpha=alpha){
  
  nPower <- function(n=n, k=k, f=f, alpha=alpha) {
    critF <- qf(1-alpha, k-1, k*n - k)
    1-pf(critF, k-1, k*n - k, k*n * f^2)
  }
  
  fPower <- function(f=f, k=k, n=n, alpha=alpha) {
    critF <- qf(1-alpha, k-1, k*n - k)
    pwrf<-1-pf(critF, k-1, k*n - k, k*n * f^2)
  }
  
  if (isTRUE(length(f)==1)){
      power.value<-sapply(n, fPower, k=k, f=f, alpha=0.05)
      plot(n, power.value, xlab="n", ylab="power", main=paste("Power curve for ANOVA test with effect size = ", f), lwd=2, col="red", type="l")
   } else if (isTRUE(length(n)==1)){
              power.value<-sapply(n, fPower, k=k, f=f, alpha=0.05)
              plot(f, power.value, xlab="effect size f", ylab="power", main=paste("Power curve for ANOVA test with sample size = ", n), lwd=2, col="red", type="l")
          } else{
                pwrn <- sapply(n, nPower, k=k, f=f, alpha=alpha) 
                matplot(f, pwrn, type="l", lty=1, lwd=2, xlab="effect size f", ylab="Power", xaxs="i",  xlim=c(-0.05, 1.1), col=1:length(n))
                legend(x="bottomright", legend=paste("N =", n), lwd=2, col=1:length(n))
                pwrf <- sapply(f, fPower, n=n, k=k, alpha=alpha) 
                matplot(n, pwrf, type="l", lty=1, lwd=2, xlab="sample size n", ylab="Power", xaxs="i",  xlim=c(0, max(n)), col=1:length(f))
                legend(x="bottomright", legend=paste("Effect Size =", f), lwd=2, col=1:length(f))
            }
}

