if (!requireNamespace("fda", quietly = TRUE)) {
  stop("fda package is needed for this demo to work. Please install it.",
       call. = FALSE)
}
if (!requireNamespace("MASS", quietly = TRUE)) {
  stop("MASS package is needed for this demo to work. Please install it.",
       call. = FALSE)
}

library(MASS)
library(fda)
library(freqdom)
library(freqdom.fda)
library(pcdpca)
data(pm10)

X = center.fd(pm10)
n = dim(X$coef)[2]

rev.freqdom = function(XI){
  XI$freq = rev(XI$freq)
  XI
}
MSE = function(X,Y=0){ sum((X-Y)**2) / nrow(X) }

## Static PCA ##
PR = prcomp(t(X$coef))
Y1 = PR$x
Y1[,-1] = 0
Xpca = Y1 %*% t(PR$rotation)

## Dynamic PCA ##
XI.est = dpca(t(X$coef),q=4,freq=pi*(-150:150/150),Ndpc = 1)  # finds the optimal filter
Y.est = XI.est$scores
Xdpca.est = XI.est$Xhat

## Periodically correlated PCA ##
XI.est = pcdpca(t(X$coef),q=3,freq=pi*(-150:150/150),period=7)  # finds the optimal filter
Y.pcest = pcdpca.scores(t(X$coef), XI.est)  # applies the filter
Y.pcest[,-1] = 0 # forces the use of only one component
Xpcdpca.est = pcdpca.inverse(Y.pcest, XI.est)  # deconvolution

# Creates functional objects
Xpcdpca.est.fd = fd(t(Re(Xpcdpca.est)),basis=X$basis)
Xdpca.est.fd = fd(t(Re(Xdpca.est)),basis=X$basis)
Xpca.fd = fd(t(Xpca),basis=X$basis)

# Write down results
ind = 15:(n-15)
cat("NMSE PCA = ")
cat(MSE(t(X$coef)[ind,],Xpca[ind,]) / MSE(t(X$coef)[ind,],0))
cat("\nNMSE DPCA =  ")
cat(MSE(t(X$coef)[ind,],Xdpca.est[ind,]) / MSE(t(X$coef)[ind,],0))
cat("\nNMSE PCDPCA = ")
cat(MSE(t(X$coef)[ind,],Xpcdpca.est[ind,]) / MSE(t(X$coef)[ind,],0))
cat("\n")

# Figure 1: 10 observations reconstructed from the first component
ind = 20 + 1:10
par(mfrow=c(2,2),ps = 12, cex = 1, cex.main = 1)
plot(X[ind],ylim=c(-5,3),xlab="Intraday time", ylab="Sqrt(PM10)",lwd=2)
title("Original curves")
plot(Xpca.fd[ind],ylim=c(-5,3),xlab="Intraday time", ylab="Sqrt(PM10)",lwd=2)
title("PCA curves")
plot(Xdpca.est.fd[ind],ylim=c(-5,3),xlab="Intraday time", ylab="Sqrt(PM10)",lwd=2)
title("DPCA curves")
plot(Xpcdpca.est.fd[ind],ylim=c(-5,3),xlab="Intraday time", ylab="Sqrt(PM10)",lwd=2)
title("PC-DPCA curves")
par(mfrow=c(1,1))

# Figure 2: Scores: static, dynamic and the differences
par(mfrow=c(1,3),ps = 12, cex = 1, cex.main = 1)
plot(Re(Y.est[,1]),t='l', ylab="1st DFPC scores", xlab="Time [days]",ylim=c(-7,7))
plot(Re(Y.pcest[,1]),t='l', ylab="1st PC-DFPC scores", xlab="Time [days]",ylim=c(-7,7))
plot(Re(Y.pcest[,1]-Y.est[,1]),t='l', ylab="Differences", xlab="Time [days]",ylim=c(-7,7))
par(mfrow=c(1,1))

# Figure 3: 3 elements of the first filter
d = 2
midpoint = 4
days = c(4:7,1:3)
for (day in 1:length(days)){
  for (i in (midpoint - d):(midpoint + d)){
    F = fd((XI.est$operators[1,1:15 + 15*(days[day]-1),i]),X$basis)
    F$basis$rangeval = i - midpoint + c(0,1)
    if ((i == midpoint - d) && (day == 1)){
      xlim = c(-d,d+1)
      plot(F,xlim=xlim,ylim=c(-0.3,0.65),xlab="", ylab="",xaxt='n',lwd=4,col=day,bty="n")
    }
    else {
      lines(F,lwd=4,col=day)
    }
    if (i == midpoint)
      abline(h=0,lty=1)
    abline(v=F$basis$rangeval[1],lty=2)
    abline(v=F$basis$rangeval[1]+1,lty=2)
  }
}

legend(-1.97, 0.6, c("1 - Monday","2 - Tuesday","3 - Wednesday","4 - Thursday","5 - Friday","6 - Saturday","7 - Sunday"), col = 1:7, lty=1, lwd=3)
