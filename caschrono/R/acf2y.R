acf2y <- function(y, lag.max=40, numer=TRUE) {
# Shumway function modified
  num <- length(y)
  ACF1 <- stats::acf(as.vector(y), lag.max, plot=FALSE)$acf[-1,1,1]
  PACF <- stats::pacf(as.vector(y), lag.max, plot=FALSE)$acf[,1,1]
  LAG <- 1:lag.max
  minA <- min(ACF1)
  minP <- min(PACF)
  maxA <- max(ACF1)
  maxP <- max(PACF)
  U <- 2/sqrt(num)
  L <- -U
  minu <- max(-1, min(minA, minP, L) - .01) 
  maxu <- min(1, max(maxA, maxP) + 0.05)

  ############### graphical parameters
  mai.n <- c(0, 0.8, 0.4, 0.1) 
  mai.s <- c(0.9, mai.n[2], 0, 0.1)
  lar <- 7
  hau <- 7  
  a <- lar - mai.n[2] 
  b <- (hau - mai.s[1] - mai.n[3]) / 2 
  fig.n <- c(0, 1, (mai.s[1] + b) / hau, 1)
  fig.s <- c(0, 1, 0, (mai.s[1] + b) / hau)
  
  ############# 1st figure 
  op1 <- par(fig = fig.n, mai = mai.n)
  plot(LAG, ACF1, type = "h", ylim = c(minu, maxu), xlab = "", 
     xaxt = "n", ylab = "ACF", main = paste("Time series: ", deparse(substitute(y))), 
     cex = .8, las = 1, cex.lab = 0.9, cex.axis = .8)
  abline(h = 0)
  abline(h = L, lty = "dashed", col = "blue")
  abline(h = U, lty = "dashed", col = "blue")
  
  # ############ 2nd figure
  op2 <- par(new = TRUE, fig = fig.s, mai = mai.s)
  plot(LAG, PACF, type = "h", ylim = c(minu, maxu), xlab = "Lag",
       ylab = "PACF", cex = .8, las = 1, cex.lab = 0.9, cex.axis = .8)
  abline(h = 0)
  abline(h = L, lty = "dashed", col = "blue")
  abline(h = U, lty = "dashed", col = "blue")
  par(op2)
  par(op1)
  # return result
  if (numer)  
    return(cbind(LAG, ACF1, PACF))
}
