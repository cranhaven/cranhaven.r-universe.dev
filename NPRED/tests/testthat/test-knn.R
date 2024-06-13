skip("skip")

library(NPRED)

#-----------------------------------------------------------------
# general test on knn conditional bootstrap for resampling
data(data1) # AR9 model   x(i)=0.3*x(i-1)-0.6*x(i-4)-0.5*x(i-9)+eps
x <- data1[, 1] # response
py <- data1[, -1] # possible predictors
t <- ts(1:seq_len(x))

ans.ar9 <- stepwise.PIC(x, py) # identify the meaningful predictors and estimate partial weights

z <- py[, ans.ar9$cpy] # predictor matrix
pw <- ans.ar9$wt # partial weights
# zout = apply(z,2,mean)  #vector denoting where we want outputs, can be a matrix representing grid.
zout <- z # vector denoting where we want outputs, can be a matrix representing grid.

cur <- 250
fut <- 250
n <- cur + fut

xhat1 <- knn(x[1:cur], z[1:cur, ], zout[(cur - fut + 1):cur, ],
  reg = T, pw =
    pw
) # knn regression estimate using partial weights.

# Mean of the conditional bootstrap estimate should be approximately
# the same as the regression estimate.
xhat2 <- knn(x[1:cur], z[1:cur, ], zout[(cur - fut + 1):cur, ],
  reg = F, pw =
    pw
) # alternatively, knn conditional bootstrap (100 realisations).

ts.plot(
  ts(cbind(x[(cur + 1):n], xhat1, xhat2, rowMeans(xhat2))),
  col = c("purple", "black", rep("red", 100), "green"),
  lwd = c(3, rep(1, 2 + 100))
)

# boxplot(as.numeric(xhat2));points(1,xhat1);points(1,mean(xhat2),pch=2)


#-----------------------------------------------------------------
# general test on knn regression with extraplation
data(data3)
nobs <- 500
ndim <- 15
x <- ts(data3[, 1]) # response
z <- ts(data3[, -1]) # possible predictors
zout <- ts(data.gen.ar1(nobs, ndim = ndim)$dp) # new input

xhat1 <- xhat2 <- matrix(nrow = nobs, ncol = 1)
# xhat1 <- NPRED::knn(x,z,zout,k=5,reg=T,extrap=F)
# xhat2 <- NPRED::knn(x,z,zout,k=5,reg=T,extrap=T)

if (TRUE) {
  # start.time <- Sys.time()
  for (i in 1:nobs) {
    xhat1[i] <- NPRED::knn(x[-i],
      z[-i, ],
      zout[i, ],
      k = 5,
      reg = T,
      extrap = F
    )
    xhat2[i] <- NPRED::knn(x[-i],
      z[-i, ],
      zout[i, ],
      k = 5,
      reg = T,
      extrap = T
    )
  }
  # print(Sys.time()-start.time)
}

xhat1 - xhat2
# ts.plot(x,xhat1,xhat2,col=c("black","red","blue"),ylim=c(-10,10), lwd=c(1,1,2))

par(mfrow = c(1, 1))
plot(xhat1,
  xhat2,
  xlim = c(-10, 10),
  ylim = c(-10, 10)
)
abline(coef = c(0, 1), lwd = 1)
