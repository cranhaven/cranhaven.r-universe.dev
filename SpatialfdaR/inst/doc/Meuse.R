## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SpatialfdaR)

## -----------------------------------------------------------------------------
p <- MeuseData$pts
e <- MeuseData$edg
t <- MeuseData$tri
np <- dim(p)[[1]]
ne <- dim(e)[[1]]
nt <- dim(t)[[1]]
covdata <- MeuseData$covdata

## -----------------------------------------------------------------------------
plotFEM.mesh(p, t, nonum=TRUE)

## -----------------------------------------------------------------------------
data <- matrix(0,nrow=np,ncol=2)
data[,1] <- covdata[,1]
data[,2] <- log(covdata[,2])
Dist <- as.matrix(covdata[,1])
Zinc <- as.matrix(log(covdata[,2]))
LogZinc <- log10(Zinc)

## -----------------------------------------------------------------------------
lsList <- lsfit(Dist, LogZinc)
print(paste("Regression coefficient =",round(lsList$coef[1],2)))
print(paste("Intercept =",round(lsList$coef[2],2)))

## -----------------------------------------------------------------------------
plot(Dist, LogZinc, type="p", xlim=c(0,0.9), ylim=c(0.65, 0.90),
     xlab = "Distance from shore",
     ylab = "Log10 Zinc Concentration")
abline(lsList$int, lsList$coef)

## -----------------------------------------------------------------------------
MeuseBasis <- create.FEM.basis(p, e, t)

## -----------------------------------------------------------------------------
smoothList <- smooth.FEM.basis(p, LogZinc, MeuseBasis, lambda=1e-4, covariates=Dist)
LogZincfd <- smoothList$fd
df        <- smoothList$df
gcv       <- smoothList$gcv
beta      <- as.numeric(smoothList$beta)
SSE       <- smoothList$SSE

## -----------------------------------------------------------------------------
Xgrid <- seq(-2.2, 2.2, len=21)
Ygrid <- seq(-2.2, 2.2, len=21)
op <- par(no.readonly=TRUE)
plotFEM.fd(LogZincfd, Xgrid, Ygrid, 
           xlab="Latitude", ylab="Longitude", zlab="log10 Zinc Concentration")
par(op)

## -----------------------------------------------------------------------------
smoothList0 <- smooth.FEM.basis(p, LogZinc, MeuseBasis, lambda=1e-4)
SSE0        <- smoothList0$SSE

