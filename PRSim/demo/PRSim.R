# short demo
data(runoff)

out <- prsim(data=runoff, number_sim=1, marginal="empirical")
out <- prsim(data=runoff, number_sim=1, marginal="kappa", GoFtest = "KS")

### GEV distribution
require("evd")
require("ismev")
rGEV <- function(n, theta)  rgev(n, theta[1], theta[2], theta[3])
pGEV <- function(x, theta)  pgev(x, theta[1], theta[2], theta[3])
GEV_fit <- function( xdat, ...)   gev.fit( xdat, show=FALSE, ...)$mle


### GEV
out <- prsim(data=runoff, number_sim=1, marginal="GEV", GoFtest = "KS", n_par=3)

sim <- out$simulation
# p_val <- out$p_val
# par(mai=c(.9,.9,.1,.1))
plot(sim$timestamp[1:1000], sim$Qobs[1:1000], type="l", 
     xlab="Time [d]", ylab=expression(paste("Discharge [m"^3,"/s]")))
matlines(sim$timestamp[1:1000], sim[1:1000, grep("r", names(sim))],
         lty=1, col="gray")


