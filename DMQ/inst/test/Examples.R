library(DMQ)
library(parallel)

# Load Microsoft Corporation logarithmic percentage returns from December 8, 
# 2010 to November 15, 2018 for a total of T = 2000 observation
data("MSFT")

# parallel computation
iG = 6
cluster = makeCluster(iG)

##############################################################
######################## Estimate DMQ ########################
##############################################################

# Percentiles
vTau = seq(0.01, 0.99, 0.01)

# Reference quantile
iTau_star = 50

# Fix the median to a constant
FixReference = TRUE

set.seed(123)

# Estimate DMQ using DEoptim
Fit_DEoptim = EstimateDMQ(vY = vY,
                  vTau = vTau,
                  iTau_star = iTau_star,
                  FixReference = FixReference,
                  fn.optimizer = fn.DEoptim,
                  cluster = cluster)

stopCluster(cluster)

# Estimate DMQ using optim
Fit_optim = EstimateDMQ(vY = vY,
                          vTau = vTau,
                          iTau_star = iTau_star,
                          FixReference = FixReference,
                          fn.optimizer = fn.optim)

# Estimate DMQ using solnp
Fit_solnp = EstimateDMQ(vY = vY,
                  vTau = vTau,
                  iTau_star = iTau_star,
                  FixReference = FixReference,
                  fn.optimizer = fn.solnp)


Fit_solnp$optimizer$value
Fit_optim$optimizer$value
Fit_DEoptim$optimizer$value

Fit = Fit_DEoptim

# Extract estimated quantiles
mQ = Fit$lFilter$mQ

# plot estimated quantiles at levels 5%, 10%, ...
plot.ts(t(mQ)[, seq(5, 95, 5)], plot.type = "single", las = 1, 
        main = "Estimated quantiles at 5%, 10%, ..., 95%", ylab = "q_t")


# Compute estimated moments
Moments = MomentsDMQ(Fit)

# returns with estimated mean
plot.ts(vY, las = 1, ylab = "Y_t", 
        main = "Returns with estimated conditional means")
lines(Moments$mCenterdMoments[, 1], col = "red")

# absolute returns with estimated variance
plot.ts(abs(vY), las = 1, ylab = "Y_t", 
        main = "Absolute returns with estimated conditional variance")
lines(Moments$mCenterdMoments[, 2], col = "red")

##############################################################
######################## Forecast DMQ ########################
##############################################################

# Forecast horizon
iH = 200

Prediction = ForecastDMQ(Fit, iH) 

# plot estimated and predicted quantiles at levels 5%, 10%, ...
plot.ts(rbind(t(mQ), Prediction[-1,])[, seq(5, 95, 5)], plot.type = "single", las = 1, 
        main = "Estimated and predicted quantiles at 5%, 10%, ..., 95%", ylab = "q_t")

abline(v = 2000, lty = 2, col = "red" )


##############################################################
######################## Simulate DMQ ########################
##############################################################

set.seed(123)

# Parameters
vPn = c("phi" = 0.986, "gamma" = 0.0458, "alpha" = 0.0, "beta" = 0.0)

# Unconditional quantiles
vQ_0 = qnorm(vTau)

# Percentiles
vTau = seq(0.01, 0.99, 0.01)

# Reference quantile
iTau_star = 50

# Simulate DM
Sim = SimulateDMQ(500, vQ_0, vTau, iTau_star, vPn)

# plot estimated quantiles at levels 5%, 10%, ...
plot.ts(Sim$mQ[, seq(5, 95, 5)], plot.type = "single", las = 1, 
        main = "Simulated quantiles at 5%, 10%, ..., 95%", ylab = "q_t")

# plot simulated series 
plot.ts(Sim$vY, las = 1, 
        main = "Simulated series", ylab = "Y_t")

