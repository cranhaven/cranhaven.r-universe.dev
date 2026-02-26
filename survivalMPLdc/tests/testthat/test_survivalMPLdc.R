context("Testing the regression cofficients estimation")


library(survivalMPLdc)
library(testthat)

test_that("Testing the MPL estimated regression coefficients versus the true values", {

##-- Copula types
copula3 <- 'frank'

##-- Marginal distribution for T, C, and A
a <- 2
lambda <- 2
cons7 <- 0.2
cons9 <- 10
tau <- 0.8
betas <- c(-0.5, 0.1)
phis <- c(0.3, 0.2)
distr.ev <- 'weibull'
distr.ce <- 'exponential'

##-- Real marginal baseline hazard for T
xi<-seq(0, 5.4, 0.01)
ht0b <- a * ( xi^ (a - 1)) / (lambda ^ a)

##-- Sample size 200
n <- 200
set.seed(0)
##-- One sample Monte Carlo dataset
cova <- cbind(rbinom(n, 1, 0.5), runif(n, min=-10, max=10))
set.seed(0)
surv <- surv_data_dc(n, a, cova, lambda, betas, phis, cons7, cons9, tau, copula3,
                      distr.ev, distr.ce)
n <- nrow(cova)
p <- ncol(cova)

##-- event and dependent censoring proportions
colSums(surv)[c(2,3)]/n
X <- surv[,1] # Observed time
del<-surv[,2] # failure status
eta<-surv[,3] # dependent censoring status

##-- Inputs
control<-coxph_mpl_dc.control(ordSp=1, binCount=20, tau=0.8, copula=copula3,
                               pent='mat1', smpart="REML", penc='mat1', smparc="REML",
                               maxit2=50, maxit=5000,
                               cat.smpar='No')

##-- Perform MPL estimation
coxMPLests <- coxph_mpl_dc(surv, cova, control = control, )


##-- Obtain the MPL coefficient estimates
mpl_beta_phi_zp <- rbind( coef(object = coxMPLests, parameter = "beta",),
                          coef(object = coxMPLests, parameter = "phi",)
)


##-- Plot the true and estimated baseline hazards (95% confidence interval) for T
plot(x = coxMPLests, parameter = "theta", funtype="hazard",
     xout = xi, se = TRUE,
     cols=c("blue", "red"), ltys=c(4, 2), type="l", lwd=1, cex=1, cex.axis=1, cex.lab=1,
     xlab="Time", ylab="Hazard",
     xlim=c(0, 5.4), ylim=c(0, 3)
)
lines(x = xi, y = ht0b,
      col="green",
      lty=1, lwd=1,
      cex.axis=1, cex.lab=1,
      xlim=c(0, 5.4), ylim=c(0, 3)
      )
title("MPL Hazard", cex.main=1)
legend( 'topleft',legend = c( "MPL", "95% Confidence Interval", "True"),
        col = c("blue", "red", "green"),
        lty = c(4, 2, 1),
        cex = 1)

expect_equal(
  round( mpl_beta_phi_zp[,1], 7 ),
  c(-0.5367184, 0.1258855, 0.2217520, 0.1782889)
  )

expect_equal(
  round( mpl_beta_phi_zp[,2], 8 ),
  c(0.16322076, 0.01784505, 0.21279377, 0.02124492)
)

})


