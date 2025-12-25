library(testthat)

context("A simulated study to demonstrate the function ``BayesRobustProbit'' when applying hypersphere decomposition to model the correlation structure.")


test_that("A simulation study of ARMA", {

library(BayesRGMM)
rm(list=ls(all=TRUE))
Interactive = interactive()

Fixed.Effs = c(-0.2,-0.8, 1.0, -1.2)
P = length(Fixed.Effs)
q = 1
T = 10
N = 100
num.of.iter = 200
set.seed(1)

ARMA.sim.data = SimulatedDataGenerator(Num.of.Obs = N, Num.of.TimePoints = T, Fixed.Effs = Fixed.Effs, 
	Random.Effs = list(Sigma = 0.5*diag(1), df=3), Cor.in.DesignMat = 0., 
	list(Missing.Mechanism = 2, RegCoefs = c(-1.5, 1.2)), Cor.Str = "ARMA", 
	ARMA.para=list(AR.para = 0.2, MA.para=0.2))

sum(ARMA.sim.data$sim.data$y==1, na.rm=T)/sum(!is.na(ARMA.sim.data$sim.data$y))

ARMA.output = BayesRobustProbit(fixed = as.formula(paste("y~-1+", paste0("x", 1:P, collapse="+"))), 
	                          data=ARMA.sim.data$sim.data, random = ~ 1, Robustness = TRUE, subset = NULL, 
	                          na.action='na.exclude', arma.order = c(1, 1), num.of.iter = num.of.iter, 
	                          Interactive = Interactive)

original = options(digits = 4)

Model.Estimation = BayesRobustProbitSummary(ARMA.output)

cat("\nCoefficients:\n")
print(Model.Estimation$beta.est.CI)

cat("\nAMRA parameters:\n\n")
print(Model.Estimation$arma.est)

cat("\nRandom effect: \n")
print(Model.Estimation$random.cov)

cat("\nModel Information:\n")
print(Model.Estimation$model.info)

options(original)

})

