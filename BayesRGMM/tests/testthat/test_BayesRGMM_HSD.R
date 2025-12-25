library(testthat)

context("A simulated study to demonstrate the function ``BayesRobustProbit'' when applying ARMA model for the correlation structure.")


test_that("A simulation study of HSD", {

library(BayesRGMM)
rm(list=ls(all=TRUE))
Interactive = interactive()

Fixed.Effs = c(-0.2, -0.3, 0.8, -0.4) #c(-0.2,-0.8, 1.0, -1.2)
P = length(Fixed.Effs)
q = 1
T = 5
N = 100
num.of.iter = 100
set.seed(1)

HSD.para = c(-0.5,  -0.3)
a = length(HSD.para)
w = array(runif(T*T*a), c(T, T, a))

for(time.diff in 1:a)
	w[, , time.diff] = 1*(as.matrix(dist(1:T, 1:T, method="manhattan")) ==time.diff)

#signif(CorrMat.HSD(w, HSD.para), 4)


HSD.sim.data = SimulatedDataGenerator(Num.of.Obs = N, Num.of.TimePoints = T, Fixed.Effs = Fixed.Effs, 
	Random.Effs = list(Sigma = 0.5*diag(1), df=3), Cor.in.DesignMat = 0., 
	Missing = list(Missing.Mechanism = 2, RegCoefs = c(-1.5, 1.2)), Cor.Str = "HSD", 
	HSD.DesignMat.para = list(HSD.para = HSD.para, DesignMat = w))

sum(HSD.sim.data$sim.data$y==1, na.rm=T)/sum(!is.na(HSD.sim.data$sim.data$y))

sum(is.na(HSD.sim.data$sim.data$y))

hyper.params = list(
        sigma2.beta = 1,
        sigma2.delta = 1,
        v.gamma = 5,
        InvWishart.df = 5,
        InvWishart.Lambda = diag(q) )

HSD.output = BayesRobustProbit(fixed = as.formula(paste("y~-1+", paste0("x", 1:P, collapse="+"))), 
	data=HSD.sim.data$sim.data, random = ~ 1, Robustness = TRUE, HS.model = ~IndTime1+IndTime2, 
	subset = NULL, na.action='na.exclude', hyper.params = hyper.params, num.of.iter = num.of.iter, 
	Interactive = Interactive)

original = options(digits = 4)
Model.Estimation = BayesRobustProbitSummary(HSD.output)

cat("\nCoefficients:\n")
print(Model.Estimation$beta.est.CI)

cat("\nParameters in HSD model:\n")
print(Model.Estimation$delta.est.CI)

cat("\nRandom effect: \n")
print(Model.Estimation$random.cov)

cat("\nModel Information:\n")
print(Model.Estimation$model.info)

cat("\nEstimate of Ri: \n")
print(Model.Estimation$Ri, quote = FALSE)

options(original)

})

