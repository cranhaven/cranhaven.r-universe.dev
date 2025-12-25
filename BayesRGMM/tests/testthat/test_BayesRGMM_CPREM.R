library(testthat)

context("A simulated study to demonstrate the function ``BayesCumulativeProbitHSD'' to deal with the ordinal data using cummulative probit model.")


test_that("A simulation study of CumulativeProbit", {

library(BayesRGMM)
rm(list=ls(all=TRUE))
set.seed(1)
Interactive = interactive()

Fixed.Effs = c(-0.1, 0.1, -0.1) #c(-0.8, -0.3, 1.8, -0.4) #c(-0.2,-0.8, 1.0, -1.2)
P = length(Fixed.Effs) 
q = 1 #number of random effects
T = 7 #time points
N = 100 #number of subjects
Num.of.Cats = 3 #in KBLEE simulation studies, please fix it. 
num.of.iter = 1000 #number of iterations

HSD.para = c(-0.9, -0.6) #the parameters in HSD model
a = length(HSD.para)
w = array(runif(T*T*a), c(T, T, a)) #design matrix in HSD model
 
for(time.diff in 1:a)
w[, , time.diff] = 1*(as.matrix(dist(1:T, 1:T, method="manhattan")) ==time.diff)


x = array(0, c(T, P, N))
for(i in 1:N){
    #x[,, i] = t(rmvnorm(P, rep(0, T), AR1.cor(T, Cor.in.DesignMat)))
    x[, 1, i] = 1:T
    x[, 2, i] = rbinom(1, 1, 0.5)
    x[, 3, i] = x[, 1, i]*x[, 2, i]
}

DesignMat = x

#Generate a data with HSD model


#MAR
CPREM.sim.data = SimulatedDataGenerator.CumulativeProbit(Num.of.Obs = N, 
    Num.of.TimePoints = T, Num.of.Cats = Num.of.Cats, Fixed.Effs = Fixed.Effs, 
    Random.Effs = list(Sigma = 0.5*diag(1), df=3), DesignMat = DesignMat, 
    Missing = list(Missing.Mechanism = 2, MissingRegCoefs=c(-0.7, -0.2, -0.1)), 
    HSD.DesignMat.para = list(HSD.para = HSD.para, DesignMat = w))


print(table(CPREM.sim.data$sim.data$y))
print(CPREM.sim.data$classes)

BCP.output = BayesRobustProbit(fixed = as.formula(paste("y~", paste0("x", 1:P, collapse="+"))), 
    data=CPREM.sim.data$sim.data, random = ~ 1, Robustness = TRUE, 
    subset = NULL, na.action='na.exclude', HS.model = ~IndTime1+IndTime2, 
    hyper.params=NULL, num.of.iter=num.of.iter, Interactive = Interactive)

BCP.Est.output = BayesRobustProbitSummary(BCP.output)

})

