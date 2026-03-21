## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BRDT)

## -----------------------------------------------------------------------------
###Generate the prior distribution of failure probability
##Beta is conjugate prior to binomial distribution
#Get the non-informative prior Beta(1, 1)
pi <- pi_MCSim_beta(M = 5000, seed = 10, a = 1, b = 1)

#Get the consumer's risk
n = 10
R <- 0.8
c <- 2
b_CR <- bconsumerrisk(n = n, c = c, pi = pi, R = R)
print(b_CR)

##As n increases, CR decreases
#Get the optimal test sample size
thres_CR <- 0.05 #CR < 0.05
b_n <- boptimal_n(c = c, pi = pi, R = R, thres_CR = thres_CR)
print(b_n)

## -----------------------------------------------------------------------------
#RDT cost
Cf <- 0
Cv <- 10
n_optimal <- 24
RDTcost <- bcost_RDT(Cf = Cf, Cv = Cv, n = n_optimal)
print(RDTcost)

#RG Cost
G <- 10000 #G can be obtained from specific reliability growth models
RGcost <- bcost_RG(G = G)
print(RGcost)

#WS Cost
Cw <- 10
N <- 1
n_optimal <- 24 

WScost <- bcost_WS(Cw = 10, N = 1, n = n_optimal, c = 1, pi = pi);
print(WScost[1]) #expected failure probability
print(WScost[2]) #expected warranty services cost

#Expected overall cost
Overall_cost <- bcost_expected(Cf = Cf, Cv = Cv, n = n_optimal, G = G, Cw = Cw, N = N, c = c, pi = pi)
print(Overall_cost)

## -----------------------------------------------------------------------------
#Vectors to get combinations of different R and c
Rvec <- seq(0.8, 0.85, 0.01)
cvec <- seq(0, 2, 1)


Plan_optimal_cost <- boptimal_cost(Cf = 10, Cv = 10, G = 100, Cw = 10, N = 100, Rvec = Rvec, cvec = cvec, pi = pi, thres_CR = 0.5)
print(Plan_optimal_cost)

## -----------------------------------------------------------------------------
nvec <- seq(0, 10, 1)
Rvec <- seq(0.8, 0.85, 0.01)
cvec <- seq(0, 2, 1)
pi <- pi_MCSim_beta(M = 5000, seed = 10, a = 1, b = 1)

#Get data from all combinations of n, c, R
data_all <- bdata_generator(Cf = 10, Cv = 10, nvec = nvec, G = 10000, Cw = 10, N = 100, Rvec = Rvec, cvec = cvec, pi = pi, par = all(), option = c("all"), thres_CR = 0.05)
head(data_all)

#Get data with optimal test sample size and minimum overall costs from all combinations of c, R
data_optimal <- bdata_generator(Cf = 10, Cv = 10, nvec = nvec, G = 10000, Cw = 10, N = 100, Rvec = Rvec, cvec = cvec, pi = pi, par = all(), option = c("optimal"), thres_CR = 0.05)
head(data_optimal)

