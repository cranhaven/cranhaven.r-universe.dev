# MSRDT (Multi-state Reliability Demonstration Tests)
## Description
This package provides the Bayesian methods to get the optimal test sample size in binomial RDT design and multi-state RDT designs. Numerical estimation of consumer's risk can be obtained through Monte Carlo Simulation. The package provides three categories of methods.
* Binomial RDT (b_.R): This is the conventional test design using failure count data and assuming binomial failure distributions over the testing period.
* MSRDT-Multiple Periods (MP_.R) : This is the MSRDT design with multiple testing periods, which includes two scenarios depending on the test criteria over cumulative periods (Cum) or separate periods (Sep). For failure probabilities over multiple testing periods, the multinomial distribution is assumed.
* MSRDT-Multiple Failure Modes (MFM_.R) : This is the MSRDT design with multiple failure modes. For each failure mode, the binomial failure probability is assumed.
## Reference
This is the R package implementation for the design methods of multi-state reliabiltiy demonstration tests (MSRDTs) with failure count data. The original work is from one of the research projects listed on [Suiyao Chen's Homepage](https://sites.google.com/mail.usf.edu/suiyaochen-professional/publication?authuser=0). 

The paper [Multi-state Reliability Demonstration Tests](https://www.researchgate.net/publication/315955046_Multi-State_Reliability_Demonstration_Tests) has been published in [Quality Engineering](https://www.tandfonline.com/eprint/6aSdzucbThJSnxZMZira/full). To cite this paper, please use 
> Suiyao Chen, Lu Lu & Mingyang Li (2017) Multi-state reliability demonstration tests, Quality Engineering, 29:3, 431-445, DOI: 10.1080/08982112.2017.1314493
## Installation
To install from Github:
```
devtools::install_github("ericchen12377/MSRDT")
#build vignettes if needed
devtools::install_github("ericchen12377/MSRDT", build_vignettes = TRUE, force = TRUE)
library(MSRDT)
#view vignettes
browseVignettes('MSRDT')
```
## Examples
```
######Binomial RDT Design######
###Generate the prior distribution of failure probability
##Beta is conjugate prior to binomial distribution
#Get the non-informative prior Beta(1, 1)
pi <- pi_MCSim_beta(M = 5000, seed = 10, a = 1, b = 1)

#Get the consumer's risk
n = 10
R = 0.8
c = 2
b_CR <- bconsumerrisk(n = n, c = c, pi = pi, R = R)
print(b_CR)
#         [,1]
#>[1,] 0.3330482

##As n increases, CR decreases
#Get the optimal test sample size
thres_CR = 0.05 #CR < 0.05
b_n <- boptimal_n(c = c, pi = pi, R = R, thres_CR = thres_CR)
print(b_n)
#>[1,] 24

```
```
######MSRDT MPCum Design######
###Generate the prior distribution of failure probability
##Dirichlet is conjugate prior to multinomial distribution
#Get the non-informative prior Dirichlet(1, 1, 1)
pi <- pi_MCSim_dirichlet(M = 5000, seed = 10, par = c(1, 1, 1))

#Get the consumer's risk
n = 10
cvec = c(1, 1)
Rvec = c(0.8, 0.7)
MPCum_CR <- MPCum_consumerrisk(n = n, cvec = cvec, pivec = pi, Rvec = Rvec)
print(MPCum_CR)
#>[1] 0.3383538

##As n increases, CR decreases
#Get the optimal test sample size
thres_CR = 0.05 #CR < 0.05
MPCum_n <- MPCum_optimal_n(cvec = cvec, pivec = pi, Rvec = Rvec, thres_CR = thres_CR)
print(MPCum_n)
#>[1] 20
```

## [Updates](NEWS.md)
* Version logs are provided.
