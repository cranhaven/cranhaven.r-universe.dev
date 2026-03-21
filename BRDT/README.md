# BRDT (Binomial Reliability Demonstration Tests)

## Description
This package provides the Bayesian methods to get the optimal test sample size in binomial reliability demonstration tests design.
* Conventional Binomial RDT: the package implements the conventional test design using failure count data and assuming binomial failure distributions over the testing period. Binomial RDT can also be referred to as binomial test or attribute test.
* Binomial RDT with Acceptance Uncertainty: the package implements the design methods to quantify the BRDT acceptance decision uncertainty and evaluate the cost impacts on related reliability assurance activities. Optimal test plans with minimum overall costs can be obtained.
* Data generator: the package implements a data generator to generate the test plans dataset with specific design settings.

## Reference
This is the R package implementation for the design methods of binomial reliabiltiy demonstration tests (BRDTs) with failure count data. The acceptanace decision uncertainty of BRDT has been quantified and the impacts of the uncertainty on related reliability assurance activities such as reliability growth and warranty services are evaluated. The original work is from one of the research projects listed on [Suiyao Chen's Homepage](https://sites.google.com/mail.usf.edu/suiyaochen-professional/publication?authuser=0). 

The paper [Optimal Binomial Reliability Demonstration Tests Design under Acceptance Decision Uncertainty](https://www.tandfonline.com/doi/full/10.1080/08982112.2020.1757703) has been published in [Quality Engineering](https://www.tandfonline.com/doi/full/10.1080/08982112.2020.1757703). To cite this paper, please use 
> Suiyao Chen, Lu Lu, Qiong Zhang & Mingyang Li (2020) Optimal binomial reliability demonstration tests design under acceptance decision uncertainty, Quality Engineering, DOI: 10.1080/08982112.2020.1757703

## Installation
To install from Github:
```
devtools::install_github("ericchen12377/BRDT")
#build vignettes if needed
devtools::install_github("ericchen12377/BRDT", build_vignettes = TRUE, force = TRUE)
library(BRDT)
#view vignettes
browseVignettes('BRDT')
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

######Getting All Related Costs######
#RDT cost
Cf <- 0
Cv <- 10
n_optimal <- 24
RDTcost <- bcost_RDT(Cf = Cf, Cv = Cv, n = n_optimal)
print(RDTcost)
#> [1] 240

#RG Cost
G <- 10000 #G can be obtained from specific reliability growth models
RGcost <- bcost_RG(G = G)
print(RGcost)
#> [1] 10000

#WS Cost
Cw <- 10
N <- 1
n_optimal <- 24 

WScost <- bcost_WS(Cw = 10, N = 1, n = n_optimal, c = 1, pi = pi);
print(WScost[1]) #expected failure probability
#> [1] 0.05852312
print(WScost[2]) #expected warranty services cost
#> [1] 0.5852312

#Expected overall cost
Overall_cost <- bcost_expected(Cf = Cf, Cv = Cv, n = n_optimal, G = G, Cw = Cw, N = N, c = c, pi = pi)
print(Overall_cost)
#> [1] 9006.048

######Optimal Test Plans with Minimum Overall Costs######
#Vectors to get combinations of different R and c
Rvec <- seq(0.8, 0.85, 0.01)
cvec <- seq(0, 2, 1)


Plan_optimal_cost <- boptimal_cost(Cf = 10, Cv = 10, G = 100, Cw = 10, N = 100, Rvec = Rvec, cvec = cvec, pi = pi, thres_CR = 0.5)
print(Plan_optimal_cost)
#>   n    R c        CR        AP RDT Cost RG Cost RG Cost Expected  WS Cost
#> 6 4 0.85 0 0.4304362 0.2029711       50     100         79.70289 163.8177
#>   WS Failure Probability WS Cost Expected Overall Cost
#> 6              0.1638177         33.25026     162.9532

######Getting All Related Costs######
nvec <- seq(0, 10, 1)
Rvec <- seq(0.8, 0.85, 0.01)
cvec <- seq(0, 2, 1)
pi <- pi_MCSim_beta(M = 5000, seed = 10, a = 1, b = 1)

#Get data from all combinations of n, c, R
data_all <- bdata_generator(Cf = 10, Cv = 10, nvec = nvec, G = 10000, Cw = 10, N = 100, Rvec = Rvec, cvec = cvec, pi = pi, par = all(), option = c("all"), thres_CR = 0.05)
head(data_all)
#>   n   R c        CR        AP RDT Cost RG Cost RG Cost Expected  WS Cost
#> 1 0 0.8 0 0.7948000 1.0000000       10   10000            0.000 500.3654
#> 2 1 0.8 0 0.6300625 0.4996346       20   10000         5003.654 329.4260
#> 3 2 0.8 0 0.5011545 0.3350420       30   10000         6649.580 246.0267
#> 4 3 0.8 0 0.3994328 0.2526127       40   10000         7473.873 196.5127
#> 5 4 0.8 0 0.3189480 0.2029711       50   10000         7970.289 163.8177
#> 6 5 0.8 0 0.2551039 0.1697208       60   10000         8302.792 140.6811
#>   WS Failure Probability WS Cost Expected Overall Cost
#> 1              0.5003654        500.36539     510.3654
#> 2              0.3294260        164.59264    5188.2465
#> 3              0.2460267         82.42927    6762.0096
#> 4              0.1965127         49.64160    7563.5146
#> 5              0.1638177         33.25026    8053.5392
#> 6              0.1406811         23.87651    8386.6681

#Get data with optimal test sample size and minimum overall costs from all combinations of c, R
data_optimal <- bdata_generator(Cf = 10, Cv = 10, nvec = nvec, G = 10000, Cw = 10, N = 100, Rvec = Rvec, cvec = cvec, pi = pi, par = all(), option = c("optimal"), thres_CR = 0.05)
head(data_optimal)
#>    n    R c         CR         AP RDT Cost RG Cost RG Cost Expected  WS Cost
#> 1 13 0.80 0 0.04379640 0.07316579      140   10000         9268.342 67.02070
#> 2 13 0.81 0 0.04952572 0.07316579      140   10000         9268.342 67.02070
#> 3 14 0.82 0 0.04779152 0.06826217      150   10000         9317.378 62.88279
#> 4 15 0.83 0 0.04739077 0.06396965      160   10000         9360.303 59.20539
#> 5 16 0.84 0 0.04933936 0.06018231      170   10000         9398.177 55.91351
#> 6 18 0.85 0 0.04545871 0.05380893      190   10000         9461.911 50.26193
#>   WS Failure Probability WS Cost Expected Overall Cost
#> 1             0.06702070         4.903623     9413.246
#> 2             0.06702070         4.903623     9413.246
#> 3             0.06288279         4.292516     9471.671
#> 4             0.05920539         3.787348     9524.091
#> 5             0.05591351         3.365004     9571.542
#> 6             0.05026193         2.704541     9654.615
```

## [Updates](NEWS.md)
* Version logs are provided.
