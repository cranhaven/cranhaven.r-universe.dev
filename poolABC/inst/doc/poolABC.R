## ----options, include = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  out.width = '80%', dpi = 300,
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(poolABC)

## ----example, tidy=TRUE-------------------------------------------------------
data.frame(chr = c("NC297", "NC297"), pos =c(3530, 5450), rc = c("A", "T"), allele_count = 2, allele_states = c("A/G", "T/A"), deletion_sum = 0, snp_type = "pop", major_alleles = c("AA", "TT"), minor_alleles = c("GG", "AN"), maa_1 = c("54/55", "51/54"), maa_2 = c("76/78",  "96/96"), mia_1 = c("1/55", "3/54"), mia_2 = c("2/78", "0/96"))

## ----import data, eval=FALSE, include=TRUE------------------------------------
#  # load multiple files and organize information by contig
#  files <- importContigs(path = "/home/myawesomedata", pops = c(1, 4, 7, 10))

## ----select blocks, eval=FALSE, include=TRUE, tidy=TRUE-----------------------
#  # randomly select blocks of a given size from several contigs
#  blocks <- pickWindows(freqs, positions, range, rMajor, rMinor, coverage, window = 1000, nLoci = 100)

## ----observed sumstats, eval=FALSE, include=TRUE------------------------------
#  # compute a set of observed summary statitstics
#  obs <- statsContig(randomWindows = blocks, nPops = 4, stat.names = stat.names)

## ----two populations, tidy=TRUE-----------------------------------------------
# set the mean and variance of the coverage
sMean <- c(84.34, 66.76); sVars <- c(1437.22, 912.43) 
# create a list containing the information of the pool sizes by population
size <- rep(list(rep(10, 10)), 2)
# run simulation for a two-populations model
sims <- poolSim(model="2pops", nDip=200, nPops=2, nLoci=100, nSites=2000, mutrate=2e-8, size=size, mean=sMean, variance=sVars, minimum=15, maximum=180, min.minor=1, Nref=c(25000, 25000), ratio=c(0.1, 3), pool=c(5, 180), seq=c(0.0001, 0.001), split=c(0, 3), CW=c(1e-13, 1e-3), WC=c(1e-13, 1e-3), bT=c(0, 0.5))

## ----four populations, tidy=TRUE----------------------------------------------
# set the mean and variance of the coverage
sMean <- c(84.34, 66.76, 65.69, 68.83); sVars <- c(1437.22, 912.43, 848.02, 1028.23)
# create a list containing the information of the pool sizes by population
size <- rep(list(rep(5, 20)), 4)
# run simulation for a four-populations model
sims <- poolSim(model="Single", nDip=400, nPops=4, nLoci=100, nSites=2000, mutrate=2e-8, size=size, mean=sMean, variance=sVars, minimum=15, maximum=180, min.minor=2, Nref=c(25000, 25000), ratio=c(0.1, 3), pool=c(5, 180), seq=c(0.0001, 0.001), split=c(0, 3), CW=c(1e-13, 1e-3), WC=c(1e-13, 1e-3), CC=c(1e-13, 1e-3), WW=c(1e-13, 1e-3), ANC=c(1e-13, 1e-3), bT=c(0, 0.5), bCW=c(0, 0.5), bWC=c(0, 0.5))

## ----multiple simulations, tidy=TRUE------------------------------------------
# set the mean and variance of the coverage
sMean <- c(84.34, 66.76); sVars <- c(1437.22, 912.43) 
# create a list containing the information of the pool sizes by population
size <- rep(list(rep(5, 20)), 2)
# define how many simulations to run 
nSims <- 10
# run one batch of simulations
sims <- t(replicate(n = nSims, unlist(poolSim(model="2pops", nDip=200, nPops=2, nLoci=100, nSites=1000, mutrate=2e-8, size=size, mean=sMean, variance=sVars, minimum=20, maximum=185, min.minor=2, Nref=c(25000, 25000), ratio=c(0.1, 3), pool=c(5, 180), seq=c(0.0001, 0.001), split=c(0, 3), CW=c(1e-13, 1e-3), WC=c(1e-13, 1e-3), bT=c(0, 0.5)))))

## ----load data----------------------------------------------------------------
# load the data included in the package
data("sumstats"); data("params"); data("limits")

## ----ABC, eval=FALSE, include=TRUE, tidy=TRUE---------------------------------
#  # parameter estimation with ÃBC function
#  myabc <- ABC(nPops = 2, ntrials = 10, freqs = freqs, positions = positions, range = range, rMajor = rMajor, rMinor = rMinor, coverage = coverage, window = 1000, nLoci = 100, limits = limits, params = params, sumstats = sumstats, tol = 0.01, method = "regression")

## ----global sumstat, eval=FALSE, include=TRUE, tidy=TRUE----------------------
#  # load multiple files and organize information by contig
#  blocks <- pickWindows(freqs = freqs, positions = positions, range = range, rMajor = rMajor, rMinor = rMinor, coverage = coverage, window = 1000, nLoci = 800)
#  # compute a set of summary statistics from the observed data
#  global <- statsContig(randomWindows = blocks, nPops = 2, stat.names = stat.names)

## ----merge posteriors, eval=FALSE, message=FALSE, include=TRUE, tidy=TRUE-----
#  # merge posterior distributions
#  myabc <- mergepost(target = myabc$target, global = global, post = myabc$adjusted, wtreg = myabc$weights)

## ----plot param, eval=FALSE, message=FALSE, include=TRUE----------------------
#  # plot the density estimation of a parameter
#  plot_weighted(prior = params, merged_posterior = myabc, limits = limits, index = 2)

## ----model, message=FALSE, tidy=TRUE------------------------------------------
# create a vecto of model indices
index <- c(rep("model1", nrow(sumstats)/2), rep("model2", nrow(sumstats)/2))
# select a random simulation to act as target
target <- sumstats[10, ]
# perform model selection with ABC
mysel <- modelSelect(target = target, index = index, sumstats = sumstats, tol = 0.1, method = "regression")
# display the structure of the mysel object
str(mysel)

## ----summary model------------------------------------------------------------
# check results of model selection
msel <- summary_modelSelect(object = mysel)

## ----Bayes factors------------------------------------------------------------
# print results of model selection 
msel

## ----compute error, warning=FALSE, tidy=TRUE----------------------------------
# perform an Approximate Bayesian Computation simulation study
mycv <- simulationABC(params = params, sumstats = sumstats, limits = limits, nval = 100, tol = 0.01, method = "regression")
# display the structure of the mycv object
str(mycv, max.level = 2)

## ----plot error, fig.height=5, fig.width=6, message=FALSE---------------------
# plot the prediction errors 
plot_errorABC(x = mycv, method = "reg", statistic = "median", index = 1)

## ----simulate model selection-------------------------------------------------
# perform a leave-one-out cross validation for model selection
modelSim <- sim_modelSel(index = index, sumstats = sumstats, nval = 100, tol = 0.1)
# display the structure of the modelSim object
str(modelSim)

## ----model selection error----------------------------------------------------
# compute the mean misclassification probabilities  
mselError <- error_modelSel(object = modelSim)

## ----barplot, fig.height=3, fig.width=5, message=FALSE------------------------
# display a barplot of model misclassification
plot_msel(object = mselError)

