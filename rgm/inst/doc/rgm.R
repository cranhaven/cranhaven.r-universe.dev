## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,fig.width=6, fig.height=4
)

## ----simulation, eval=FALSE---------------------------------------------------
#  # Running the simulation with specific parameters
#  a <- rgm:::sim.rgm(p=27, B=5, n=146, mcmc_iter = 100, seed=1234)

## ----run-experiment, eval=FALSE-----------------------------------------------
#  
#  # Fitting the model
#  res <- rgm:::rgm(a$data, X=a$X, iter=10000)
#  

## ----load, echo=FALSE, eval=FALSE---------------------------------------------
#  load("SimRes_p87.RData")
#  #res = smaller_res

## ----plot-sample-theta, echo=FALSE, warning=FALSE, eval=FALSE-----------------
#  # Loading the RGM package
#  suppressPackageStartupMessages(library(rgm))
#  
#  suppressPackageStartupMessages(library(ggplot2))
#  
#  suppressPackageStartupMessages(library(pROC))
#  
#  suppressPackageStartupMessages(library(gplots))
#  
#  suppressPackageStartupMessages(library(grid))
#  
#  suppressPackageStartupMessages(library(gridExtra))
#  
#  suppressPackageStartupMessages(library(dendextend))
#  
#  

## ----eval=FALSE---------------------------------------------------------------
#  
#  ps = rgm:::post_processing_rgm(simulated_data = a,results = res)
#  
#  

## ----beta_convergence, warning=FALSE, eval=FALSE------------------------------
#  ps$beta_convergence

## ----rgm_recovery, eval=FALSE-------------------------------------------------
#  
#  ps$rgm_recovery

## ----roc_plot, eval=FALSE-----------------------------------------------------
#  ps$roc_plot

## ----estimation_of_alpha, eval=FALSE------------------------------------------
#  ps$estimation_of_alpha

## ----posterior_distribution, eval=FALSE---------------------------------------
#  ps$posterior_distribution

## ----edge_prob, eval=FALSE----------------------------------------------------
#  ps$edge_prob

