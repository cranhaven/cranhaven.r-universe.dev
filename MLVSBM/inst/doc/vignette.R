## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(MLVSBM)

## ----simulate-----------------------------------------------------------------
set.seed(123)
my_mlvsbm <- MLVSBM::mlvsbm_simulate_network(
  n = list(I = 60, O = 40), # Number of nodes for the lower level and the upper level
  Q = list(I = 3, O = 3), # Number of blocks for the lower level and the upper level
  pi = c(.5, .3, .2), # Block proportion for the upper level, must sum to one
  gamma = matrix(c(.8, .2, .05,  # Block proportion for the lower level,
                   .1, .7, .05,
                   .1, .1, .9), # each column must sum to one
                 nrow = 3, ncol = 3, byrow = TRUE),  
  alpha = list(I = matrix(c(.1, .1, .3, 
                            .1, .2, .5,
                            .1, .5, .5), 
                          nrow = 3, ncol = 3, byrow = TRUE), # Connection matrix
               O = matrix(c(.4, .1, .1, 
                            .1, .5, .1,
                            .1, .1, .6), 
                          nrow = 3, ncol = 3, byrow = TRUE)),# between blocks
  directed = list(I = TRUE, O = FALSE), # Are the upper and lower level directed or not ?
  affiliation = "preferential", # How the affiliation matrix is generated
  no_empty_org = FALSE) # May the affiliation matrix have column suming to 0

## ----create-------------------------------------------------------------------
lower_level <- my_mlvsbm$adjacency_matrix$I # matrix of size nI x nI
upper_level <- my_mlvsbm$adjacency_matrix$O # matrix of size nO x nO
affiliation <- my_mlvsbm$affiliation_matrix # matrix of size nI x nO
my_mlvsbm2 <- MLVSBM::mlvsbm_create_network(X = list(I = lower_level, O = upper_level),
                                            A = affiliation)

## ----infer--------------------------------------------------------------------
fit <- MLVSBM::mlvsbm_estimate_network(my_mlvsbm, nb_cores = 1L)

## ----generic, fig.width=7, fig.height=7---------------------------------------
print(fit)
plot(fit, type = "matrix", order = "affiliation")
plot(fit, type = "matrix", order = "degree")
coef(fit)
pred <- predict(fit)

## ----output_MLVSBM------------------------------------------------------------
my_mlvsbm$ICL # A data frame of the inferred models 
my_fit <- my_mlvsbm$fittedmodels[[which.max(my_mlvsbm$ICL$ICL)]] # The fitted model with index  the highest ICL
my_mlvsbm$ICL_sbm # The ICL of the SBM
my_sbm_lower <- my_mlvsbm$fittedmodels_sbm$lower[[3]] # A fitted SBM for the lower level with 3 blocks
my_sbm_upper <- my_mlvsbm$fittedmodels_sbm$upper[[2]] # A fitted SBM for the upper level with 2 blocks

## ----output_Fit---------------------------------------------------------------
fit$parameters # The connectivity and membership parameters of the model
fit$Z # The block membership of each nodes
fit$vbound # A vector of the varational bound of the VEM algorithm
tau <- fit$membership # The variational parameters of the model
pred <- fit$X_hat # The links predictions for each level

