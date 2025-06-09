## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
# library(abn)

## -----------------------------------------------------------------------------
# # Load only a subset of the example data for illustration
# mydat <- ex1.dag.data[, c("b1", "p1", "g1", "b2", "p2", "b3", "g2")]
# mydists <- list(b1="binomial",
#                 p1="poisson",
#                 g1="gaussian",
#                 b2="binomial",
#                 p2="poisson",
#                 b3="binomial",
#                 g2="gaussian")
# 
# # Estimate model score for different maximum numbers of parent nodes
# num.vars <- ncol(mydat) # number of variables
# max.pars <- 1:(num.vars-1) # vector of possible maximum number of parent nodes
# 
# npars_scores <- data.frame(max.pars = max.pars, score = rep(NA, length(max.pars))) # data frame to store scores
# 
# # loop over maximum number of parent nodes
# for (i in max.pars) {
#   mycache <- buildScoreCache(data.df = mydat,
#                              data.dists = mydists,
#                              method = "bayes",
#                              max.parents = i)
#   mp.dag <- mostProbable(mycache)
#   myfit <- fitAbn(mp.dag)
# 
#   npars_scores[i, "score"] <- myfit$mlik # store score
# }
# 
# # Plot the scores for different maximum numbers of parent nodes
# library(ggplot2)
# ggplot(npars_scores, aes(x = max.pars, y = score)) +
#   geom_point() +
#   geom_line() +
#   labs(x = "Maximum number of parent nodes", y = "Model score") +
#   # set x-axis labels to integers
#   scale_x_continuous(breaks = seq(0, num.vars, 1))

## -----------------------------------------------------------------------------
# # Load the example data
# mydat <- ex1.dag.data
# mydists <- list(b1="binomial",
#                 p1="poisson",
#                 g1="gaussian",
#                 b2="binomial",
#                 p2="poisson",
#                 b3="binomial",
#                 g2="gaussian",
#                 b4="binomial",
#                 b5="binomial",
#                 g3="gaussian")
# 
# # Define edges and their directions as fixed or forbidden
# dag.banned <- matrix(0, nrow = 10, ncol = 10, dimnames = list(names(mydat), names(mydat)))
# 
# # Define edges and their directions as forbidden
# dag.banned["b1", "b2"] <- 1
# dag.banned["b1", "b3"] <- 1
# dag.banned["b1", "b4"] <- 1
# 
# # Display the matrix
# dag.banned
# 
# # Plot the forbidden edges
# plotAbn(dag = dag.banned, data.dists = mydists)

