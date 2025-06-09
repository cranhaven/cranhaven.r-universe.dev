## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
# library(abn)

## -----------------------------------------------------------------------------
# mydat <- ex1.dag.data
# str(mydat)

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
# # max.par <- list("b1"=1,"p1"=2,"g1"=3,"b2"=4,"p2"=1,"b3"=2,"g2"=3,"b4"=4,"b5"=1,"g3"=2) # set different max parents for each node
# max.par <- 4 # set the same max parents for all nodes

## -----------------------------------------------------------------------------
# mycache <- buildScoreCache(data.df = mydat,
#                            data.dists = mydists,
#                            method = "bayes", # the default method is "bayes"
#                            max.parents = max.par)

## -----------------------------------------------------------------------------
# mp.dag <- mostProbable(score.cache = mycache)

## -----------------------------------------------------------------------------
# plot(mp.dag)

## -----------------------------------------------------------------------------
# myfit <- fitAbn(object = mp.dag)

## -----------------------------------------------------------------------------
# summary(myfit)
# plot(myfit)

## -----------------------------------------------------------------------------
# simdat <- simulateAbn(object = myfit,
#                       n.iter = 10000L)
# summary(simdat)

