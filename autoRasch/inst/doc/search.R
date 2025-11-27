## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(autoRasch)
library(doParallel)

## -----------------------------------------------------------------------------
grMap <- matrix(c(rep(0,50),rep(1,50)),ncol = 1, dimnames = list(c(1:100),c("cov")))
ipoqlldif_score <- autoRasch::compute_score(shortDIF, incl_set = c(1:4), type = "ipoqlldif", groups_map = grMap)
summary(ipoqlldif_score)

## -----------------------------------------------------------------------------
ipoqll_scores <- compute_scores(shortDIF, incl_sets = rbind(c(1:3),c(2:4)), type = "ipoqll", cores = 2)
ipoqll_scores[,1:7]

## -----------------------------------------------------------------------------
setting <- autoRaschOptions()
setting$isHessian <- FALSE
stepwise_res <- stepwise_search(shortDIF, incl_set = c(1:4), cores = 2,
                               groups_map = grMap, method = "fast",
                               criterion = "ipoqlldif", isTraced = TRUE)

## -----------------------------------------------------------------------------
plot_search(stepwise_res, type="l")

