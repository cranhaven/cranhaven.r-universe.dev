## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- error=TRUE--------------------------------------------------------------
library(GGMncv)

# p > n
set.seed(2)
main <- gen_net(p = 50, 
                edge_prob = 0.05)

set.seed(2)
y <- MASS::mvrnorm(n = 49, 
                   mu = rep(0, 50), 
                   Sigma = main$cors)

fit <- ggmncv(cor(y), n = nrow(y))

## ---- message=FALSE, warning=FALSE--------------------------------------------
fit <- ggmncv(cor(y), n = nrow(y), 
              penalty = "atan",
              progress = FALSE,
              initial = ledoit_wolf, Y = y)

## ---- message=FALSE, warning=FALSE--------------------------------------------
plot(get_graph(fit), 
     node_size = 5)

## -----------------------------------------------------------------------------
initial_ggmncv <- function(y, ...){
  Rinv <- corpcor::invcor.shrink(y, verbose = FALSE)
  return(Rinv)
}

fit2 <- ggmncv(cor(y), n = nrow(y), 
               penalty = "atan",
               progress = FALSE,
               initial = initial_ggmncv, 
               y = y)

plot(get_graph(fit2), 
     node_size = 5)

## -----------------------------------------------------------------------------
# ledoit and wolf
score_binary(estimate = fit$adj, 
             true = main$adj, 
             model_name = "lw")

# Shaffer and strimmer
score_binary(estimate = fit2$adj, 
             true = main$adj,  
             model_name = "ss")


## ---- error=TRUE--------------------------------------------------------------
# p -> n
main <- gen_net(p = 50, 
                edge_prob = 0.05)

y <- MASS::mvrnorm(n = 60, 
                   mu = rep(0, 50), 
                   Sigma = main$cors)

fit <- ggmncv(cor(y), n = nrow(y), 
              penalty = "atan",
              progress = FALSE)

score_binary(estimate = fit$adj, 
             true = main$adj)

## -----------------------------------------------------------------------------
fit <- ggmncv(cor(y), n = nrow(y), 
              progress = FALSE,
              penalty = "atan",
              initial = ledoit_wolf, Y = y)

score_binary(estimate = fit$adj, 
             true = main$adj)

## -----------------------------------------------------------------------------
fit_l1 <- ggmncv(cor(y), n = nrow(y), 
              progress = FALSE, 
              penalty = "lasso")

score_binary(estimate = fit_l1$adj, 
             true = main$adj)

## -----------------------------------------------------------------------------
# atan
kl_mvn(true = solve(main$cors), estimate = fit$Theta)

# lasso
kl_mvn(true = solve(main$cors), estimate = fit_l1$Theta)

