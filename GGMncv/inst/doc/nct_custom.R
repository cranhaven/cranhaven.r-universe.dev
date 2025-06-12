## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(GGMncv)

set.seed(1)

main <- gen_net(p = 10)

y1 <- MASS::mvrnorm(n = 500, 
                    mu = rep(0, 10), 
                    Sigma = main$cors)

y2 <- MASS::mvrnorm(n = 500, 
                    mu = rep(0, 10), 
                    Sigma = main$cors)

Correlation <- function(x, y){
  cor(x[upper.tri(x)], y[upper.tri(x)])
}

compare_ggms <- nct(y1, y2, 
                    FUN = Correlation, 
                    progress = FALSE)

compare_ggms

## -----------------------------------------------------------------------------
hist(compare_ggms$Correlation_perm, 
     main = "null dist: correlation")
abline(v = compare_ggms$Correlation_obs)

## -----------------------------------------------------------------------------
1 - mean(compare_ggms$Correlation_perm > compare_ggms$Correlation_obs)

## -----------------------------------------------------------------------------
# define function
r2 <- function(x, y){
  diag(x) <- 1
  diag(y) <- 1
  
  # network 1
  inv1 <- solve(corpcor::pcor2cor(x))
  beta1 <- -(inv1[1,-1] / inv1[1,1])
  r21 <- cor(y1[,1], y1[,-1] %*% beta1)^2
  
  # network 2
  inv2 <- solve(corpcor::pcor2cor(y))
  beta2 <- -(inv2[1,-1] / inv2[1,1])
  r22 <- cor(y2[,1], y2[,-1] %*% beta2)^2
  
  return(as.numeric(r21 - r22))
}

## -----------------------------------------------------------------------------
compare_ggms <- nct(y1, y2, 
                    progress = FALSE,
                    FUN = r2)

hist(compare_ggms$r2_perm,  
     main = "null dist: R2 Difference")
abline(v = compare_ggms$r2_obs)

