## ----fivenets-cycle5----------------------------------------------------------
library(ergmito)
data(fivenets)
(ans <- ergmito(fivenets ~ edges + triadcensus(15)))

## ----fivenets-ll--------------------------------------------------------------
# Approach using the limiting value
l <- with(ans$formulae, {
  sapply(1:nnets(ans), function(i) {
    # Preparing suff stats
    S <- t(t(stats_statmat[[i]]) - target_stats[i, ])
    W <- stats_weights[[i]]
    theta <- coef(ans)["edges"]
    
    # Index of members of S0
    s0idx <- which(S[,2] == 0)
    
    - log(sum(W[s0idx] * exp(S[s0idx,1] * theta)))
  })
})
sum(l)

# Which should be equivalent to the approach with large negative number
ans$formulae$loglik(c(coef(ans)["edges"], -1e100))

## ----grad-fivenets------------------------------------------------------------
g <- with(ans$formulae, {
  sapply(1:nnets(ans), function(i) {
    # Preparing suff stats
    S <- t(t(stats_statmat[[i]]) - target_stats[i, ])
    W <- stats_weights[[i]]
    theta <- coef(ans)["edges"]
    
    # Index of members of S0
    s0idx <- which(S[,2] == 0)
    
    - sum(W[s0idx] * S[s0idx,1] * exp(S[s0idx,1] * theta))/
      sum(W[s0idx] * exp(S[s0idx,1] * theta))
  })
})
sum(g)

# Which is equivalent to
ans$formulae$grad(c(coef(ans)["edges"], -1e100))

## ----hess-fivenets------------------------------------------------------------
h <- with(ans$formulae, {
  sapply(1:nnets(ans), function(i) {
    # Preparing suff stats
    S <- t(t(stats_statmat[[i]]) - target_stats[i, ])
    W <- stats_weights[[i]]
    theta <- coef(ans)["edges"]
    
    # Index of members of S0
    s0idx <- which(S[,2] == 0)
    
    # First part
    - sum(W[s0idx] * S[s0idx,1]^2 * exp(S[s0idx,1] * theta))/
      sum(W[s0idx] * exp(S[s0idx,1] * theta)) + 
      # Second bit
      sum(W[s0idx] * S[s0idx,1] * exp(S[s0idx,1] * theta)) ^ 2/
      sum(W[s0idx] * exp(S[s0idx,1] * theta))^2
  })
})
sum(h)

## ----hessian2-----------------------------------------------------------------
ans$formulae$hess(c(coef(ans)["edges"], -1e100))

