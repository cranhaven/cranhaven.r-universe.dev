## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 6, fig.align = "center")

## -----------------------------------------------------------------------------
library(aphylo)

# Parameter estimates
psi  <- c(.05, .025)
mu_d <- c(.2, .1)
mu_s <- c(.1, .05)
Pi   <- .5

## ----data-sim-----------------------------------------------------------------
set.seed(223)
x <- raphylo(n = 200, psi = psi, mu_d = mu_d, mu_s = mu_s, Pi = Pi)
plot(x)

## ----eval=FALSE---------------------------------------------------------------
#  # Edgelist describing parent->offspring relations
#  write.csv(x$tree, file = "tree.tree", row.names = FALSE)
#  
#  # Tip annotations
#  ann <- with(x, rbind(tip.annotation, node.annotation))
#  write.csv(ann, file = "annotations.csv", row.names = FALSE)
#  
#  # Event types
#  events <- with(x, cbind(c(tip.type*NA, node.type)))
#  rownames(events) <- 1:nrow(events)
#  write.csv(events, file = "events.csv", row.names = FALSE)

## ----inference, cache=TRUE----------------------------------------------------
ans <- aphylo_mcmc(x ~ psi + mu_d + mu_s + Pi)
ans

## ----plot-predict-------------------------------------------------------------
plot(ans)

## ----plot-loglike-------------------------------------------------------------
plot_logLik(ans)

## ----predict-score------------------------------------------------------------
ps <- prediction_score(ans)
ps       # Printing
plot(ps) # and plotting

