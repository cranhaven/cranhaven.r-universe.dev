## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SBMSplitMerge)

## -----------------------------------------------------------------------------
edge_model <- edgemod(
	function(e, p) dweibull(e, p[1,,], p[2,,], log=TRUE)
	,
	function(p) rweibull(1, p[1], p[2])
)

## -----------------------------------------------------------------------------
block_model <- dma(1, 10)

## -----------------------------------------------------------------------------
param_model <- parammod(
		function(params){
				dgamma(params$theta0[1], 1, 1, log=TRUE) +
						dgamma(params$theta0[2], 1, 1, log=TRUE) +
						sum(dgamma(params$thetak[,1], 1, 1, log=TRUE)) +
						sum(dgamma(params$thetak[,2], 1, 1, log=TRUE))
		},
		function(kappa){
				params(
						c(rgamma(1, 1, 1), rgamma(1, 1, 1))
				,
						cbind(rgamma(kappa, 1, 1), rgamma(kappa, 1, 1))
				)
		},
		function(x){ cbind(log(x[1]), log(x[2]))},
		function(x){ cbind(exp(x[1]), exp(x[2]))},
		function(x){ -log(x[1])-log(x[2])}
		)

## -----------------------------------------------------------------------------
model <- sbmmod(block_model, param_model, edge_model)

## -----------------------------------------------------------------------------
set.seed(1)
true_blocks   <- blocks(rep(c(1, 2, 3), c(10, 20, 20)))
true_params   <- params(c(1, 0.5), cbind(1, c(3,4,5)))
true_sbm      <- sbm(true_blocks, true_params)
weibull_edges <- redges(true_sbm, model$edge)

## -----------------------------------------------------------------------------
print(true_blocks)
plot(true_blocks)
plot(weibull_edges)

## -----------------------------------------------------------------------------
weibull_posterior <- sampler(weibull_edges, model, 300, "rj", sigma=0.1)

## -----------------------------------------------------------------------------
weibull_plots <- eval_plots(weibull_posterior)
weibull_plots

