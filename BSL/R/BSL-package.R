#'Bayesian synthetic likelihood
#'
#'@description Bayesian synthetic likelihood (BSL,
#'  \insertCite{Price2018;textual}{BSL}) is an alternative to standard,
#'  non-parametric approximate Bayesian computation (ABC). BSL assumes a
#'  multivariate normal distribution for the summary statistic likelihood and it
#'  is suitable when the distribution of the model summary statistics is
#'  sufficiently regular.
#'
#'  In this package, a Metropolis Hastings Markov chain Monte Carlo (MH-MCMC)
#'  implementation of BSL is available. We also include implementations of four
#'  methods (BSL, uBSL, semiBSL and BSLmisspec) and two shrinkage estimators
#'  (graphical lasso and Warton's estimator).
#'
#'  Methods: (1) BSL \insertCite{Price2018}{BSL}, which is the standard form of
#'  Bayesian synthetic likelihood, assumes the summary statistic is roughly
#'  multivariate normal; (2) uBSL \insertCite{Price2018}{BSL}, which uses an
#'  unbiased estimator to the normal density; (3) semiBSL
#'  \insertCite{An2018}{BSL}, which relaxes the normality assumption to an
#'  extent and maintains the computational advantages of BSL without any tuning;
#'  and (4) BSLmisspec \insertCite{Frazier2019}{BSL}, which estimates the
#'  Gaussian synthetic likelihood whilst acknowledging that there may be
#'  incompatibility between the model and the observed summary statistic.
#'
#'  Shrinkage estimators are designed particularly to reduce the number of
#'  simulations if method is BSL or semiBSL: (1) graphical lasso
#'  \insertCite{Friedman2008}{BSL} finds a sparse precision matrix with an
#'  L1-regularised log-likelihood. \insertCite{An2019;textual}{BSL} use
#'  graphical lasso within BSL to bring down the number of simulations
#'  significantly when the dimension of the summary statistic is high; and (2)
#'  Warton's estimator \insertCite{Warton2008}{BSL} penalises the correlation
#'  matrix and is straightforward to compute. When using the Warton's shrinkage
#'  estimator, it is also possible to utilise the Whitening transformation
#'  \insertCite{Kessy2018}{BSL} to help decorrelate the summary statsitics, thus
#'  encouraging sparsity of the synthetic likelihood covariance matrix.
#'
#'  Parallel computing is supported through the \code{foreach} package and users
#'  can specify their own parallel backend by using packages like
#'  \code{doParallel} or \code{doMC}. The \code{n} model simulations required to
#'  estimate the synthetic likelihood at each iteration of MCMC will be
#'  distributed across multiple cores. Alternatively a vectorised simulation
#'  function that simultaneously generates \code{n} model simulations is also
#'  supported.
#'
#'  The main functionality is available through:
#'
#'  \itemize{ \item \code{\link{bsl}}: The general function to perform BSL,
#'  uBSL, or semiBSL (with or without parallel computing). \item
#'  \code{\link{selectPenalty}}: A function to select the penalty when using
#'  shrinkage estimation within BSL or semiBSL. }
#'
#'  Several examples have also been included. These examples can be used to
#'  reproduce the results of An et al. (2019), and can help practitioners learn
#'  how to use the package.
#'
#'  \itemize{
#'
#'  \item \code{\link{ma2}}: The MA(2) example from
#'  \insertCite{An2019;textual}{BSL}.
#'
#'  \item \code{\link{mgnk}}: The multivariate G&K example from
#'  \insertCite{An2019;textual}{BSL}.
#'
#'  \item \code{\link{cell}}: The cell biology example from
#'  \insertCite{Price2018;textual}{BSL} and \insertCite{An2019;textual}{BSL}.
#'
#'  \item \code{\link{toad}}: The toad example from
#'  \insertCite{Marchand2017;textual}{BSL}, and also considered in
#'  \insertCite{An2018;textual}{BSL}.
#'
#'  }
#'
#'  Extensions to this package are planned. For a journal article describing how
#'  to use this package, including full descriptions on the MA(2) and toad examples,
#'  see \insertCite{An2022;textual}{BSL}.
#'
#'@references
#'
#'\insertAllCited{}
#'
#'@author Ziwen An, Leah F. South and Christopher Drovandi
"_PACKAGE"
#> [1] "_PACKAGE"
