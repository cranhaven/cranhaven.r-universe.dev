#' Bodi: Boosting Diversity Algorithm
#'
#' We provide an implementation of the boosting diversity algorithm. This is a 
#' gradient boosting-based algorithm by incorporating a diversity term to
#' guide the gradient boosting iterations. The idea is to trade off some individual 
#' optimality for global enhancement. The improvement is obtained with progressively 
#' generated predictors by boosting diversity. See Borel et al. (2021) <https://hal.archives-ouvertes.fr/hal-03041309v1>
#' 
#' @name Bodi-package
#' @aliases bodi-package bodi
#' @docType package
#' @author Yannig Goude [aut, cre], Mathias Bourel [aut], Jairo Cugliari [aut], Jean-Michel Poggi [aut]\cr
#' 
#' Mantainer: Yannig Goude <yannig.goude@edf.fr>
#' 
#' @references \itemize{ \item Mathias Bourel, Jairo Cugliari, Yannig Goude, Jean-Michel Poggi. Boosting Diversity in Regression Ensembles. 
#' https://hal.archives-ouvertes.fr/hal-03041309v1 (2021). }
#' @keywords Boosting Regression Ensemble
NULL