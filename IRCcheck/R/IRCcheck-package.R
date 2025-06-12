#' @title IRCcheck:  Irrepresentable Condition Check
#' 
#' @description L1 regularization requires the IRC for consistent model selection, that is,
#'              with more data, the true model is converged upon. This package allows
#'              for checking the IRC in both regression and Gaussian graphical models.
#'              
#'              Importantly, the IRC cannot be checked in real data. The primary 
#'              use for this package is to explore the IRC in a \emph{true} model
#'              that may be used in a simulation study. Alternatively, it is very 
#'              informative to simply look at the IRC as a function of sparsity
#'              and the number of variables, including the regularization path
#'              and false selections.
#'              
#' @docType package
#'
#' @name IRCcheck-package
#' 
NULL

