#' @title slimrec
#' @description Sparse Linear Method to Predict Ratings and Top-N
#'   Reccomendations
#' @details \strong{Sparse linear method}
#'   (\href{http://glaros.dtc.umn.edu/gkhome/node/774}{DOI:
#'   10.1109/ICDM.2011.134}) predicts ratings and top-n recommendations suited
#'   for sparse implicit positive feedback systems. SLIM is decomposed into
#'   multiple elasticnet optimization problems which are solved in parallel over
#'   multiple cores. The package is based on  "SLIM: Sparse Linear Methods for
#'   Top-N Recommender Systems"  by Xia Ning and George Karypis.
#'
#'   The method predicts ratings of a user for a given item as a linear
#'   combination ratings of all other items provided by the user. The
#'   coefficients for an item are determined elastic-net regression (both L1 and
#'   L2 regularization) over ratings matrix.
#'
#'   The optimization problem solves:
#'
#'   \deqn{\min_{c_{j,.}} \frac{1}{2} \|a_{j,.} - Ac_{j,.}\|^2_{2} +
#'   \frac{\beta}{2} \|c_{j,.}\|^2_{2} + \gamma \|c_{j,.}\|_{1}} subject to
#'   \eqn{c_{j,j} = 0} and optional non-negative constraint \eqn{c_{j,.} >= 0}
#'   where \eqn{a_{j,.}} is the j th column of the input ratings matrix and
#'   \eqn{c_{j,.}} is the j th column of the coefficient matrix(to be
#'   determined).
#'
#'   The method assumes that unknown rating values to be zero. Hence, it is
#'   primarily designed for implicit feeback mechanisms, but not restricted
#'   them. The main use of the ratings is to generate top-n lists of users and
#'   items.
#'
#'   The package provides three functions: \itemize{ \item \code{slim}: Function
#'   to compute ratings and coefficient matrix for the sparse ratings matrix
#'   using SLIM method. \item \code{tune_slim}: Function to arrive at an optimal
#'   value of \code{alpha} for \code{\link{slim}}. \item \code{top_rows/cols}:
#'   Functions to find row/column numbers corresponding the largest values in a
#'   particular column/row of a matrix. This is helpful to generate top-n users
#'   or items as recommendations. }
#'
#'   The package is primarily based on the wonderful package \pkg{glmnet} by
#'   Jerome Friedman, Trevor Hastie, Noah Simon, Rob Tibshirani.
#'
#'   If you intend to use SLIM method for large matrices( around >= 1e7
#'   ratings), this package might be slow enough to be practically useful even
#'   in parallel mode. You might want to look at \pkg{biglasso} and other
#'   implementations like \href{http://librec.net/}{librec}.
#'
#' @import assertthat
#' @import parallel
#' @import glmnet
#' @import Matrix
#' @import bigmemory
#' @import pbapply
#' @importFrom stats runif
#'
"_PACKAGE"