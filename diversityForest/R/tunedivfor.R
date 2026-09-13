# -------------------------------------------------------------------------------
#   This file is part of 'diversityForest'.
#
# 'diversityForest' is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# 'diversityForest' is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with 'diversityForest'. If not, see <http://www.gnu.org/licenses/>.
#
#  NOTE: 'diversityForest' is a fork of the popular R package 'ranger', written by Marvin N. Wright.
#  Most R and C++ code is identical with that of 'ranger'. The package 'diversityForest'
#  was written by taking the original 'ranger' code and making any
#  changes necessary to implement diversity forests.
#
# -------------------------------------------------------------------------------

##' First, both for \code{nsplits} and \code{proptry} a grid of possible values may be provided,
##' where default grids are used if no grids are provided. Second, for each pairwise combination of
##' values from these two grids a forest is constructed. Third, 
##' that pair of \code{nsplits} and \code{proptry} values is used as the optimized set of parameter
##' values that is associated with the smallest out-of-bag prediction error. If several pairs of
##' parameter values are associated with the same smallest out-of-bag prediction error, the
##' pair with the smallest (parameter) values is used.
##' 
##' @title Optimization of the values of the tuning parameters \code{nsplits} and \code{proptry}
##' @param formula Object of class \code{formula} or \code{character} describing the model to fit. Interaction terms supported only for numerical variables.
##' @param data Training data of class \code{data.frame}, \code{matrix}, \code{dgCMatrix} (Matrix) or \code{gwaa.data} (GenABEL).
##' @param nsplitsgrid Grid of values to consider for \code{nsplits}. Default grid: 2, 5, 10, 30, 50, 100, 200.
##' @param proptrygrid Grid of values to consider for \code{proptry}. Default grid: 0.05, 1.
##' @param num.trees.pre Number of trees used for each forest constructed during tuning parameter optimization. Default is 500.
##' @return List with elements
##'   \item{\code{nsplitsopt}}{Optimized value of \code{nsplits}.}
##'   \item{\code{proptryopt}}{Optimized value of \code{proptry}.}
##'   \item{\code{tunegrid}}{Two-dimensional \code{data.frame}, where each row contains one pair of values considered for \code{nsplits} (first entry) and \code{proptry} (second entry).}
##'   \item{\code{ooberrs}}{The out-of-bag prediction errors obtained for each pair of values considered for \code{nsplits} and \code{proptry}, where the ordering of pairs of values is the same as in \code{tunegrid} (see above).}
##' @examples
##' 
##' ## Load package:
##' 
##' library("diversityForest")
##' 
##' 
##' ## Set seed to obtain reproducible results:
##' 
##' set.seed(1234)
##'
##'
##' ## Tuning parameter optimization for the iris data set:
##' 
##' tuneres <- tunedivfor(formula = Species ~ ., data = iris, num.trees.pre = 20)
##' # NOTE: num.trees.pre = 20 is specified too small for practical 
##' # purposes - the out-of-bag error estimates of the forests 
##' # constructed during optimization will be much too variable!!
##' # In practice, num.trees.pre = 500 (default value) or a 
##' # larger number should be used.
##' 
##' tuneres
##' 
##' tuneres$nsplitsopt
##' tuneres$proptryopt
##' tuneres$tunegrid
##' tuneres$ooberrs
##'
##' @author Roman Hornung
##' @references
##' \itemize{
##'   \item Hornung, R. (2022). Diversity forests: Using split sampling to enable innovative complex split procedures in random forests. SN Computer Science 3(2):1, <\doi{10.1007/s42979-021-00920-1}>.
##'   \item Wright, M. N., Ziegler, A. (2017). ranger: A fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software 77:1-17, <\doi{10.18637/jss.v077.i01}>.
##'   }
##' @seealso \code{\link{divfor}}
##' @encoding UTF-8
##' @useDynLib diversityForest, .registration = TRUE
##' @importFrom Rcpp evalCpp
##' @import stats 
##' @import utils
##' @importFrom Matrix Matrix
##' @export
tunedivfor <- function(formula = NULL, data = NULL, 
                       nsplitsgrid = c(2, 5, 10, 30, 50, 100, 200),
                       proptrygrid = c(0.05, 1),
                       num.trees.pre = 500) {

  tunegrid <- expand.grid(proptrygrid=proptrygrid, nsplitsgrid=nsplitsgrid, stringsAsFactors = FALSE)[,2:1]
  
  ooberrs <- 0
  for(i in 1:nrow(tunegrid))
    ooberrs[i] <- divfor(formula, data = data, num.trees = num.trees.pre, nsplits=tunegrid$nsplitsgrid[i], proptry=tunegrid$proptrygrid[i], write.forest=FALSE, verbose=FALSE)$prediction.error
  
  bestind <- which.min(ooberrs)
  nsplitsopt <- tunegrid$nsplitsgrid[bestind]
  proptryopt <- tunegrid$proptrygrid[bestind]
  
  result <- list(nsplitsopt=nsplitsopt, proptryopt=proptryopt, tunegrid=tunegrid, ooberrs=ooberrs)
  class(result) <- "tunedivfor"
  
  return(result)
  
}

