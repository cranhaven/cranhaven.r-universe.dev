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

##' @export
importance <- function(x, ...)  UseMethod("importance")

##' Extract variable importance of \code{divfor} object.
##'
##'
##' @title Diversity Forest variable importance
##' @param x \code{divfor} object.
##' @param ... Further arguments passed to or from other methods.
##' @return Variable importance measures.
##' @seealso \code{\link{divfor}}
##' @author Marvin N. Wright
##' @aliases importance
##' @export 
importance.divfor <- function(x, ...) {
  if (!inherits(x, "divfor")) {
    stop("Object ist no divfor object.")
  }
  if (is.null(x$variable.importance) || length(x$variable.importance) < 1) {
    stop("No variable importance found. Please use 'importance' option when growing the forest.")
  }
  return(x$variable.importance)
}

# Author: Marvin N. Wright
importance_pvalues <- function(x, method = c("janitza", "altmann"), num.permutations = 100, formula = NULL, data = NULL, ...) {
  method <- match.arg(method)
  if (!inherits(x, "divfor") & !inherits(x, "holdoutRF")) {
    stop("Object is no divfor or holdoutRF object.")
  }
  if (x$importance.mode == "none" || is.null(x$variable.importance) || length(x$variable.importance) < 1) {
    stop("No variable importance found. Please use 'importance' option when growing the forest.")
  }

  if (method == "janitza") {
    if (x$importance.mode == "impurity") {
      stop("Impurity variable importance found. Please use (hold-out) permutation importance or corrected impurity importance to use this method.")
    }
    if (!inherits(x, "holdoutRF") && x$importance.mode == "permutation") {
      warning("Permutation variable importance found, inaccurate p-values. Please use hold-out permutation importance or corrected impurity importance to use this method.")
    }
    if (x$treetype != "Classification") {
      warning("This method is tested for classification only, use with care.")
    }
    
    ## Mirrored VIMP
    m1 <- x$variable.importance[x$variable.importance < 0]
    m2 <- x$variable.importance[x$variable.importance == 0]
    vimp <- c(m1, -m1, m2)
    
    ## Compute p-value
    ## Note: ecdf is smaller or equal, problems with 0 importance values
    pval <- 1 - numSmaller(x$variable.importance, vimp) / length(vimp)
    
    ## TODO: 100 ok? increase? 
    if (length(m1) == 0) {
      stop("No negative importance values found. Consider the 'altmann' approach.")
    }
    if (length(m1) < 100) {
      warning("Only few negative importance values found, inaccurate p-values. Consider the 'altmann' approach.")
    }
  } else if (method == "altmann") {
    if (!inherits(x, "divfor")) {
      stop("Altmann method not available for holdoutRF objects.")
    }
    if (is.null(formula) || is.null(data)) {
      stop("Formula and data required for the 'altmann' method.")
    }
    if (is.character(formula)) {
      formula <- formula(formula)
    }
    
    ## Permute and compute importance again
    if (x$treetype == "Survival") {
      dependent.variable.name <- all.vars(formula)[1:2]
    } else {
      dependent.variable.name <- all.vars(formula)[1]
    }
    vimp <- sapply(1:num.permutations, function(i) {
      dat <- data
      dat[, dependent.variable.name] <- dat[sample(nrow(dat)), dependent.variable.name]
      divfor(formula, dat, num.trees = x$num.trees, mtry = x$mtry, min.node.size = x$min.node.size, 
             importance = x$importance.mode, replace = x$replace, ...)$variable.importance
    })
    
    ## Compute p-value
    pval <- sapply(1:nrow(vimp), function(i) {
      (sum(vimp[i, ] >= x$variable.importance[i]) + 1)/(ncol(vimp) + 1)
    })

  } else {
    stop("Unknown p-value method. Available methods are: 'janitza' and 'altmann'.")
  }
  
  ## Return VIMP and p-values
  res <- cbind(x$variable.importance, pval)
  colnames(res) <- c("importance", "pvalue")
  return(res)
}
