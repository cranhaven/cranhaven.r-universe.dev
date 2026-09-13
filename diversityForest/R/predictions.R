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

#' @export
predictions <- function(x, ...)  UseMethod("predictions")

# Extract predictions of \code{divfor.prediction} object.
#
#
#' @title Diversity Forest predictions
#' @param x \code{divfor.prediction} object.
#' @param ... Further arguments passed to or from other methods.
#' @return Predictions: Classes for Classification forests, Numerical values for Regressions forests and the estimated survival functions for all individuals for Survival forests.
#' @seealso \code{\link{divfor}}
#' @author Marvin N. Wright
#' @aliases predictions
#' @export
predictions.divfor.prediction <- function(x, ...) {
  if (!inherits(x, "divfor.prediction")) {
    stop("Object ist no divfor.prediction object.")
  }
  if (x$treetype == "Classification" || x$treetype == "Regression" || x$treetype == "Probability estimation") {
    if (is.null(x$predictions)) {
      stop("No predictions found.")
    } else {
      return(x$predictions)
    }
  } else if (x$treetype == "Survival") {
    if (is.null(x$survival)) {
      stop("No predictions found.")
    } else {
      return(x$survival)
    }
  } else {
    stop("Unknown tree type.")
  }
}

# Extract training data predictions of \code{divfor} object.
#
#
#' @title Diversity Forest predictions
#' @param x \code{divfor} object.
#' @param ... Further arguments passed to or from other methods.
#' @return Predictions: Classes for Classification forests, Numerical values for Regressions forests and the estimated survival functions for all individuals for Survival forests.
#' @seealso \code{\link{divfor}}
#' @author Marvin N. Wright
#' @export
predictions.divfor<- function(x, ...) {
  if (!inherits(x, "divfor")) {
    stop("Object ist no divfor object.")
  }
  if (x$treetype == "Classification" || x$treetype == "Regression" || x$treetype == "Probability estimation") {
    if (is.null(x$predictions)) {
      stop("No predictions found.")
    } else {
      return(x$predictions)
    }
  } else if (x$treetype == "Survival") {
    if (is.null(x$survival)) {
      stop("No predictions found.")
    } else {
      return(x$survival)
    }
  } else {
    stop("Unknown tree type.")
  }
}
