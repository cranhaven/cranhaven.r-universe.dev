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

# Print contents of \code{divfor} object.
#' @author Roman Hornung, Marvin N. Wright
#' @export
print.divfor <- function(x, ...) {
  cat("divfor result\n\n")
  cat("Call:\n", deparse(x$call), "\n\n")
  cat("Type:                            ", x$treetype, "\n")
  cat("Number of trees:                 ", x$num.trees, "\n")
  cat("Sample size:                     ", x$num.samples, "\n")
  cat("Number of independent variables: ", x$num.independent.variables, "\n")
  cat("Nsplits:                         ", x$nsplits, "\n")
  cat("Proptry:                         ", x$proptry, "\n")
  cat("Target node size:                ", x$min.node.size, "\n")
  cat("Variable importance mode:        ", x$importance.mode, "\n")
  cat("Splitrule:                       ", x$splitrule, "\n")
  if (x$treetype == "Survival") {
    cat("Number of unique death times:    ", length(x$unique.death.times), "\n")
  }
  if (x$treetype == "Classification") {
    cat("OOB prediction error:            ", sprintf("%1.2f %%", 100*x$prediction.error), "\n")
  } else if (x$treetype == "Regression") {
    cat("OOB prediction error (MSE):      ", x$prediction.error, "\n")
  } else if (x$treetype == "Survival") {
    cat("OOB prediction error (1-C):      ", x$prediction.error, "\n")
  } else if (x$treetype == "Probability estimation") {
    cat("OOB prediction error (Brier s.): ", x$prediction.error, "\n")
  } else {
    cat("OOB prediction error:            ", x$prediction.error, "\n")
  }
  if (x$treetype == "Regression") {
    cat("R squared (OOB):                 ", x$r.squared, "\n")
  }
}

# Print contents of \code{interactionfor} object.
#' @author Roman Hornung, Marvin N. Wright
#' @export
print.interactionfor <- function(x, ...) {
  cat("interactionfor result\n\n")
  cat("Call:\n", deparse(x$call), "\n\n")
  cat("Type:                            ", x$treetype, "\n")
  cat("Number of trees:                 ", x$num.trees, "\n")
  cat("Sample size:                     ", x$num.samples, "\n")
  cat("Number of independent variables: ", x$num.independent.variables, "\n")
  cat("Npairs:                          ", x$npairs, "\n")
  cat("Target node size:                ", x$min.node.size, "\n")
  cat("Effect importance mode:          ", x$importance.mode, "\n")
  cat("Splitrule:                       ", x$splitrule, "\n")
  if (x$treetype == "Survival") {
    cat("Number of unique death times:    ", length(x$unique.death.times), "\n")
  }
  if (x$treetype == "Classification") {
    cat("OOB prediction error:            ", sprintf("%1.2f %%", 100*x$prediction.error), "\n")
  } else if (x$treetype == "Regression") {
    cat("OOB prediction error (MSE):      ", x$prediction.error, "\n")
  } else if (x$treetype == "Survival") {
    cat("OOB prediction error (1-C):      ", x$prediction.error, "\n")
  } else if (x$treetype == "Probability estimation") {
    cat("OOB prediction error (Brier s.): ", x$prediction.error, "\n")
  } else {
    cat("OOB prediction error:            ", x$prediction.error, "\n")
  }
  if (x$treetype == "Regression") {
    cat("R squared (OOB):                 ", x$r.squared, "\n")
  }
}

# Print contents of \code{multifor} object.
#' @author Roman Hornung, Marvin N. Wright
#' @export
print.multifor <- function(x, ...) {
  cat("multifor result\n\n")
  cat("Variable importance mode:        ", x$importance.mode, "\n")
  cat("Call:\n", deparse(x$call), "\n\n")
  cat("Type:                            ", x$treetype, "\n")
  cat("Number of trees:                 ", x$num.trees, "\n")
  cat("Sample size:                     ", x$num.samples, "\n")
  cat("Number of independent variables: ", x$num.independent.variables, "\n")
  cat("nvar:                            ", x$nvar, "\n")
  cat("Target node size:                ", x$min.node.size, "\n")
  if (x$treetype == "Classification") {
    cat("OOB prediction error:            ", sprintf("%1.2f %%", 100*x$prediction.error), "\n")
  } else if (x$treetype == "Probability estimation") {
    cat("OOB prediction error (Brier s.): ", x$prediction.error, "\n")
  }
}



# Print contents of \code{divfor.forest} object.
#' @author Marvin N. Wright
#' @export
print.divfor.forest <- function(x, ...) {
  cat("divfor forest object\n\n")
  cat("Type:                         ", x$treetype, "\n")
  cat("Number of trees:              ", x$num.trees, "\n")
  if (x$treetype == "Survival") {
    cat("Number of unique death times: ", length(x$unique.death.times), "\n")
  }
}

# Print contents of \code{interactionfor.forest} object.
#' @author Marvin N. Wright
#' @export
print.interactionfor.forest <- function(x, ...) {
  cat("interactionfor forest object\n\n")
  cat("Type:                         ", x$treetype, "\n")
  cat("Number of trees:              ", x$num.trees, "\n")
  if (x$treetype == "Survival") {
    cat("Number of unique death times: ", length(x$unique.death.times), "\n")
  }
}

# Print contents of \code{multifor.forest} object.
#' @author Marvin N. Wright
#' @export
print.multifor.forest <- function(x, ...) {
  cat("multifor forest object\n\n")
  cat("Type:                         ", x$treetype, "\n")
  cat("Number of trees:              ", x$num.trees, "\n")
}


# Print contents of \code{divfor.prediction} object.
#' @author Marvin N. Wright
#' @export
print.divfor.prediction <- function(x, ...) {
  cat("divfor prediction\n\n")
  cat("Type:                            ", x$treetype, "\n")
  cat("Sample size:                     ", x$num.samples, "\n")
  cat("Number of independent variables: ", x$num.independent.variables, "\n")
  if (x$treetype == "Survival") {
    cat("Number of unique death times:    ", length(x$unique.death.times), "\n")
  }
}

# Print contents of \code{interactionfor.prediction} object.
#' @author Marvin N. Wright
#' @export
print.interactionfor.prediction <- function(x, ...) {
  cat("interactionfor prediction\n\n")
  cat("Type:                            ", x$treetype, "\n")
  cat("Sample size:                     ", x$num.samples, "\n")
  cat("Number of independent variables: ", x$num.independent.variables, "\n")
  if (x$treetype == "Survival") {
    cat("Number of unique death times:    ", length(x$unique.death.times), "\n")
  }
}

# Print contents of \code{multifor.prediction} object.
#' @author Marvin N. Wright
#' @export
print.multifor.prediction <- function(x, ...) {
  cat("multifor prediction\n\n")
  cat("Type:                            ", x$treetype, "\n")
  cat("Sample size:                     ", x$num.samples, "\n")
  cat("Number of independent variables: ", x$num.independent.variables, "\n")
}

#' @export
str.divfor.forest <- function(object, max.level = 2, ...) {
  class(object) <- "list"
  str(object, max.level = max.level, ...)
}

#' @export
str.divfor <- function(object, max.level = 2, ...) {
  class(object) <- "list"
  str(object, max.level = max.level, ...)
}

# Print contents of \code{print.tunedivfor}  object.
#' @author Roman Hornung
#' @export
print.tunedivfor <- function(x, ...) {
  cat("tunedivfor result\n\n")
  cat("Optimized nsplits: ", x$nsplitsopt, "\n")
  cat("Optimized proptry: ", x$proptry, "\n")
  cat("Grid nsplits:      ", paste(unique(x$tunegrid$nsplitsgrid), collapse=", "), "\n")
  cat("Grid proptry:      ", paste(unique(x$tunegrid$proptrygrid), collapse=", "), "\n")
}
