#' Method for estimating the relative influence.
#'
#' Helper function for computing the relative influence of each variable in the BT object.
#'
#' @param BTFit_object a \code{\link{BTFit}} object.
#' @param n.iter number of boosting iterations used for computation. If not provided, the function will perform a best guess approach to determine the optimal number of iterations. In fact,
#' if a validation set was used during the fitting, the retained number of iterations is the one corresponding to the lowest validation set error ; otherwise, if cross-validation was performed, the
#' number of iterations resulting in lowest cross-validation error will be used; otherwise, if the out-of-bag parameter was defined, the OOB error will be used to determine the optimal
#' number of iterations; otherwise, all iterations will be used.
#' @param rescale whether or not the results should be rescaled (divided by the maximum observation). Default set to \code{FALSE}.
#' @param sort.it whether or not the results should be (reverse) sorted. Default set to \code{FALSE}.
#' @param consider.competing whether or not competing split should be considered in the relative influence computation. Default set to \code{FALSE}.
#' @param consider.surrogates whether or not surrogates should be considered in the relative influence computation. Default set to \code{FALSE}.
#'
#' @return Returns by default an unprocessed vector of estimated relative influences. If the \code{rescale} and \code{sort.it} arguments are used, it returns
#' a processed version of the same vector.
#'
#' @details
#' This function is not intended for end-user use. It performs the relative influence computation and is called during the summary function.
#' Note that a permutation approach is not yet implemented.
#'
#' @author Gireg Willame \email{gireg.willame@@gmail.com}
#'
#' \emph{This package is inspired by the \code{gbm3} package. For more details, see \url{https://github.com/gbm-developers/gbm3/}}.
#'
#' @seealso \code{\link{BT}}, \code{\link{BTFit}}, \code{\link{BT_perf}}.
#'
#' @references M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries |: GLMs and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries ||: Tree-Based Methods and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2019). \strong{Effective Statistical Learning Methods for Actuaries |||: Neural Networks and Extensions}, \emph{Springer Actuarial}.
#'
#' M. Denuit, D. Hainaut and J. Trufin (2022). \strong{Response versus gradient boosting trees, GLMs and neural networks under Tweedie loss and log-link}.
#' Accepted for publication in \emph{Scandinavian Actuarial Journal}.
#'
#' M. Denuit, J. Huyghe and J. Trufin (2022). \strong{Boosting cost-complexity pruned trees on Tweedie responses: The ABT machine for insurance ratemaking}.
#' Paper submitted for publication.
#'
#' M. Denuit, J. Trufin and T. Verdebout (2022). \strong{Boosting on the responses with Tweedie loss functions}. Paper submitted for publication.
#'
#' @rdname BT_relative_influence
#' @keywords internal
.BT_relative_influence <- function(BTFit_object,
                                  n.iter,
                                  rescale = FALSE,
                                  sort.it = FALSE,
                                  consider.competing = FALSE,
                                  consider.surrogates = FALSE) {
  # Initial checks
  .check_if_BT_fit(BTFit_object)
  if (!is.logical(rescale) ||
      (length(rescale) > 1) || is.na(rescale))
    stop("rescale argument must be a logical")
  if (!is.logical(sort.it) ||
      (length(sort.it) > 1) || is.na(sort.it))
    stop("sort.it must be a logical")

  # Fill in missing values
  if (missing(n.iter)) {
    if (.has_train_validation_split(BTFit_object)) {
      n.iter <- .BT_callPerformance(BTFit_object, method = "validation")
    }
    else if (.has_cross_validation(BTFit_object)) {
      n.iter <- .BT_callPerformance(BTFit_object, method = "cv")
    }
    else if (.has_bagging(BTFit_object)) {
      n.iter <- .BT_callPerformance(BTFit_object, method = "OOB")
    }
    else{
      n.iter <- BTFit_object$BTParams$n.iter
    }
    message("n.iter not given. Using ", n.iter, " trees.")
  }
  else{
    .check_n_iter(n.iter) # Additional checks on n.iter
    if (n.iter > length(BTFit_object$BTIndivFits)) {
      stop("n.iter exceeds number in fit")
    }
  }

  # Create relative influence for every variable
  rel_inf_verbose <-
    unlist(lapply(BTFit_object$BTIndivFits[seq(1, n.iter)],
                  function(xx) {
                    .get_rel_inf_of_vars(xx,
                                        considerCompeting = consider.competing,
                                        considerSurrogates = consider.surrogates)
                  }))

  # Sum across trees
  rel_inf_compact <-
    unlist(lapply(split(
      rel_inf_verbose, names(rel_inf_verbose)
    ), sum))

  # rel_inf_compact excludes variables that never entered the model
  # insert 0's for the excluded variables
  if (length(BTFit_object$var.names) != length(names(rel_inf_compact))) {
    varToAdd <-
      BTFit_object$var.names[!(BTFit_object$var.names %in% names(rel_inf_compact))]
    rel_inf <- c(rel_inf_compact, rep(0, length(varToAdd)))
    names(rel_inf)[(length(rel_inf_compact) + 1):length(BTFit_object$var.names)] <-
      varToAdd
  } else{
    rel_inf <- rel_inf_compact
  }

  # Rescale and sort
  if (rescale)
    rel_inf <- rel_inf / max(rel_inf)
  if (sort.it)
    rel_inf <- rev(sort(rel_inf))

  return(rel_inf)
}

#### Helper function ####
#' @keywords internal
.get_rel_inf_of_vars <-
  function(rpart_object,
           considerCompeting,
           considerSurrogates) {
    if (!is.null(rpart_object$splits) &
        (nrow(rpart_object$splits) > 0)) {
      frameWithoutLeafs <-
        rpart_object$frame[rpart_object$frame$var != "<leaf>", ]
      generateVec <- function(ncompete, nsurrogate) {
        c(
          "PrimarySplit",
          rep("CompetingSplit", ncompete),
          rep("SurrogateSplit", nsurrogate)
        )
      }
      typeOfSplitList <-
        mapply(
          generateVec,
          frameWithoutLeafs$ncompete,
          frameWithoutLeafs$nsurrogate,
          SIMPLIFY = FALSE
        )
      primarySplitRef <-
        unlist(sapply(seq(1, length(
          typeOfSplitList
        )), function(xx)
          rep(xx, length(
            typeOfSplitList[[xx]]
          ))))
      typeOfSplitVec <- unlist(typeOfSplitList)
      filterVec <- c("PrimarySplit")

      ## According to rpart doc, need to rescale for anova method.
      scaledImportance <- rpart_object$splits[, "improve"]
      indexSurrogate <- (typeOfSplitVec == "SurrogateSplit")
      if (rpart_object$method == "anova") {
        scaledImportance[!indexSurrogate] <-
          rpart_object$splits[!indexSurrogate, "improve"] * frameWithoutLeafs[primarySplitRef[!indexSurrogate], "dev"]
      }
      ## According to rpart doc, need to adjust the surrogates as well.
      if (considerSurrogates) {
        scaledImportance[indexSurrogate] <-
          (scaledImportance[typeOfSplitVec == "PrimarySplit"][primarySplitRef[indexSurrogate]] *
             rpart_object$splits[indexSurrogate, "adj"])
        filterVec <- c(filterVec, "SurrogateSplit")
      }
      filterVec <-
        if (considerCompeting)
          c(filterVec, "CompetingSplit")
      else
        filterVec
      indexFilter <- which(typeOfSplitVec %in% filterVec)
      return(lapply(split(
        scaledImportance[indexFilter], names(scaledImportance[indexFilter])
      ), sum))
    }
    else
      (return(list()))
  }
#.get_rel_inf_of_vars <- function(rpart_object) {
#  if (!is.null(rpart_object$splits)) return(lapply(split(rpart_object$splits[,3], names(rpart_object$splits[,3])), sum)) # 3 - Improvement
#  else (return(list())) # With rpart : splits isn't returned if we've a single node (i.e. no splits).
#}
