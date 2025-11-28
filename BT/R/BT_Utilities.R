############################
# Accessors.
############################
#' @keywords internal
.iteration_error <-
  function(object,
           which = c('train', 'validation', 'cv')) {
    .check_if_BT_fit(object)
    switch(
      match.arg(which),
      train = object$BTErrors$training.error,
      validation = object$BTErrors$validation.error,
      cv = object$BTErrors$cv.error,
      stop("Unknown error measure")
    )
  }


############################
# Checks different BT arguments.
############################

#' @keywords internal
.check_if_rpart_params <- function(params) {
  .assertInherits(params, "list")
}

#' @keywords internal
.check_tweedie_power <- function(tweedie.power) {
  if (is.null(tweedie.power))
    stop("Please defined a tweedie power.")
  if (!is.double(tweedie.power) ||
      (length(tweedie.power) > 1) ||
      is.infinite(tweedie.power))
    stop("tweedie.power should be a finite numeric")
  if (tweedie.power > 0 &&
      tweedie.power < 1)
    stop("tweedie.power is not defined between 0 and 1.")
}

#' @keywords internal
.check_n_iter <- function(n.iter) {
  if (is.null(n.iter) ||
      !.check_if_natural_number(n.iter) ||
      (length(n.iter) > 1))
    stop("n.iter should be a positive integer.")
}

#' @keywords internal
.check_interaction_depth <- function(interaction.depth) {
  if (!is.null(interaction.depth) &&
      (!.check_if_natural_number(interaction.depth) ||
       (length(interaction.depth) > 1)))
    stop("When defined interaction.depth should be a positive integer.")
}

#' @keywords internal
.check_shrinkage <- function(shrinkage) {
  if (is.null(shrinkage) ||
      (length(shrinkage) > 1))
    stop("Please define a shrinkage parameter.")
  if (shrinkage <= 0 ||
      shrinkage > 1)
    stop("Shrinkage parameter should be > 0 and <=1.")
}

#' @keywords internal
.check_bag_fraction <- function(bag.fraction) {
  if (is.null(bag.fraction) ||
      (length(bag.fraction) > 1))
    stop("Please define a bag.fraction parameter.")
  if (bag.fraction <= 0 ||
      bag.fraction > 1)
    stop("bag.fraction parameter should be > 0 and <=1.")
}

#' @keywords internal
.check_colsample_bytree <- function(colsample.bytree, numExplVar) {
  if (!is.null(colsample.bytree)) {
    if (length(colsample.bytree) > 1)
      stop("colsample.bytree should be a positive integer.")
    if (colsample.bytree > numExplVar)
      stop("colsample.bytree should be lower than the number of explanatory variables.")
    if (!.check_if_natural_number(colsample.bytree))
      stop("colsample.bytree should be a positive integer.")
  }
}

#' @keywords internal
.check_train_fraction <- function(train.fraction) {
  if (is.null(train.fraction) ||
      (length(train.fraction) > 1))
    stop("Please define a train.fraction parameter.")
  if (train.fraction <= 0 ||
      train.fraction > 1)
    stop("When defined train.fraction should be > 0 and <= 1.")
}

#' @keywords internal
.check_keep_data <- function(keep.data) {
  if (!is.logical(keep.data) ||
      (length(keep.data) > 1) ||
      is.na(keep.data))
    stop("keep.data should be a boolean.")
}

#' @keywords internal
.check_is_verbose <- function(is.verbose) {
  if (!is.logical(is.verbose) ||
      (length(is.verbose) > 1) ||
      is.na(is.verbose))
    stop("is.verbose should be a boolean.")
}

#' @keywords internal
.check_cv_folds <- function(cv.folds) {
  if (is.null(cv.folds))
    stop("cv.folds should be defined.")
  if (!.check_if_natural_number(cv.folds) ||
      (length(cv.folds) > 1))
    stop("cv.folds should be a positive integer.")
}

#' @keywords internal
.check_folds_id <- function(folds.id) {
  if (!is.null(folds.id) &&
      (!is.vector(folds.id) ||
       any(is.na(folds.id))))
    stop("When defined folds.id should be a vector of CV index.")
}

#' @keywords internal
.check_n_cores <- function(n.cores) {
  if (!.check_if_natural_number(n.cores) ||
      (length(n.cores) > 1))
    stop("n.cores should be a positive integer.")
  detectedCores <- parallel::detectCores()
  if (n.cores > detectedCores)
    stop(paste0(
      "n.cores is higher than maximum available cores (",
      detectedCores,
      ")."
    ))
  if (n.cores == detectedCores)
    warning(
      "n.cores is equal to maximum available cores. System might become unresponsive and crash in case of insufficient memory.",
      immediate. = T
    )
}

#' @keywords internal
.check_weights <- function(weights) {
  if (!is.double(weights) ||
      any(weights <= 0))
    stop("Non-double and negative weights not allowed.")
}

#' @keywords internal
.check_ABT <- function(ABT) {
  if (!is.logical(ABT) ||
      (length(ABT) > 1) || is.na(ABT))
    stop("ABT should be a boolean.")
}

############################
# Check different outputs.
############################
#' @keywords internal
.has_train_validation_split <- function(object) {
  (object$BTParams$train.fraction != 1) # Previously, !is.null(...)
}

#' @keywords internal
.has_bagging <- function(object) {
  object$BTParams$bag.fraction < 1
}

#' @keywords internal
.has_cross_validation <- function(object) {
  !is.null(object$BTErrors$cv.error)
}

#' @keywords internal
.check_if_natural_number <-
  function(x, tol = .Machine$double.eps ^ 0.5) {
    x > tol & abs(x - round(x)) < tol
  }

############################
# Check different classes.
############################

#' @keywords internal
.assertInherits <- function(object, class.name) {
  if (!isTRUE(inherits(object, class.name))) {
    stop("Function requires a ", class.name, " object.")
  }
}

#' @keywords internal
.check_if_BT_fit <- function(object) {
  .assertInherits(object, "BTFit")
}

#' @keywords internal
.check_if_BTCV_fit <- function(object) {
  .assertInherits(object, "BTCVFit")
}

############################
# Splitting strategy.
############################

#' @keywords internal
.BT_splittingStrategy <- function(rpart_object, interaction.depth) {
  ff <- rpart_object$frame
  # No split available - rootnode.
  if (is.null(rpart_object$splits) ||
      nrow(rpart_object$splits) == 0) {
    return()
  }

  # Points to primary splits in ff
  fpri <- which(ff$var != "<leaf>")
  # Points to primaries in the splits matrix
  spri <-
    1 + cumsum(c(0, 1 + ff$ncompete[fpri] + ff$nsurrogate[fpri]))
  spri <- spri[seq_along(fpri)]

  # Add improvements to primary splits in ff and special treatment for anova.
  ff <-
    cbind(ff[fpri, ], "improve" = rpart_object$splits[spri, "improve"])
  if (rpart_object$method == "anova")
    ff$improve <- ff$improve * ff$dev

  ff$node <- as.numeric(rownames(ff))
  ff <- ff[order(ff$improve, decreasing = T), c("node", "improve")]

  for (i in seq(1, interaction.depth)) {
    if (i == 1) {
      # Initialization
      nodeToKeep <- c(1)
      nodeCandidates <- c(2, 3)
    } else{
      # Be sure we consider only positive improvement (normally not needed, managed by rpart)
      nodeIndex <- match(nodeCandidates, ff[ff$improve > 0, "node"])
      if (all(is.na(nodeIndex))) {
        # No further splits possible.
        return(setdiff(ff$node, nodeToKeep))
      }
      bestSplittingNode <- nodeCandidates[which.min(nodeIndex)]
      nodeToKeep <- c(nodeToKeep, bestSplittingNode)
      nodeCandidates <-
        c(
          setdiff(nodeCandidates, bestSplittingNode),
          c(2 * bestSplittingNode, 2 * bestSplittingNode + 1)
        )
    }
  }
  return(setdiff(ff$node, nodeToKeep))
}
