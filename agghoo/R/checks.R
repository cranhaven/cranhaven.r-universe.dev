# Internal usage: check and fill arguments with default values.

defaultLoss_classif <- function(y1, y2) {
  if (is.null(dim(y1)))
    # Standard case: "hard" classification
    mean(y1 != y2)
  else {
    # "Soft" classification: predict() outputs a probability matrix
    # In this case "target" could be in matrix form.
    if (!is.null(dim(y2)))
      mean(rowSums(abs(y1 - y2)))
    else {
      # Or not: y2 is a "factor".
      y2 <- as.character(y2)
      # NOTE: the user should provide target in matrix form because
      # matching y2 with columns is rather inefficient!
      names <- colnames(y1)
      positions <- list()
      for (idx in seq_along(names))
        positions[[ names[idx] ]] <- idx
      mean(vapply(
        seq_along(y2),
        function(idx) sum(abs(y1[idx,] - positions[[ y2[idx] ]])),
        0))
    }
  }
}

defaultLoss_regress <- function(y1, y2) {
  mean(abs(y1 - y2))
}

# TODO: allow strings like "MSE", "abs" etc
checkLoss <- function(loss, task) {
  if (!is.null(loss) && !is.function(loss))
    stop("loss: function(y1, y2) --> Real")
  if (is.null(loss)) {
    loss <- if (task == "classification") {
      defaultLoss_classif
    } else {
      defaultLoss_regress
    }
  }
  loss
}

checkCV <- function(CV) {
  if (is.null(CV))
    CV <- list(type="MC", V=10, test_size=0.2, shuffle=TRUE)
  else {
    if (!is.list(CV))
      stop("CV: list of type('MC'|'vfold'), V(integer, [test_size, shuffle]")
    if (is.null(CV$type)) {
      warning("CV$type not provided: set to MC")
      CV$type <- "MC"
    }
    if (is.null(CV$V)) {
      warning("CV$V not provided: set to 10")
      CV$V <- 10
    }
    if (CV$type == "MC" && is.null(CV$test_size))
      CV$test_size <- 0.2
    if (CV$type == "vfold" && is.null(CV$shuffle))
      CV$shuffle <- TRUE
  }
  CV
}

checkDaTa <- function(data, target) {
  if (!is.data.frame(data) && !is.matrix(data))
    stop("data: data.frame or matrix")
  if (is.data.frame(target) || is.matrix(target)) {
    if (!is.numeric(target))
      stop("multi-columns target must be a probability matrix")
    if (nrow(target) != nrow(data) || ncol(target) == 1)
      stop("target probability matrix does not match data size")
  }
  else if (!is.numeric(target) && !is.factor(target) && !is.character(target))
    stop("target: numeric, factor or character vector")
}

checkTask <- function(task, target) {
  if (!is.null(task))
    task <- match.arg(task, c("classification", "regression"))
  ifelse(is.numeric(target), "regression", "classification")
}

checkModPar <- function(gmodel, params) {
  if (is.character(gmodel))
    gmodel <- match.arg(gmodel, c("knn", "ppr", "rf", "tree"))
  else if (!is.null(gmodel) && !is.function(gmodel))
    stop("gmodel: function(dataHO, targetHO, param) --> function(X) --> y")
  if (is.numeric(params) || is.character(params))
    params <- as.list(params)
  if (!is.list(params) && !is.null(params))
    stop("params: numerical, character, or list (passed to model)")
  if (is.function(gmodel) && !is.list(params))
    stop("params must be provided when using a custom model")
  if (is.list(params) && is.null(gmodel))
    stop("model (or family) must be provided when using custom params")
  list(gmodel=gmodel, params=params)
}
