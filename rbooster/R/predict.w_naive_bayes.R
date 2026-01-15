#' Predict Discrete Naive Bayes
#'
#' @description
#' Function for Naive Bayes algorithm prediction.
#'
#' @param object "w_bayes" class object..
#' @param newdata new observations which predictions will be made on.
#' @param type "pred" or "prob".
#' @param ... additional arguments.
#'
#' @details
#' Calls \code{predict.w_discrete_naive_bayes} or \code{predict.w_gaussian_naive_bayes}
#' accordingly
#'
#' Type "pred" will give class predictions. "prob" will give probabilities for
#' each class.
#'
#' @return A vector of class predictions or a matrix of class probabilities
#' depending of \code{type}
#'
#' @seealso [predict()], [rbooster::predict.w_discrete_naive_bayes()], [rbooster::predict.w_gaussian_naive_bayes()]
#'
#' @rdname predict.w_naive_bayes
#' @export

predict.w_naive_bayes <- function(object, newdata = NULL, type = "prob", ...){
  if ("w_discrete_naive_bayes" %in% class(object)) {
    preds <- predict.w_discrete_naive_bayes(object = object, newdata = newdata, type = type, ...)
  } else {
    if ("w_gaussian_naive_bayes" %in% class(object)){
      preds <- predict.w_gaussian_naive_bayes(object = object, newdata = newdata, type = type, ...)
    } else {
      stop(paste("Class", class(object), "not applicable."))
    }
  }
  return(preds)
}

#' @rdname predict.w_naive_bayes
#' @export
predict.w_discrete_naive_bayes <- function(object, newdata, type = "prob", ...) {
  categories <- object$categories
  p <- object$p
  boundaries <- object$boundaries
  x_classes <- object$x_classes
  k_classes <- object$k_classes
  class_names <- object$class_names
  priors <- object$priors
  ps <- object$ps
  breaks <- object$breaks

  x_test <- newdata
  n_test <- nrow(x_test)
  p_test <- ncol(x_test)

  m_discretize <- discretize(xx = x_test, breaks = breaks, boundaries = boundaries, categories = categories)
  x_test <- m_discretize$x_discrete

  likelihoods <- list()
  for (i in 1:k_classes) {
    likelihoods[[i]] <- matrix(data = NA, nrow = n_test, ncol = p)
  }

  for (i in 1:p) {
    cat_temp <- categories[[i]]
    k_x <- length(cat_temp)
    for (j in 1:k_classes) {
      for (l in 1:k_x) {
        likelihoods[[j]][x_test[,i] == categories[[i]][l],i] <- ps[[i]][l,j]
      }
    }
  }

  posteriors <- matrix(data = NA, nrow = n_test, ncol = k_classes)

  for (i in 1:k_classes) {
    posteriors[,i] <- apply(cbind(likelihoods[[i]], priors[i]), 1, prod)
  }

  posteriors <- do.call(rbind, lapply(1:n_test, function(m) posteriors[m,]/sum(posteriors[m,])))
  try(colnames(posteriors) <- class_names, silent = TRUE)

  if (type == "prob") {
    return(posteriors)
  }
  if (type == "pred") {
    predictions <- unlist(apply(posteriors, 1, function(m) names(which.max(m))))
    predictions <- factor(predictions, levels = class_names, labels = class_names)
    return(predictions)
  }
}

#' @rdname predict.w_naive_bayes
#' @export
predict.w_gaussian_naive_bayes <- function(object, newdata = NULL, type = "prob", ...){
  n_train <- object$n_train
  p <- object$p
  x_classes <- object$x_classes
  n_classes <- object$n_classes
  k_classes <- object$k_classes
  priors <- object$priors
  class_names <- object$class_names
  means <- object$means
  stds <- object$stds

  x_test <- newdata
  n_test <- nrow(x_test)

  for (i in 1:p) {
    if (is.factor(x_test[,i])) {
      x_test[,i] <- as.numeric(x_test[,i])
    }
  }

  stds <- lapply(stds, function(m){
    m[m<=0] <- 0.001
    m
  })

  densities <- lapply(1:k_classes, function(m) sapply(1:p, function(m2) {
    d <- stats::dnorm(x_test[,m2], mean = means[[m]][m2], sd = stds[[m]][m2])
    d[is.infinite(d)] <- .Machine$double.xmax
    d[d == 0] <- 1e-20
    return(d)
  }))

  likelihoods <- sapply(1:k_classes, function(m) apply(densities[[m]], 1, prod))
  posteriors <- sapply(1:k_classes,
                       function(m) apply(cbind(priors[m], likelihoods[,m]), 1,
                                         prod))

  posteriors <- t(apply(posteriors, 1, function(m) {
    if(all(m == 0)){
      stats::runif(k_classes, min = 0, max = 1)
    } else{
      m
    }
  }))

  posteriors[is.infinite(posteriors)] <- .Machine$double.xmax
  posteriors <- posteriors/apply(posteriors, 1, sum)

  colnames(posteriors) <- class_names

  if (type == "prob") {
    return(posteriors)
  }
  if (type == "pred") {
    predictions <- unlist(apply(posteriors, 1, function(m) names(which.max(m))))
    predictions <- factor(predictions, levels = class_names, labels = class_names)
    return(predictions)
  }
}
