#' Naive Bayes algorithm with case weights
#'
#' @description
#' Function for Naive Bayes algorithm classification with case weights.
#'
#' @param x_train explanatory variables.
#' @param y_train a factor class variable.
#' @param w a vector of case weights.
#' @param discretize If \code{TRUE} numerical variables are discretized and discrete naive bayes is applied,
#' @param breaks number of break points for discretization. Ignored if \code{discretize = TRUE}.
#'
#' @details
#' \code{w_naive_bayes} calls \code{w_gaussian_naive_bayes} or \code{w_discrete_naive_bayes}.
#'
#' if \code{discrete = FALSE}, \code{w_gaussian_naive_bayes} is called. It uses Gaussian densities with case weights and allows
#' multiclass classification.
#'
#' if \code{discrete = TRUE}, \code{w_discrete_naive_bayes} is called. It uses conditional probabilities for each category with
#' laplace smoothing and allows multiclass classification.
#'
#' @return a \code{w_naive_bayes} object with below components.
#'  \item{n_train}{Number of cases in the input dataset.}
#'  \item{p}{Number of explanatory variables.}
#'  \item{x_classes}{A list of datasets, which are \code{x_train} separated
#'  for each class.}
#'  \item{n_classes}{Number of cases for each class in input dataset.}
#'  \item{k_classes}{Number of classes in class variable.}
#'  \item{priors}{Prior probabilities.}
#'  \item{class_names}{Names of classes in class variable.}
#'  \item{means}{Weighted mean estimations for each variable.}
#'  \item{stds}{Weighted standart deviation estimations for each variable.}
#'  \item{categories}{Labels for discretized variables.}
#'  \item{boundaries}{Upper and lower boundaries for discretization.}
#'  \item{ps}{probabilities for each variable categories.}
#' @examples
#'
#'library(rbooster)
#'## short functions for cross-validation and data simulation
#'cv_sampler <- function(y, train_proportion) {
#'  unlist(lapply(unique(y), function(m) sample(which(y==m), round(sum(y==m))*train_proportion)))
#'}
#'
#'data_simulation <- function(n, p, k, train_proportion){
#'  means <- seq(0, k*1.5, length.out = k)
#'  x <- do.call(rbind, lapply(means,
#'                             function(m) matrix(data = rnorm(n = round(n/k)*p,
#'                                                             mean = m,
#'                                                             sd = 2),
#'                                                nrow = round(n/k))))
#'  y <- factor(rep(letters[1:k], each = round(n/k)))
#'  train_i <- cv_sampler(y, train_proportion)
#'
#'  data <- data.frame(x, y = y)
#'  data_train <- data[train_i,]
#'  data_test <- data[-train_i,]
#'  return(list(data = data,
#'              data_train = data_train,
#'              data_test = data_test))
#'}
#'
#'### binary classification example
#'n <- 500
#'p <- 10
#'k <- 2
#'dat <- data_simulation(n = n, p = p, k = k, train_proportion = 0.8)
#'x <- dat$data[,1:p]
#'y <- dat$data[,p+1]
#'
#'x_train <- dat$data_train[,1:p]
#'y_train <- dat$data_train[,p+1]
#'
#'x_test <- dat$data_test[,1:p]
#'y_test <- dat$data_test[,p+1]
#'
#'## discretized Naive Bayes classification
#'mm1 <- w_naive_bayes(x_train = x_train, y_train = y_train, discretize = TRUE, breaks = 4)
#'preds1 <- predict(object = mm1, newdata = x_test, type = "pred")
#'table(y_test, preds1)
#'# or
#'mm2 <- w_discrete_naive_bayes(x_train = x_train, y_train = y_train, breaks = 4)
#'preds2 <- predict(object = mm2, newdata = x_test, type = "pred")
#'table(y_test, preds2)
#'
#'## Gaussian Naive Bayes classification
#'mm3 <- w_naive_bayes(x_train = x_train, y_train = y_train, discretize = FALSE)
#'preds3 <- predict(object = mm3, newdata = x_test, type = "pred")
#'table(y_test, preds3)
#'
#'#or
#'mm4 <- w_gaussian_naive_bayes(x_train = x_train, y_train = y_train)
#'preds4 <- predict(object = mm4, newdata = x_test, type = "pred")
#'table(y_test, preds4)
#'
#'## multiclass example
#'n <- 500
#'p <- 10
#'k <- 5
#'dat <- data_simulation(n = n, p = p, k = k, train_proportion = 0.8)
#'x <- dat$data[,1:p]
#'y <- dat$data[,p+1]
#'
#'x_train <- dat$data_train[,1:p]
#'y_train <- dat$data_train[,p+1]
#'
#'x_test <- dat$data_test[,1:p]
#'y_test <- dat$data_test[,p+1]
#'
#'# discretized
#'mm5 <- w_discrete_naive_bayes(x_train = x_train, y_train = y_train, breaks = 4)
#'preds5 <- predict(object = mm5, newdata = x_test, type = "pred")
#'table(y_test, preds5)
#'
#'# gaussian
#'mm6 <- w_gaussian_naive_bayes(x_train = x_train, y_train = y_train)
#'preds6 <- predict(object = mm6, newdata = x_test, type = "pred")
#'table(y_test, preds6)
#'
#'## example for case weights
#'n <- 500
#'p <- 10
#'k <- 5
#'dat <- data_simulation(n = n, p = p, k = k, train_proportion = 0.8)
#'x <- dat$data[,1:p]
#'y <- dat$data[,p+1]
#'
#'x_train <- dat$data_train[,1:p]
#'y_train <- dat$data_train[,p+1]
#'
#'# discretized
#'weights <- ifelse(y_train == "a" | y_train == "c", 1, 0.01)
#'
#'mm7 <- w_discrete_naive_bayes(x_train = x_train, y_train = y_train, breaks = 4, w = weights)
#'
#'preds7 <- predict(object = mm7, newdata = x_test, type = "pred")
#'table(y_test, preds7)
#'
#'# gaussian
#'weights <- ifelse(y_train == "b" | y_train == "d", 1, 0.01)
#'
#'mm8 <- w_gaussian_naive_bayes(x_train = x_train, y_train = y_train, w = weights)
#'
#'preds8 <- predict(object = mm8, newdata = x_test, type = "pred")
#'table(y_test, preds8)
#'
#' @rdname w_naive_bayes
#' @export

w_naive_bayes <- function(x_train, y_train, w = NULL, discretize = TRUE, breaks = 3){
  if (discretize) {
    model <- w_discrete_naive_bayes(x_train = x_train, y_train = y_train, breaks = breaks, w = w)
    class(model) <- c("w_naive_bayes", "w_discrete_naive_bayes")
  } else {
    model <- w_gaussian_naive_bayes(x_train = x_train, y_train = y_train, w = w)
    class(model) <- c("w_naive_bayes", "w_gaussian_naive_bayes")
  }
  model$discretize <- discretize
  return(model)
}

#' @rdname w_naive_bayes
#' @export

w_gaussian_naive_bayes <- function(x_train, y_train, w = NULL){
  n_train <- nrow(x_train)
  p <- ncol(x_train)

  for (i in 1:p) {
    if (is.factor(x_train[,i])) {
      x_train[,i] <- as.numeric(x_train[,i])
    }
  }

  if (is.null(w)) {
    w <- rep(1, n_train)
  }
  w <- w*n_train/sum(w)

  class_names <- unique(y_train)
  k_classes <- length(class_names)

  n_train <- nrow(x_train)
  n_classes <- sapply(class_names, function(m) sum(y_train == m))

  priors <- sapply(class_names, function(m) sum(w[y_train == m])/n_train)

  x_classes <- lapply(class_names, function(m) x_train[y_train == m, ,drop = FALSE])
  w_classes <- lapply(class_names, function(m) w[y_train == m])

  means <- lapply(1:k_classes, function(m2) sapply(1:p, function(m) {
    ww <- w_classes[[m2]]/sum(w_classes[[m2]])*n_classes[m2]
    ms <- Hmisc::wtd.mean(x = x_classes[[m2]][,m], na.rm = TRUE, weights = ww)
    return(ms)
  }))

  stds <- lapply(1:k_classes, function(m2) sapply(1:p, function(m) {
    ww <- w_classes[[m2]]/sum(w_classes[[m2]])*n_classes[m2]
    vars <- Hmisc::wtd.var(x = x_classes[[m2]][,m], na.rm = TRUE, weights = ww)
    return(sqrt(vars))
  }))

  model <- structure(list(n_train = n_train,
                          p = p,
                          x_classes = x_classes,
                          n_classes = n_classes,
                          k_classes = k_classes,
                          priors = priors,
                          class_names = class_names,
                          means = means,
                          stds = stds),
                     class = "w_gaussian_naive_bayes")
  return(model)
}

#' @rdname w_naive_bayes
#' @export

w_discrete_naive_bayes <- function(x_train,
                                   y_train,
                                   breaks = 3,
                                   w = NULL) {

  n_train <- nrow(x_train)
  p <- ncol(x_train)

  any_numeric <- 0
  for (i in 1:p) {
    if (is.numeric(x_train[,1])) {
      any_numeric <- any_numeric + 1
    }
  }

  if (is.null(w)) {
    w <- rep(1/n_train, n_train)
  }

  w <- w/sum(w)*n_train
  discretization <- FALSE

  if (any_numeric == p) {
    discretization = TRUE
  }

  if (any_numeric < p & any_numeric != 0) {
    stop("All variables must be discrete or continuous. Use 'discretize' to discretize required variables.")
  }

  if (any_numeric == 0) {
    discretization == FALSE
  }

  class_names <- levels(y_train)
  k_classes <- length(class_names)
  n_classes <- sapply(class_names, function(m) sum(y_train == m))
  n_classes_weighted <- sapply(class_names, function(m) sum(w[y_train == m]))
  w_classes <- sapply(class_names, function(m) w[y_train == m])

  if (discretization) {
    m_discretize <- discretize(xx = x_train, breaks = breaks)

    x_train <- m_discretize$x_discrete
    boundaries <- m_discretize$boundaries
    categories <- m_discretize$categories
  } else {
    categories <- list()
    for (i in 1:p) {
      categories[[i]] <- levels(x_train[,i])
    }
  }

  x_classes <- lapply(class_names, function(m) x_train[y_train == m,,drop = FALSE])
  priors <- (n_classes_weighted + 1/k_classes)/(n_train + 1)

  ps <- lapply(1:p, function(m) {
    cat_temp <- categories[[m]]
    k_temp <- length(cat_temp)
    sapply(1:k_classes, function(m2) {
      ff <- c()
      for (ii in 1:k_temp) {
        ff[ii] <- sum((x_classes[[m2]][,m] == cat_temp[ii])*w_classes[[m2]])
      }
      (ff + 1/k_temp)/(sum(ff) + 1)
    })
  })

  results <- structure(list(categories = categories,
                            p = p,
                            boundaries = boundaries,
                            x_classes = x_classes,
                            k_classes = k_classes,
                            class_names = class_names,
                            priors = priors,
                            ps = ps,
                            breaks = breaks
  ), class = "w_discrete_naive_bayes")
  return(results)
}
