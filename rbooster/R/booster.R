#' @title  AdaBoost Framework for Any Classifier
#'
#' @description This function allows you to use any classifier to be used in
#' Discrete or Real AdaBoost framework.
#'
#' @param x_train feature matrix.
#' @param y_train a factor class variable. Boosting algorithm allows for
#' k >= 2. However, not all classifiers are capable of multiclass
#' classification.
#' @param classifier pre-ready or a custom classifier function. Pre-ready
#' classifiers are "rpart", "glm", "gnb", "dnb", "earth".
#' @param predictor prediction function for classifier. It's output must be a
#' factor variable with the same levels of y_train
#' @param method "discrete" or "real" for Discrete or Real Adaboost.
#' @param x_test optional test feature matrix. Can be used instead of predict
#' function. print_detail and print_plot gives information about test.
#' @param y_test optional a factor test class variable with the same levels as
#' y_train. Can be used instead of predict function. print_detail and print_plot
#' gives information about test.
#' @param weighted_bootstrap If classifier does not support case weights,
#' weighted_bootstrap must be TRUE used for weighting. If classifier supports
#' weights, it must be FALSE. default is FALSE.
#' @param max_iter maximum number of iterations. Default to 30. Probably should
#' be higher for classifiers other than decision tree.
#' @param lambda a parameter for model weights. Default to 1. Higher values
#' leads to unstable weak classifiers, which is good sometimes. Lower values
#' leads to slower fitting.
#' @param print_detail a logical for printing errors for each iteration.
#' Default to TRUE
#' @param print_plot a logical for plotting errors. Default to FALSE.
#' @param bag_frac a value between 0 and 1. It represents the proportion of
#' cases to be used in each iteration. Smaller datasets may be better to create
#' weaker classifiers. 1 means all cases. Default to 0.5. Ignored if
#' \code{weighted_bootstrap == TRUE}.
#' @param p_weak number of variables to use in weak classifiers. It is the
#' number of columns in \code{x_train} by default. Lower values lead to weaker
#' classifiers.
#' @param ... additional arguments for classifier and predictor functions.
#' weak classifiers.
#'
#' @details
#' \code{method} can be "discrete" and "real" at the moment and indicates Discrete
#' AdaBoost and Real AdaBoost. For multiclass classification, "discrete" means SAMME,
#' "real" means SAMME.R algorithm.
#'
#' Pre-ready classifiers are "rpart", "glm", "dnb", "gnb", "earth", which means
#' CART, logistic regression, Gaussian naive bayes, discrete naive bayes and MARS
#' classifier respectively.
#'
#' \code{predictor} is valid only if a custom \code{classifier} function is
#' given. A custom classifier funtion should be as \code{function(x_train, y_train,
#' weights, ...)} and its output is a model object which can be placed in
#' \code{predictor}. \code{predictor} function is \code{function(model, x_new, type
#' ...)} and its output must be a vector of class predictions. type must be "pred"
#' or "prob", which gives a vector of classes or a matrix of probabilities, which
#' each column represents each class. See \code{vignette("booster", package = "booster")}
#' for examples.
#'
#' \code{lambda} is a multiplier of model weights.
#'
#' \code{weighted_bootstrap} is for bootstrap sampling in each step. If the
#' classifier accepts case weights then it is better to turn it off. If classifier
#' does not accept case weights, then weighted bootstrap will make it into
#' weighted classifier using bootstrap. Learning may be slower this way.
#'
#' \code{bag_frac} helps a classifier to be "weaker" by reducing sample
#' size. Stronger classifiers may require lower proportions of \code{bag_frac}.
#'  \code{p_weak} does the same by reducing numbeer of variables.
#'
#' @return a booster object with below components.
#'  \item{n_train}{Number of cases in the input dataset.}
#'  \item{w}{Case weights for the final boost.}
#'  \item{p}{Number of features.}
#'  \item{weighted_bootstrap}{TRUE if weighted bootstrap applied. Otherwise FALSE.}
#'  \item{max_iter}{Maximum number of boosting steps.}
#'  \item{lambda}{The multiplier of model weights.}
#'  \item{predictor}{Function for prediction}
#'  \item{alpha}{Model weights.}
#'  \item{err_train}{A vector of train errors in each step of boosting.}
#'  \item{err_test}{A vector of test errors in each step of boosting. If there are
#'  no test data, it returns NULL}
#'  \item{models}{Models obtained in each boosting step}
#'  \item{x_classes}{A list of datasets, which are \code{x_train} separated for
#'  each class.}
#'  \item{n_classes}{Number of cases for each class in input dataset.}
#'  \item{k_classes}{Number of classes in class variable.}
#'  \item{bag_frac}{Proportion of input dataset used in each boosting step.}
#'  \item{class_names}{Names of classes in class variable.}
#'
#' @author Fatih Saglam, fatih.saglam@omu.edu.tr
#'
#' @importFrom stats predict
#'
#' @seealso \code{predict.booster}
#'
#' @examples
#'require(rbooster)
#'## n number of cases, p number of variables, k number of classes.
#'cv_sampler <- function(y, train_proportion) {
#'  unlist(lapply(unique(y), function(m) sample(which(y==m), round(sum(y==m))*train_proportion)))
#'}
#'
#'data_simulation <- function(n, p, k, train_proportion){
#'  means <- seq(0, k*2.5, length.out = k)
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
#'### binary classification
#'dat <- data_simulation(n = 500, p = 2, k = 2, train_proportion = 0.8)
#'
#'mm <- booster(x_train = dat$data_train[,1:2],
#'              y_train = dat$data_train[,3],
#'              classifier = "rpart",
#'              method = "discrete",
#'              x_test = dat$data_test[,1:2],
#'              y_test = dat$data_test[,3],
#'              weighted_bootstrap = FALSE,
#'              max_iter = 100,
#'              lambda = 1,
#'              print_detail = TRUE,
#'              print_plot = TRUE,
#'              bag_frac = 1,
#'              p_weak = 2)
#'
#'## test prediction
#'mm$test_prediction
#'## or
#'pp <- predict(object = mm, newdata = dat$data_test[,1:2], type = "pred")
#'## test error
#'tail(mm$err_test, 1)
#'sum(dat$data_test[,3] != pp)/nrow(dat$data_test)
#'
#'### multiclass classification
#'dat <- data_simulation(n = 800, p = 5, k = 3, train_proportion = 0.8)
#'
#'mm <- booster(x_train = dat$data_train[,1:5],
#'              y_train = dat$data_train[,6],
#'              classifier = "rpart",
#'              method = "real",
#'              x_test = dat$data_test[,1:5],
#'              y_test = dat$data_test[,6],
#'              weighted_bootstrap = FALSE,
#'              max_iter = 100,
#'              lambda = 1,
#'              print_detail = TRUE,
#'              print_plot = TRUE,
#'              bag_frac = 1,
#'              p_weak = 2)
#'
#'## test prediction
#'mm$test_prediction
#'## or
#'pp <- predict(object = mm, newdata = dat$data_test[,1:5], type = "pred", print_detail = TRUE)
#'## test error
#'tail(mm$err_test, 1)
#'sum(dat$data_test[,6] != pp)/nrow(dat$data_test)
#'
#'### binary classification, custom classifier
#'dat <- data_simulation(n = 500, p = 10, k = 2, train_proportion = 0.8)
#'x <- dat$data[,1:10]
#'y <- dat$data[,11]
#'
#'x_train <- dat$data_train[,1:10]
#'y_train <- dat$data_train[,11]
#'
#'x_test <- dat$data_test[,1:10]
#'y_test <- dat$data_test[,11]
#'
#'## a custom regression classifier function
#'classifier_lm <- function(x_train, y_train, weights, ...){
#'  y_train_code <- c(-1,1)
#'  y_train_coded <- sapply(levels(y_train), function(m) y_train_code[(y_train == m) + 1])
#'  y_train_coded <- y_train_coded[,1]
#'
#'  model <- lm.wfit(x = as.matrix(cbind(1,x_train)), y = y_train_coded, w = weights)
#'  return(list(coefficients = model$coefficients,
#'              levels = levels(y_train)))
#'}
#'
#'## predictor function
#'
#'predictor_lm <- function(model, x_new, type = "pred", ...) {
#'  coef <- model$coefficients
#'  levels <- model$levels
#'
#'  fit <- as.matrix(cbind(1, x_new))%*%coef
#'  probs <- 1/(1 + exp(-fit))
#'  probs <- data.frame(probs, 1 - probs)
#'  colnames(probs) <- levels
#'
#'  if (type == "pred") {
#'    preds <- factor(levels[apply(probs, 1, which.max)], levels = levels, labels = levels)
#'    return(preds)
#'  }
#'  if (type == "prob") {
#'    return(probs)
#'  }
#'}
#'
#'## real AdaBoost
#'mm <- booster(x_train = x_train,
#'              y_train = y_train,
#'              classifier = classifier_lm,
#'              predictor = predictor_lm,
#'              method = "real",
#'              x_test = x_test,
#'              y_test = y_test,
#'              weighted_bootstrap = FALSE,
#'              max_iter = 50,
#'              lambda = 1,
#'              print_detail = TRUE,
#'              print_plot = TRUE,
#'              bag_frac = 0.5,
#'              p_weak = 2)
#'
#'## test prediction
#'mm$test_prediction
#'pp <- predict(object = mm, newdata = x_test, type = "pred", print_detail = TRUE)
#'## test error
#'tail(mm$err_test, 1)
#'sum(y_test != pp)/nrow(x_test)
#'
#'## discrete AdaBoost
#'mm <- booster(x_train = x_train,
#'              y_train = y_train,
#'              classifier = classifier_lm,
#'              predictor = predictor_lm,
#'              method = "discrete",
#'              x_test = x_test,
#'              y_test = y_test,
#'              weighted_bootstrap = FALSE,
#'              max_iter = 50,
#'              lambda = 1,
#'              print_detail = TRUE,
#'              print_plot = TRUE,
#'              bag_frac = 0.5,
#'              p_weak = 2)
#'
#'## test prediction
#'mm$test_prediction
#'pp <- predict(object = mm, newdata = x_test, type = "pred", print_detail = TRUE)
#'## test error
#'tail(mm$err_test, 1)
#'sum(y_test != pp)/nrow(x_test)
#'
#'# plot function can be used to plot errors
#'plot(mm)
#'
#'# more examples are in vignette("booster", package = "rbooster")
#'
#' @references
#' Freund, Y., & Schapire, R. E. (1997). A decision-theoretic generalization of
#' on-line learning and an application to boosting. Journal of computer and
#' system sciences, 55(1), 119-139.
#'
#' Hastie, T., Rosset, S., Zhu, J., & Zou, H. (2009). Multi-class AdaBoost.
#' Statistics and its Interface, 2(3), 349-360.
#'
#' @rdname booster
#' @export

booster <- function(x_train,
                    y_train,
                    classifier = "rpart",
                    predictor = NULL,
                    method = "discrete",
                    x_test = NULL,
                    y_test = NULL,
                    weighted_bootstrap = FALSE,
                    max_iter = 50,
                    lambda = 1,
                    print_detail = TRUE,
                    print_plot = FALSE,
                    bag_frac = 0.5,
                    p_weak = NULL,
                    ...) {

  bb <- get(paste0(method, "_adaboost"))
  mm <- bb(x_train = x_train,
           y_train = y_train,
           classifier = classifier,
           predictor = predictor,
           x_test = x_test,
           y_test = y_test,
           weighted_bootstrap = weighted_bootstrap,
           max_iter = max_iter,
           lambda = lambda,
           print_detail = print_detail,
           print_plot = print_plot,
           bag_frac = bag_frac,
           p_weak = p_weak,
           ... = )
  results <- mm
  results$method <- method
  class(results) <- "booster"

  return(results)
}


#' @rdname booster
#' @export
discrete_adaboost <- function(x_train,
                              y_train,
                              classifier = "rpart",
                              predictor = NULL,
                              x_test = NULL,
                              y_test = NULL,
                              weighted_bootstrap = FALSE,
                              max_iter = 50,
                              lambda = 1,
                              print_detail = TRUE,
                              print_plot = FALSE,
                              bag_frac = 0.5,
                              p_weak = NULL,
                              ...) {

  n_train <- nrow(x_train)
  p <- ncol(x_train)

  if (is.null(p_weak)) {
    p_weak <- p
  }

  if (p_weak > p) {
    p_weak <- p
    warning("p_weak is set to ncol(x_train).")
  }

  class_names <- as.character(levels(y_train))
  k_classes <- length(class_names)

  n_classes <- sapply(class_names, function(m) sum(y_train == m))
  x_classes <- lapply(class_names, function(m) x_train[y_train == m,])

  if (k_classes > 2 & !is.function(classifier)) {
    if (classifier == "glm") {
      stop("glm does not allow for multiclass classification")
    }
  }

  if (is.character(classifier)) {
    txt <- classifier
    if (txt %in% c("rpart", "glm", "gnb", "dnb", "earth")) {
      classifier <- get(paste0("classifier_", txt), pos = 2)
      predictor <- get(paste0("predictor_", txt), pos = 2)
    } else {
      stop("Classifier must be custom function or one of the following: 'rpart', 'glm', 'nb', 'earth'")
    }
  }

  n_selected <- floor((1 - bag_frac) * n_train)
  if (weighted_bootstrap) {
    sampler <- function(w) {
      forced_i <- c(sapply(1:k_classes,
                           function(m) sample(which(y_train == class_names[m]),
                                              2)))
      return(c(sample(x = setdiff(1:n_train, forced_i),
                      size = 0.632*n_train - k_classes,
                      replace = TRUE,
                      prob = w[setdiff(1:n_train, forced_i)]),
               forced_i))
    }
  } else {
    sampler <- function(w) {
      forced_i <- c(sapply(1:k_classes,
                           function(m) sample(which(y_train == class_names[m]),
                                              2)))
      return(c(sample(x = setdiff(1:n_train, forced_i),
                      size = n_train - n_selected - 2*k_classes,
                      replace = FALSE),
               forced_i))
    }
  }

  w <- rep(1/n_train, n_train)
  err <- c()
  err_train <- c()
  alpha <- c()
  models <- list()
  selected_vars <- data.frame()

  fit_train <- matrix(0, nrow = n_train, ncol = k_classes)
  if (!is.null(x_test) & !is.null(y_test)) {
    n_test <- nrow(x_test)
    err_test <- c()
    fit_test <- matrix(0, nrow = n_test, ncol = k_classes)
  }

  y_codes <- c(-1/(k_classes - 1), 1)
  y_train_coding <- sapply(class_names, function(m) y_codes[as.numeric(y_train == m) + 1])

  for (i in 1:max_iter) {
    selection_i <- sampler(w)
    selected_vars <- rbind(selected_vars, sort(sample(1:p, p_weak)))
    selected_var <- unlist(selected_vars[i,])

    x_temp <- x_train[selection_i,selected_var, drop = FALSE]
    y_temp <- y_train[selection_i]
    w_temp <- w[selection_i]
    n_temp <- nrow(x_temp)

    if (weighted_bootstrap) {
      w_model <- NULL
    } else {
      w_model <- w_temp*n_temp/sum(w_temp)
    }

    models[[i]] <- classifier(x_temp, y_temp, w_temp*n_temp/sum(w_temp))
    preds <- predictor(model = models[[i]], x_new = x_train[,selected_var, drop = FALSE])
    preds_num <- sapply(class_names, function(m) y_codes[as.numeric(preds == m) + 1])
    errors <- preds != y_train

    err[i] <- sum(w*(errors))/sum(w)
    alpha[i] <- lambda * (k_classes - 1)/k_classes * log((1 - err[i])/err[i]) + log(k_classes - 1)

    fit_train <- fit_train + alpha[i]*preds_num
    fit_train_pred <- class_names[apply(fit_train, 1, which.max)]

    err_train[i] <- sum(fit_train_pred != y_train)/n_train

    if ((is.null(x_test) | is.null(y_test)) & print_detail) {
      cat(i, " Train err:",
          formatC(err_train[i], digits = 5, format = "f"),
          ", Weighted err:",
          formatC(err[i], digits = 5, format = "f"),
          "\n",
          sep = "")
      next
    }

    preds_test <- predictor(models[[i]], x_test[,selected_var, drop = FALSE])
    preds_num_test <- sapply(class_names, function(m) y_codes[as.numeric(preds_test == m) + 1])
    fit_test <- fit_test + alpha[i]*preds_num_test
    fit_test_pred <- class_names[apply(fit_test, 1, which.max)]

    err_test[i] <- sum(fit_test_pred != y_test)/n_test

    if (print_detail) {
      cat(i, " Train err:",
          formatC(err_train[i], digits = 5, format = "f"),
          ", Test err:",
          formatC(err_test[i], digits = 5, format = "f"),
          ", Weighted err:",
          formatC(err[i], digits = 5, format = "f"),
          "\n", sep = "")
    }

    if ((1 - err[i]) == 1 | err[i] == 1 | abs(err[i] - (1 - 1/k_classes)) < 1e-15) {
      warning(paste("Learning stopped at iteration", i))
      break
    } else {
      w <- w*exp(alpha[i]*errors)
      w <- w/sum(w)
    }
  }

  if (length(models) > 1 & print_plot) {
    if (!is.null(x_test) & !is.null(y_test)) {
      plot(err_train, xlab = "Iteration", ylab = "Error",
           ylim = c(0, max(c(err_train, err_test)))*1.1)
      graphics::lines(err_train)
      graphics::points(err_test, col = "red", pch = 2)
      graphics::lines(err_test, col = "red")
      graphics::legend("topright", legend = c("Train", "Test"), lty = c(1,1),
                       col = c("black", "red"), pch = c(1,2))
    } else {
      plot(err_train, xlab = "Iteration", ylab = "Error",
           ylim = c(min(c(err_train)), max(c(err_train))))
      graphics::lines(err_train)
      graphics::legend("topright", legend = c("Train"), lty = c(1),
                       col = c("black"),
                       pch = c(1))
    }
  }

  if (is.null(x_test)) {
    err_test <- NULL
    fit_test_pred <- NULL
  }

  results <- structure(list(method = "discrete",
                            n_train = n_train,
                            w = w,
                            p = p,
                            p_weak = p_weak,
                            selected_vars = selected_vars,
                            weighted_bootstrap = weighted_bootstrap,
                            max_iter = max_iter,
                            lambda = lambda,
                            predictor = predictor,
                            alpha = alpha,
                            err_train = err_train,
                            err_test = err_test,
                            test_prediction = fit_test_pred,
                            models = models,
                            x_classes = x_classes,
                            n_classes = n_classes,
                            k_classes = k_classes,
                            bag_frac = bag_frac,
                            class_names = class_names), class = "discrete_adaboost")

  return(results)
}


#' @rdname booster
#' @export
real_adaboost <- function(x_train,
                          y_train,
                          classifier = "rpart",
                          predictor = NULL,
                          x_test = NULL,
                          y_test = NULL,
                          weighted_bootstrap = FALSE,
                          max_iter = 50,
                          lambda = 1,
                          print_detail = TRUE,
                          print_plot = FALSE,
                          bag_frac = 0.5,
                          p_weak = NULL,
                          ...) {

  n_train <- nrow(x_train)
  p <- ncol(x_train)

  if (is.null(p_weak)) {
    p_weak <- p
  }

  if (p_weak > p) {
    p_weak <- p
    warning("p_weak is set to ncol(x_train).")
  }

  class_names <- as.character(levels(y_train))
  k_classes <- length(class_names)

  n_classes <- sapply(class_names, function(m) sum(y_train == m))
  x_classes <- lapply(class_names, function(m) x_train[y_train == m,])

  if (k_classes > 2 & !is.function(classifier)) {
    if (classifier == "glm") {
      stop("glm does not allow for multiclass classification")
    }
  }

  if (is.character(classifier)) {
    txt <- classifier
    if (txt %in% c("rpart", "glm", "gnb", "dnb", "earth")) {
      classifier <- get(paste0("classifier_", txt), pos = 2)
      predictor <- get(paste0("predictor_", txt), pos = 2)
    } else {
      stop("Classifier must be custom function or one of the following: 'rpart', 'glm', 'nb', 'earth'")
    }
  }

  n_selected <- floor((1 - bag_frac) * n_train)
  if (weighted_bootstrap) {
    sampler <- function(w) {
      forced_i <- c(sapply(1:k_classes,
                           function(m) sample(which(y_train == class_names[m]),
                                              2)))
      return(c(sample(x = setdiff(1:n_train, forced_i),
                      size = 0.632*n_train - k_classes,
                      replace = TRUE,
                      prob = w[setdiff(1:n_train, forced_i)]),
               forced_i))
    }
  } else {
    sampler <- function(w) {
      forced_i <- c(sapply(1:k_classes,
                           function(m) sample(which(y_train == class_names[m]),
                                              2)))
      return(c(sample(x = setdiff(1:n_train, forced_i),
                      size = n_train - n_selected - 2*k_classes,
                      replace = FALSE),
               forced_i))
    }
  }

  w <- rep(1/n_train, n_train)
  err <- c()
  err_train <- c()
  models <- list()
  selected_vars <- data.frame()
  alpha <- NULL

  fit_train <- matrix(0, nrow = n_train, ncol = k_classes)
  if (!is.null(x_test) & !is.null(y_test)) {
    n_test <- nrow(x_test)
    err_test <- c()
    fit_test <- matrix(0, nrow = n_test, ncol = k_classes)
  }

  y_codes <- c(-1/(k_classes - 1), 1)
  y_train_coding <- sapply(class_names, function(m) y_codes[as.numeric(y_train == m) + 1])

  for (i in 1:max_iter) {
    selection_sample <- sampler(w)
    selected_vars <- rbind(selected_vars, sort(sample(1:p, p_weak)))
    selected_var <- unlist(selected_vars[i,])

    x_temp <- x_train[selection_sample,selected_var, drop = FALSE]
    y_temp <- y_train[selection_sample]
    w_temp <- w[selection_sample]
    n_temp <- nrow(x_temp)

    if (weighted_bootstrap) {
      w_model <- NULL
    } else {
      w_model <- w_temp*n_temp/sum(w_temp)
    }

    models[[i]] <- classifier(x_temp, y_temp, w_model)
    probs <- predictor(models[[i]], x_train[,selected_var,drop = FALSE], type = "prob")
    preds <- class_names[apply(probs, 1, which.max)]
    errors <- preds != y_train

    err[i] <- sum(w*(errors))/sum(w)


    probs_fit <- (probs*(1 - 2*1e-5) + 1e-5)
    log_probs_fit <- log(probs_fit)

    alpha <- (-1*lambda*((k_classes - 1)/k_classes)*rowSums(y_train_coding*log_probs_fit))

    fit <- (k_classes - 1)*(log_probs_fit - (1/k_classes)*rowSums(log_probs_fit))
    fit_train <- fit_train + fit
    fit_train_pred <- class_names[apply(fit_train, 1, which.max)]

    err_train[i] <- sum(fit_train_pred != y_train)/n_train

    if ((is.null(x_test) | is.null(y_test)) & print_detail) {
      cat(i, " Train err:",
          formatC(err_train[i], digits = 5, format = "f"),
          ", Weighted err:",
          formatC(err[i], digits = 5, format = "f"),
          "\n",
          sep = "")
      next
    }

    probs_test <- predictor(models[[i]], x_test[,selected_var,drop = FALSE], type = "prob")
    preds_test <- class_names[apply(probs_test, 1, which.max)]
    probs_fit_test <- (probs_test*(1 - 2*1e-5) + 1e-5)
    log_probs_fit_test <- log(probs_fit_test)

    fit <- (k_classes - 1)*(log_probs_fit_test - (1/k_classes)*rowSums(log_probs_fit_test))
    fit_test <- fit_test + fit
    fit_test_pred <- class_names[apply(fit_test, 1, which.max)]

    err_test[i] <- sum(fit_test_pred != y_test)/n_test

    if (print_detail) {
      cat(i, " Train err:",
          formatC(err_train[i], digits = 5, format = "f"),
          ", Test err:",
          formatC(err_test[i], digits = 5, format = "f"),
          ", Weighted err:",
          formatC(err[i], digits = 5, format = "f"),
          "\n", sep = "")
    }



    if ((1 - err[i]) == 1 | err[i] == 1 | err[i] > (1 - 1/k_classes) | abs(err[i] - (1 - 1/k_classes)) < 1e-15) {
      warning(paste("Learning stopped at iteration", i))
      break
    } else {
      w_update_selected <- alpha < 0
      w[w_update_selected] <- w[w_update_selected]*exp(alpha[w_update_selected])
      w <- w/sum(w)
    }
  }

  if (length(models) > 1 & print_plot) {
    if (!is.null(x_test) & !is.null(y_test)) {
      plot(err_train, xlab = "Iteration", ylab = "Error",
           ylim = c(0, max(c(err_train, err_test)))*1.1)
      graphics::lines(err_train)
      graphics::points(err_test, col = "red", pch = 2)
      graphics::lines(err_test, col = "red")
      graphics::legend("topright", legend = c("Train", "Test"), lty = c(1,1),
                       col = c("black", "red"), pch = c(1,2))
    } else {
      plot(err_train, xlab = "Iteration", ylab = "Error",
           ylim = c(min(c(err_train)), max(c(err_train))))
      graphics::lines(err_train)
      graphics::legend("topright", legend = c("Train"), lty = c(1),
                       col = c("black"),
                       pch = c(1))
    }
  }

  if (is.null(x_test)) {
    err_test <- NULL
    fit_test_pred <- NULL
  }

  results <- structure(list(method = "real",
                            n_train = n_train,
                            w = w,
                            p = p,
                            p_weak = p_weak,
                            selected_vars = selected_vars,
                            weighted_bootstrap = weighted_bootstrap,
                            max_iter = max_iter,
                            lambda = lambda,
                            predictor = predictor,
                            alpha = alpha,
                            err_train = err_train,
                            err_test = err_test,
                            test_prediction = fit_test_pred,
                            models = models,
                            x_classes = x_classes,
                            n_classes = n_classes,
                            k_classes = k_classes,
                            bag_frac = bag_frac,
                            class_names = class_names), class = "real_adaboost")

  return(results)
}
