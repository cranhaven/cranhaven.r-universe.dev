# check that data has correct format
check_data <- function(aa, ww, xx, yy) {
  if (is.atomic(aa) | is.data.frame(aa)) {
    aa <- as.matrix(aa)
  }
  if (is.atomic(ww) | is.matrix(ww)) {
    ww <- as.data.frame(ww)
  }
  if (is.atomic(xx) | is.data.frame(xx)) {
    xx <- as.matrix(xx)
  }
  if (is.atomic(yy) | is.data.frame(yy)) {
    yy <- as.matrix(yy)
  }

  list(aa = aa, ww = ww, xx = xx, yy = yy)
}

# estimate conditional expectation with splines.
# If all three conditional expectations are estimated with splines,
# the spline basis is constructed only once.
condexp_spline <- function(aa_fit = NULL, xx_fit = NULL, yy_fit = NULL,
                           ww_fit, ww_predict, params = NULL) {
  n <- nrow(ww_fit)
  s <- ncol(ww_fit)

  # set degree and degrees of freedom if not specified by the user
  if (is.null(params)) {
    degree <- 3
    df <- ceiling(n ^ (1 / 5)) + 2
    while ((df + 1) * s + 1 > n) {
      degree <- max(degree - 1, 1)
      df <- df - 1
    }
    if (df < 1) {
      warning("Too little data available: You may want to try OLS instead
to fit conditional expectations")
    }
    params <- list(degree = degree, df = df)
  }

  # build splines
  model_formula <- model_formula_new <- " ~ -1"
  for (i in seq_len(s)) {
    intercept <- FALSE
    if (i == 1) {
      intercept <- TRUE
    }
    spl <- do.call(bs, c(list(x = ww_fit[, i], intercept = intercept),
                         params))
    new_spl <-
      do.call(bs, c(list(x = ww_predict[, i],
                         intercept = intercept,
                         knots = attr(spl, "knots"),
                         Boundary.knots = attr(spl, "Boundary.knots")),
                    params))
    assign(paste("spl", i, sep = ""), spl)
    assign(paste("new_spl", i, sep = ""), new_spl)
    model_formula <- paste(model_formula, " + spl", i, sep = "")
    model_formula_new <- paste(model_formula_new, " + new_spl", i, sep = "")
  }

  # fit model
  new_spl_modelMat <- model.matrix(as.formula(model_formula_new))
  to_return <- list()
  if (!is.null(yy_fit)) {
    ytil_lm <- lm(as.formula(paste("yy_fit", model_formula, sep = "")))

    # predict the fitted model
    to_return <-
      c(to_return,
        list(eYgW_hat = new_spl_modelMat %*% as.matrix(ytil_lm$coefficients)))
  }
  if (!is.null(xx_fit)) {
    xtil_lm <- lm(as.formula(paste("xx_fit", model_formula, sep = "")))
    to_return <-
      c(to_return,
        list(eXgW_hat = new_spl_modelMat %*% as.matrix(xtil_lm$coefficients)))
  }
  if (!is.null(aa_fit)) {
    atil_lm <- lm(as.formula(paste("aa_fit", model_formula, sep = "")))
    to_return <-
      c(to_return,
        list(eAgW_hat = new_spl_modelMat %*% as.matrix(atil_lm$coefficients)))
  }

  if (length(to_return) == 1) {
    to_return <- to_return[[1]]
  }
  to_return
}

# estimate conditional expectation with random forests
condexp_forest <- function(yy_fit, ww_fit, ww_predict, params = NULL) {
  if (is.null(params)) {
    params <- list(nodesize = 5, ntree = 500,
                   na.action = na.omit, replace = TRUE)
  }

  # extract basic numbers from input
  s <- ncol(yy_fit)
  n <- nrow(ww_predict)
  # generate return object to be filled consecutively
  eYgW_hat <- matrix(0, nrow = n, ncol = s)

  # build model formula
  formula <-
    as.formula(paste("resp ~ ",
                     paste(c(rbind(colnames(ww_fit),
                                   c(rep("+", length(colnames(ww_fit)) - 1), ""))),
                           sep = "", collapse = "")))

  # fit and evaluate model for each column of the response
  for (i in seq_len(s)) {
    eYgW_hat[, i] <-
      predict(do.call(randomForest,
                      c(list(formula = formula,
                             data = data.frame(data.frame(resp = as.matrix(yy_fit)[, i]),
                                               ww_fit)),
                        params)),
              newdata = ww_predict)
  }
  eYgW_hat
}

# estimate conditional expectation with ordinary least squares.
condexp_ols <- function(yy_fit, ww_fit, ww_predict, params = NULL) {
  # extract basic numbers from the input
  s <- ncol(yy_fit)
  n <- nrow(ww_predict)
  # generate return object to be filled
  eYgW_hat <- matrix(0, nrow = n, ncol = s)

  # model formula
  formula <-
    as.formula(paste("resp ~ ",
                     paste(c(rbind(colnames(ww_fit),
                                   c(rep("+", length(colnames(ww_fit)) - 1), ""))),
                           sep = "", collapse = "")))

  # fit and evaluate model for each column of the response
  for (i in seq_len(s)) {
    eYgW_hat[, i] <-
      predict(do.call(lm,
                      c(list(formula = formula,
                             data = cbind(data.frame(resp = as.matrix(yy_fit)[, i]),
                                          ww_fit)),
                        params)),
              newdata  = ww_predict)
  }
  eYgW_hat
}

# estimate conditional expectation with elastic net
condexp_elasticnet <- function(yy_fit, ww_fit, ww_predict, params = NULL) {
  # set alpha and nfolds if they are not user specified
 if (is.null(params$alpha)) {
    params <- c(list(alpha = 0.5), params)
  }
  if (is.null(params$nfolds)) {
    params <- c(list(nfolds = 10), params)
  }

  # extract basic numbers from the input
  s <- ncol(yy_fit)
  n <- nrow(ww_predict)
  # generate return object to be filled subsequently
  eYgW_hat <- matrix(0, nrow = n, ncol = s)

  # fit model and evaluate it for every column of the response
  for (i in seq_len(s)) {
    cv_eln <- do.call(cv.glmnet, c(list(y = yy_fit[, i, drop = FALSE],
                                        x = as.matrix(ww_fit)),
                                   params))
    lambda <- cv_eln$lambda.1se
    eYgW_hat[, i] <- predict(cv_eln, lambda = lambda,
                             newx = as.matrix(ww_predict))
  }
  eYgW_hat
}

# estimate conditional expectation with lasso
condexp_lasso <- function(yy_fit, ww_fit, ww_predict, params = NULL) {
  # call the elasticnet estimator with alpha = 1
  condexp_elasticnet(yy_fit = yy_fit, ww_fit = ww_fit, ww_predict = ww_predict,
                     params = c(list(alpha = 1), params))
}

# estimate conditional expectation with ridge
condexp_ridge <- function(yy_fit, ww_fit, ww_predict, params = NULL) {
  # call the elasticnet estimator with alpha = 0
  condexp_elasticnet(yy_fit = yy_fit, ww_fit = ww_fit, ww_predict = ww_predict,
                     params = c(list(alpha = 0), params))
}
