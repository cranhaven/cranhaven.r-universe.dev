# The penalised fit, over the core `src/ts_penalised.cpp` compiles into both languages. Nothing
# here decides anything: the design, the family, the case weights and the inner folds are settled
# above, and what is left is to hand them over column-major and to shape what comes back.
#
# A fit is a plain list of numbers, so it round trips through `saveRDS()` and predicts on another
# machine, which is what every other fitted object in the package is.

.penalised_path <- function(x, y, w, family, alpha, n_lambda = 100L, lambda = NULL,
                            thresh = 1e-8, standardize = TRUE, intercept = TRUE,
                            max_pass = 1e6) {
  .penalised_shape(ts_penalised_path_(as.numeric(x), as.numeric(y), as.numeric(w), nrow(x),
                                      ncol(x), family, alpha, as.integer(n_lambda), 0, lambda,
                                      thresh, standardize, intercept, max_pass),
                   colnames(x))
}

.penalised_cv <- function(x, y, w, family, alpha, fold, n_fold, n_lambda = 100L, thresh = 1e-8,
                          standardize = TRUE, intercept = TRUE, max_pass = 1e6,
                          threads = 1L) {
  fit <- ts_penalised_cv_(as.numeric(x), as.numeric(y), as.numeric(w), nrow(x), ncol(x), family,
                          alpha, as.integer(n_lambda), 0, NULL, thresh, standardize, intercept,
                          as.integer(fold), as.integer(n_fold), max_pass, as.integer(threads))
  out <- .penalised_shape(fit, colnames(x))
  out$cv_mean <- fit$cv_mean
  out$cv_sd <- fit$cv_sd
  out$lambda_min <- fit$lambda[fit$index_min]
  out$lambda_1se <- fit$lambda[fit$index_1se]
  out$fold_stalled <- fit$fold_stalled
  out
}

# Whether a cross-validated fit's path, on every unit or on any fold, ended at a penalty it did
# not settle at.
.penalised_stopped <- function(fit) {
  is.list(fit) && (fit$stalled > 0L || any(fit$fold_stalled > 0L))
}

.penalised_shape <- function(fit, columns) {
  beta <- matrix(fit$beta, nrow = fit$n_column,
                 dimnames = list(columns, NULL))
  list(lambda = fit$lambda, a0 = fit$a0, beta = beta, df = fit$df, dev_ratio = fit$dev_ratio,
       null_deviance = fit$null_deviance, passes = fit$passes, stalled = fit$stalled,
       family = fit$family)
}

# The penalty a fit is read at: a point of the path by name, or a number, which is interpolated
# between the two points around it the way the path itself is read.
.penalty_at <- function(model, s) {
  if (is.numeric(s)) {
    return(as.numeric(s)[1L])
  }
  named <- c(lambda.min = "lambda_min", lambda.1se = "lambda_1se")
  if (!is.character(s) || length(s) != 1L || !s %in% names(named) ||
        is.null(model[[named[[s]]]])) {
    stop("a penalised fit is read at \"lambda.min\", at \"lambda.1se\", or at a penalty of its ",
         "own, and a path fitted without a cross-validation carries neither name.", call. = FALSE)
  }
  model[[named[[s]]]]
}

.penalised_predict <- function(model, newx, s = "lambda.min") {
  ts_penalised_predict_(model$lambda, model$a0, as.numeric(model$beta), model$family,
                        .penalty_at(model, s), as.numeric(newx), nrow(newx))
}

.penalised_coef <- function(model, s = "lambda.min") {
  out <- ts_penalised_coef_(model$lambda, model$a0, as.numeric(model$beta), model$family,
                            .penalty_at(model, s))
  stats::setNames(out, c("(Intercept)", rownames(model$beta)))
}
