## Tune parameters to get the best fitted model to given data
## Dependency: caret
##
## Author: SR Dhruba, Feb 2021
################################################################################
tune_model <- function(x, y, model, tune_length = 30, search = "random", method = "cv", number = 10,
                       verbose = FALSE, parallel = FALSE, ...) {
    ## Initial check...
    na_omit <- which(!is.na(y))
    x <- as.matrix(x[na_omit, ]);      y <- y[na_omit]

    ## Tune model...
    params <- caret::trainControl(method = method, number = number, search = search, allowParallel = parallel, verboseIter = verbose)
    model <- caret::train(x, y, method = model, tuneLength = tune_length, trControl = params, preProc = NULL, ...)
    model
}


################################################################################
#' Predictive Modeling using Random Forest Regression
#'
#' This function trains a Random Forest regressor using the training data
#' provided and predict response for the test features. This implementation
#' depends on the `randomForest` package.
#'
#' @param x_train Training features for designing the RF regressor.
#' @param y_train Training response for designing the RF regressor.
#' @param x_test Test features for which response values are to be predicted.
#' If `x_test` is not given, the function will return the trained model.
#' @param lims Vector providing the range of the response values for modeling.
#' If missing, these values are estimated from the training response.
#' @param optimize Flag for model tuning. If `TRUE`, performs a grid search for
#' parameters. If `FALSE`, uses the parameters provided. Defaults to `FALSE`.
#' @param n_tree Number of decision trees to be built in the forest. Defaults
#' to `300`. Valid only when `optimize = FALSE`.
#' @param m_try Fraction of the features to be used for building each tree.
#' Defaults to `0.3333` (or 33.33%). Valid only when `optimize = FALSE`.
#' @param seed Seed for random number generator (for reproducible outcomes).
#' Defaults to `NULL`.
#' @param verbose Flag for printing the tuning progress when `optimize = TRUE`.
#' Defaults to `FALSE`.
#' @param parallel Flag for allowing parallel processing when performing grid
#' search _i.e._, `optimimze = TRUE`. Defaults to `FALSE`.
#'
#' @return
#' If `x_test` is missing, the trained RF regressor.
#'
#' If `x_test` is provided, the predicted values using the model.
#' @note The response values are filtered to be bound by range in `lims`.
#'
#' @keywords random-forest decision-tree ensemble-model
#' @export
#' @examples
#' set.seed(86420)
#' x <- matrix(rnorm(3000, 0.2, 1.2), ncol = 3);    colnames(x) <- paste0("x", 1:3)
#' y <- 0.3*x[, 1] + 0.1*x[, 2] - x[, 3] + rnorm(1000, 0, 0.05)
#'
#' ## Get the model only...
#' model <- RF_predict(x_train = x[1:800, ], y_train = y[1:800], n_tree = 300)
#'
#' ## Get predictive performance...
#' y_pred <- RF_predict(x_train = x[1:800, ], y_train = y[1:800], x_test = x[801:1000, ])
#' y_test <- y[801:1000]
#' print(performance(y_test, y_pred, measures = "RSQ"))
#'

## Dependency: stats, randomForest, caret
## Dependency_own: lambda_functions
##
## Author: SR Dhruba, Dec 2020
################################################################################

RF_predict <- function(x_train, y_train, x_test, lims, optimize = FALSE, n_tree = 300, m_try = 0.3333,
                       seed = NULL, verbose = FALSE, parallel = FALSE) {

    set.seed(seed)                          # For reproducibility

    ## Build model...
    if (optimize) {
        Forest <- tune_model(x = x_train, y = y_train, model = "rf", verbose = verbose, parallel = parallel)
    } else {
        if (m_try < 0 | m_try > 1)
            stop("Invalid value! Please choose a value between 0 and 1 (fraction of the features)!")

        m_try  <- round(m_try * ncol(x_train))
        Forest <- randomForest::randomForest(x = x_train, y = y_train, ntree = n_tree, mtry = m_try, replace = TRUE)
    }

    ## Perform prediction & return output...
    if (missing(x_test)) { return(Forest) }

    if (missing(lims)) { lims <- range(y_train) }
    y_pred <- confined(stats::predict(Forest, newdata = x_test), lims)
    y_pred

}


################################################################################
#' Predictive Modeling using Support Vector Machine
#'
#' This function trains a Support Vector Machine regressor using the training
#' data provided and predict response for the test features. This implementation
#' depends on the `kernlab` package.
#'
#' @param x_train Training features for designing the SVM regressor.
#' @param y_train Training response for designing the SVM regressor.
#' @param x_test Test features for which response values are to be predicted.
#' If `x_test` is not given, the function will return the trained model.
#' @param lims Vector providing the range of the response values for modeling.
#' If missing, these values are estimated from the training response.
#' @param kernel Kernel function for SVM implementation. The available options
#' are `linear`, `poly`, `rbf`, and `tanh`. Defaults to `rbf`.
#' @param optimize Flag for model tuning. If `TRUE`, performs a grid search for
#' parameters. If `FALSE`, uses the parameters provided. Defaults to `FALSE`.
#' @param C Cost of constraints violation. This is the constant "C" of the
#' regularization term in the Lagrange formulation. Defaults to `2`. Valid only
#' when `optimize = FALSE`.
#' @param kpar List of kernel parameters. This is a named list that contains
#' the parameters to be used with the specified kernel. The valid parameters
#' for the existing kernels are -
#' * `sigma` for the radial basis (rbf) kernel. Note that this is the
#' **inverse** kernel width.
#' * `degree`, `scale`, `offset` for the polynomial kernel.
#' * `scale`, `offset` for the hyperbolic tangent kernel.
#'
#' Valid only when `optimize = FALSE`. Defaults to `list(sigma = 0.1)`.
#' @param eps The insensitive-loss function used for epsilon-SVR. Defaults to
#' `0.01`.
#' @param seed Seed for random number generator (for reproducible outcomes).
#' Defaults to `NULL`.
#' @param verbose Flag for printing the tuning progress when `optimize = TRUE`.
#' Defaults to `FALSE`.
#' @param parallel Flag for allowing parallel processing when performing grid
#' search _i.e._, `optimimze = TRUE`. Defaults to `FALSE`.
#'
#' @return
#' If `x_test` is missing, the trained SVM regressor.
#'
#' If `x_test` is provided, the predicted values using the model.
#' @note The response values are filtered to be bound by range in `lims`.
#'
#' @keywords support-vector-machine support-vector-regression
#' @export
#' @examples
#' set.seed(86420)
#' x <- matrix(rnorm(3000, 0.2, 1.2), ncol = 3);    colnames(x) <- paste0("x", 1:3)
#' y <- 0.3*x[, 1] + 0.1*x[, 2] - x[, 3] + rnorm(1000, 0, 0.05)
#'
#' ## Get the model only...
#' model <- SVM_predict(x_train = x[1:800, ], y_train = y[1:800], kernel = "rbf")
#'
#' ## Get predictive performance...
#' y_pred <- SVM_predict(x_train = x[1:800, ], y_train = y[1:800], x_test = x[801:1000, ])
#' y_test <- y[801:1000]
#' print(performance(y_test, y_pred, measures = "RSQ"))
#'

## Dependency: stats, kernlab, caret
## Dependency_own: lambda_functions
##
## Author: SR Dhruba, Feb 2021
################################################################################

SVM_predict <- function(x_train, y_train, x_test, lims, kernel = "rbf", optimize = FALSE, C = 2, kpar = list(sigma = 0.1),
                        eps = 0.01, seed = NULL, verbose = FALSE, parallel = FALSE) {

    set.seed(seed)

    ## Build model...
    kernel <- tolower(kernel)
    if (optimize) {
        kern_func <- if (kernel == "linear") "svmLinear" else if (kernel == "poly") "svmPoly" else "svmRadial"
        SVR <- tune_model(x = x_train, y = y_train, model = kern_func, verbose = verbose, parallel = parallel)
    } else {
        kern_func <- if (kernel == "linear") "vanilladot" else paste0(kernel, "dot")
        x_train <- as.matrix(x_train);      y_train <- as.matrix(y_train)
        SVR <- kernlab::ksvm(type = "eps-svr", x = x_train, y = y_train, kernel = kern_func, kpar = kpar, C = C, epsilon = eps)
    }

    ## Perform prediction & return output...
    if (missing(x_test)) { return(SVR) }

    if (missing(lims)) { lims <- range(y_train) }
    y_pred <- confined(kernlab::predict(SVR, newdata = as.matrix(x_test)), lims)
    y_pred

}


################################################################################
#' Predictive Modeling using Elastic Net
#'
#' This function trains a Elastic Net regressor using the training data
#' provided and predict response for the test features. This implementation
#' depends on the `glmnet` package.
#'
#' @param x_train Training features for designing the EN regressor.
#' @param y_train Training response for designing the EN regressor.
#' @param x_test Test features for which response values are to be predicted.
#' If `x_test` is not given, the function will return the trained model.
#' @param lims Vector providing the range of the response values for modeling.
#' If missing, these values are estimated from the training response.
#' @param optimize Flag for model tuning. If `TRUE`, performs a grid search for
#' parameters. If `FALSE`, uses the parameters provided. Defaults to `FALSE`.
#' @param alpha EN mixing parameter with \eqn{0 \le \alpha \le 1}. `alpha = 1`
#' is the lasso penalty, and `alpha = 0` the ridge penalty. Defaults to `0.8`.
#' Valid only when `optimize = FALSE`.
#' @param seed Seed for random number generator (for reproducible outcomes).
#' Defaults to `NULL`.
#' @param verbose Flag for printing the tuning progress when `optimize = TRUE`.
#' Defaults to `FALSE`.
#' @param parallel Flag for allowing parallel processing when performing grid
#' search _i.e._, `optimimze = TRUE`. Defaults to `FALSE`.
#'
#' @return
#' If `x_test` is missing, the trained EN regressor.
#'
#' If `x_test` is provided, the predicted values using the model.
#' @note The response values are filtered to be bound by range in `lims`.
#'
#' @keywords elastic-net penalized-regression regularization
#' @export
#' @examples
#' set.seed(86420)
#' x <- matrix(rnorm(3000, 0.2, 1.2), ncol = 3);    colnames(x) <- paste0("x", 1:3)
#' y <- 0.3*x[, 1] + 0.1*x[, 2] - x[, 3] + rnorm(1000, 0, 0.05)
#'
#' ## Get the model only...
#' model <- EN_predict(x_train = x[1:800, ], y_train = y[1:800], alpha = 0.6)
#'
#' ## Get predictive performance...
#' y_pred <- EN_predict(x_train = x[1:800, ], y_train = y[1:800], x_test = x[801:1000, ])
#' y_test <- y[801:1000]
#' print(performance(y_test, y_pred, measures = "RSQ"))
#'

## Dependency: stats, glmnet, caret
## Dependency_own: lambda_functions
##
## Author: SR Dhruba, Feb 2021
################################################################################

EN_predict <- function(x_train, y_train, x_test, lims, optimize = FALSE, alpha = 0.8, seed = NULL,
                       verbose = FALSE, parallel = FALSE) {

    set.seed(seed)

    ## Build model...
    if (optimize) {
        EN <- tune_model(x = x_train, y = y_train, model = "glmnet", family = "gaussian", verbose = verbose, parallel = parallel)
        lambda <- EN$bestTune$lambda;       alpha <- EN$bestTune$alpha
    } else {
        EN <- glmnet::cv.glmnet(x = as.matrix(x_train), y = y_train, family = "gaussian", alpha = alpha,
                                nfolds = 5, type.measure = "mse")
        lambda <- EN$lambda.min
    }
    EN <- glmnet::glmnet(x = as.matrix(x_train), y = y_train, family = "gaussian", alpha = alpha, lambda = lambda)

    ## Perform prediction & return output...
    if (missing(x_test)) { return(EN) }

    if (missing(lims)) { lims <- range(y_train) }
    y_pred <- confined(glmnet::predict.glmnet(EN, newx = as.matrix(x_test), type = "response"), lims)
    y_pred

}

