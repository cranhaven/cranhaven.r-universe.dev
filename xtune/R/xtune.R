#' Regularized regression incorporating external information
#'
#' \code{xtune} uses an Empirical Bayes approach to integrate external information into regularized regression models for both linear and categorical outcomes. It fits models with feature-specific penalty parameters based on external information.
#' @param X Numeric design matrix of explanatory variables (\eqn{n} observations in rows, \eqn{p} predictors in columns). \code{xtune} includes an intercept by default.
#' @param Y Outcome vector of dimension \eqn{n}.
#' @param Z Numeric information matrix about the predictors (\eqn{p} rows, each corresponding to a predictor in X; \eqn{q} columns of external information about the predictors, such as prior biological importance). If Z is the grouping of predictors, it is best if user codes it as a dummy variable (i.e. each column indicating whether predictors belong to a specific group).
#' @param U Covariates to be adjusted in the model (matrix with \eqn{n} observations in rows, \eqn{u} predictors in columns). Covariates are non-penalized in the model.
#' @param family The family of the model according to different types of outcomes including "linear", "binary", and "multiclass".
#' @param c The elastic-net mixing parameter ranging from 0 to 1. When  \eqn{c} = 1, the model corresponds to Lasso. When \eqn{c} is set to 0, it corresponds to Ridge. For values between 0 and 1 (with a default of 0.5), the model corresponds to the elastic net.
#' @param epsilon The parameter controls the boundary of the \code{alpha}. The maximum value that \code{alpha} could achieve equals to epsilon times of alpha max calculated by the pathwise coordinate descent. A larger value of epsilon indicates a stronger shrinkage effect (with a default of 5).
#' @param sigma.square A user-supplied noise variance estimate. Typically, this is left unspecified, and the function automatically computes an estimated sigma square values using R package \code{selectiveinference}.
#' @param message Generates diagnostic message in model fitting. Default is TRUE.
#' @param control Specifies \code{xtune} control object. See \code{\link{xtune.control}} for more details.
#' @details \code{xtune} has two main usages:
#' \itemize{
#' \item The basic usage of it is to choose the tuning parameter \eqn{\lambda} in elastic net regression using an
#' Empirical Bayes approach, as an alternative to the widely-used cross-validation. This is done by calling \code{xtune} without specifying external information matrix Z.
#'
#' \item More importantly, if an external information Z about the predictors X is provided, \code{xtune} can allow predictor-specific shrinkage
#' parameters for regression coefficients in penalized regression models. The idea is that Z might be informative for the effect-size of regression coefficients, therefore we can guide the penalized regression model using Z.
#' }
#'
#' Please note that the number of rows in Z should match with the number of columns in X. Since each column in Z is a feature about X. \href{https://github.com/JingxuanH/xtune}{See here for more details on how to specify Z}.
#'
#' A majorization-minimization procedure is employed to fit \code{xtune}.
#' @return An object with S3 class \code{xtune} containing:
#' \item{beta.est}{The fitted vector of coefficients.}
#' \item{penalty.vector}{The estimated penalty vector applied to each regression coefficient. Similar to the \code{penalty.factor} argument in \link{glmnet}.}
#' \item{lambda}{The estimated \eqn{\lambda} value. Note that the lambda value is calculated to reflect that the fact that penalty factors are internally rescaled to sum to nvars in \link{glmnet}. Similar to the \code{lambda} argument in \link{glmnet}.}
#' \item{alpha.est}{The estimated second-level coefficient for prior covariate Z. The first value is the intercept of the second-level coefficient.}
#' \item{n_iter}{Number of iterations used until convergence.}
#' \item{method}{Same as in argument above}
#' \item{sigma.square}{The estimated sigma square value using \code{\link{estimateVariance}}, if \code{sigma.square} is left unspecified. When \code{family} equals to "binary" or "multiclass", the \code{sigma.square} equals to NULL.}
#' \item{family}{same as above}
#' \item{likelihood.score}{A vector containing the marginal likelihood value of the fitted model at each iteration.}
#' @author Jingxuan He and Chubing Zeng
#' @seealso \link{predict_xtune}, as well as \link{glmnet}.
#' @examples
#' ## use simulated example data
#' set.seed(1234567)
#' data(example)
#' X <- example$X
#' Y <- example$Y
#' Z <- example$Z
#'
#' ## Empirical Bayes tuning to estimate tuning parameter, as an alternative to cross-validation:
#' \donttest{
#' fit.eb <- xtune(X=X,Y=Y, family = "linear")
#' fit.eb$lambda
#' }
#'
#' ### compare with tuning parameter chosen by cross-validation, using glmnet
#' \donttest{
#' fit.cv <- glmnet::cv.glmnet(x=X,y=Y,alpha = 0.5)
#' fit.cv$lambda.min
#' }
#'
#' ## Feature-specific penalties based on external information Z:
#' \donttest{
#' fit.diff <- xtune(X=X,Y=Y,Z=Z, family = "linear")
#' fit.diff$penalty.vector
#' }
#'
#' @import glmnet crayon selectiveInference lbfgs
#' @importFrom stats optim
#' @export
#'
#'
xtune <- function(X, Y, Z = NULL,U = NULL,
                  family=c("linear","binary","multiclass"),
                  c = 0.5,
                  epsilon = 5,
                  sigma.square = NULL,
                  message = TRUE,
                  control = list()) {

        # function call
        this.call <- match.call()

        family = match.arg(family)

        # check user inputs Check X, X need to be a matrix or data.frame
        np = dim(X)
        if (is.null(np) | (np[2] <= 1))
                stop("X must be a matrix with 2 or more columns")

        nobs = as.integer(np[1])
        nvar = as.integer(np[2])

        ## Check Y
        if (length(unique(Y)) == 2 & family == "linear"){
                warning(paste("Y only has two unique values, coerce the family to binary"))
                family = "binary"
        }

        ## Check if the numbers of observation of X and Y are consistent
        dimY = dim(Y)
        nrowY = ifelse(is.null(dimY), length(Y), dimY[1])
        if (nrowY != nobs)
                stop(paste("number of observations in Y (", nrowY, ") not equal to the number of rows of X (",
                           nobs, ")", sep = ""))

        # Check Z If no Z provided, then provide Z of a single column of 1
        if (is.null(Z)) {
                if (message == TRUE){
                        cat("No Z matrix provided, only a single tuning parameter will be estimated using empirical Bayes tuning","\n")
                }
                dat_ext = matrix(rep(1, nvar))
        } else {
                #### If Z is provided:
                dimZ = dim(Z)
                nrowZ = ifelse(is.null(dimZ), length(Z), dimZ[1])
                ncolZ = ifelse(is.null(dimZ), 1, dimZ[2])

                if (nrowZ != nvar) {
                        ## check the dimension of Z
                        stop(paste("number of rows in Z (", nrow(Z), ") not equal to the number of columns in X (",
                                   nvar, ")", sep = ""))
                } else if (!is.matrix(Z)) {
                        ## check is Z is a matrix
                        Z = as.matrix(Z)
                        dat_ext <- Z
                } else if (!(typeof(Z) %in% c("double", "integer"))) {
                        stop("Z contains non-numeric values")
                } else if (all(apply(Z, 2, function(x) length(unique(x)) == 1) == TRUE)) {
                        ## check if all rows in Z are the same
                        warning(paste("All rows in Z are the same, this Z matrix is not useful, EB tuning will be performed to estimate
                                      a single tuning parameter","\n"))
                        dat_ext = matrix(rep(1, nvar))
                } else {
                        dat_ext <- Z
                }

                if (message == TRUE){
                        cat(paste("Z provided, start estimating individual tuning parameters","\n"))
                }
        }


        # Standardize the Z and add an intercept in Z to stabilize the estimation
        # Append the intercept of the prior covariate coefficient
        dat_ext = sweep(dat_ext,2,colMeans(dat_ext))
        dat_ext = cbind(1, dat_ext)

        drop(Z)
        nex = ncol(dat_ext)

        ## Check U
        if(!is.null(U)){
                if (!is.matrix(U) & !is.data.frame(U) ) {
                        stop("Covariate should be in the matrix or data frame format")
                }  else {
                        U = U
                }
        }

        ## Check c
        if (!is.numeric(c) | c >1 | c< 0) {
                stop("c should be a numerical number ranging from 0 to 1")
        }  else {
                c = c
        }

        ## Check sigma.square
        if (is.null(sigma.square)) {
                sigma.square = estimateVariance(X, Y)
        } else if (!is.double(sigma.square) | is.infinite(sigma.square) | sigma.square <=
                   0) {
                stop("sigma square should be a positive finite number")
        } else {
                sigma.square = sigma.square
        }

        # check control object
        control <- do.call("xtune.control", control)
        control <- initialize_control(control, dat_ext)

        # core function
        if(family == "linear"){
                fit <- xtune.fit(X = X, Y = Y, Z = dat_ext, U = U,
                                 c = c,
                                 epsilon = epsilon,
                                 sigma.square = sigma.square,
                                 alpha.est.init = control$alpha.est.init,
                                 maxstep = control$maxstep,
                                 maxstep_inner = control$maxstep_inner,
                                 margin = control$margin,
                                 margin_inner = control$margin_inner,
                                 compute.likelihood = control$compute.likelihood,
                                 verbosity = control$verbosity,
                                 standardize = control$standardize,
                                 intercept = control$intercept)
        } else if(family == "binary" | family == "multiclass"){
                fit <- mxtune.fit(X = X, Y = Y, Z = dat_ext, U = U,
                                 c = c,
                                 epsilon = epsilon,
                                 alpha.est.init = control$alpha.est.init,
                                 max_s = control$max_s,
                                 maxstep = control$maxstep,
                                 maxstep_inner = control$maxstep_inner,
                                 margin_s = control$margin_s,
                                 margin = control$margin,
                                 margin_inner = control$margin_inner,
                                 compute.likelihood = control$compute.likelihood,
                                 verbosity = control$verbosity,
                                 standardize = control$standardize,
                                 intercept = control$intercept)
        }



        # Check status of model fit
        if (length(unique(fit$penalty.vector)) == 1) {
                fit$penalty.vector = rep(1,nvar)
        }

        fit$family = family
        return(structure(fit, class = "xtune"))
}





#' Control function for xtune fitting
#'
#' @description Control function for \code{\link{xtune}} fitting.
#' @param alpha.est.init Initial values of alpha vector supplied to the algorithm. Alpha values are the hyper-parameters for the double exponential prior of regression coefficients, and it controls the prior variance of regression coefficients. Default is a vector of 0 with length p.
#' @param max_s Maximum number of outer loop iterations for binary or multiclass outcomes. Default is 20.
#' @param margin_s Convergence threshold of the outer loop for binary or multiclass outcomes. Default is 1e-5.
#' @param maxstep Maximum number of iterations. Default is 100.
#' @param margin Convergence threshold. Default is 1e-3.
#' @param maxstep_inner Maximum number of iterations for the inner loop of the majorization-minimization algorithm. Default is 100.
#' @param margin_inner Convergence threshold for the inner loop of the majorization-minimization algorithm. Default is 1e-3.
#' @param compute.likelihood Should the function compute the marginal likelihood for hyper-parameters at each step of the update? Default is TRUE.
#' @param verbosity Track algorithm update process? Default is FALSE.
#' @param standardize Standardize X or not, same as the standardized option in \code{glmnet}.
#' @param intercept Should intercept(s) be fitted (default=TRUE) or set to zero (FALSE), same as the intercept option in \code{glmnet}.
#' @return A list of control objects after the checking.
#' @export
#'
xtune.control <- function(alpha.est.init = NULL, max_s = 20 ,margin_s = 0.00001, maxstep = 100, margin = 0.001,
                          maxstep_inner = 100, margin_inner = 0.001, compute.likelihood = FALSE, verbosity = FALSE,
                          standardize = TRUE, intercept = TRUE) {

        if (max_s < 0) {
                stop("Error: max out loop step must be a postive integer")
        }

        if (margin_s < 0) {
                stop("Error: outer loop tolerance must be greater than 0")
        }
        if (maxstep < 0) {
                stop("Error: max out loop step must be a postive integer")
        }

        if (margin < 0) {
                stop("Error: outer loop tolerance must be greater than 0")
        }

        if (maxstep_inner < 0) {
                stop("Error: max inner loop step must be a postive integer")
        }

        if (margin_inner < 0) {
                stop("Error: inner loop tolerance must be greater than 0")
        }

        if (!is.logical(compute.likelihood)) {
                stop("Error: compute.likelihood should be either TRUE or FALSE")
        }

        if (!is.logical(verbosity)) {
                stop("Error: verbosity should be either TRUE or FALSE")
        }

        control_obj <- list(alpha.est.init = alpha.est.init, max_s = max_s, margin_s = margin_s, maxstep = maxstep, margin = margin,
                            maxstep_inner = maxstep_inner, margin_inner = margin_inner, compute.likelihood = compute.likelihood,
                            verbosity = verbosity, standardize = standardize, intercept = intercept)
}

initialize_control <- function(control_obj, ext) {
        if (is.null(control_obj$alpha.est.init) | all(apply(ext, 2, function(x) length(unique(x)) ==
                                                        1) == TRUE)) {
                alpha.est.init = rep(0, ncol(ext))
        } else if (length(control_obj$alpha.est.init) != ncol(ext)) {
                warning(cat(paste("number of elements in alpha initial values (", length(alpha.est.init),
                                  ") not equal to the number of columns of ext (", q, ")", ", alpha initial set to be all 0",
                                  sep = "")))
        } else {
                alpha.est.init = control_obj$alpha.est.init
        }
        control_obj$alpha.est.init <- alpha.est.init
        return(control_obj)
}

