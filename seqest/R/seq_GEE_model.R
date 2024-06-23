#' @title The The sequential method for generalized estimating equations case.
#'
#' @description
#' \code{seq_GEE_model} estimates the the effective variables and chooses the
#' subjects sequentially by the generalized estimating equations with adaptive
#' shrinkage estimate method.
#'
#' @details
#' seq_GEE_model fits the clustered data sequentially by generalized estimating
#' equations with adaptive shrinkage estimate. It can detect the effective
#' variables which have the impact on the response and choose the most
#' representative sample point at the same time. Specifically, we fit a initial
#' sample data and determine if the stop condition is reached. If not, we will
#' select the most informative subjects by some criterion. Iteration stops once
#' it meets our requirements.
#' @param formula An object of class "formula" (or one that can be coerced to
#'   that class): a symbolic description of the model to be fitted.
#' @param data  A data frame containing the initial random samples to obtain the
#'   initial estimate of the coefficient. Note that the first column of the data
#'   frame is the response variable, and the rest is the explanatory variables.
#' @param clusterID The id for each subject in the initial samples. Note that
#'   the subjects in the same cluster will have identical id.
#' @param data_pool A data frame containing all the random samples which we will
#'   choose subject from. The first column of the data frame is the response
#'   variable, and the rest is the explanatory variables.
#' @param clusterID_pool The id for each subject in the data_pool. Note that the
#'   subjects in the same cluster will have identical id.
#' @param strategy A character string that determines the sample selection
#'   criterion to be used, matching one of 'random' or 'D_optimal. The default
#'   value is 'D_optimal'.
#' @param d A numeric number specifying the length of the fixed size confidence set for our model.
#' The default value is 0.4.
#' @param family A description of the error distribution and link function to be
#'   used in the model. See family for details of \code{\link{family}}
#'   functions. Matching one of 'gaussian' or 'binomial'
#' @param corstr A character string specifying the correlation structure. The
#'   following are permitted: "independence", "exchangeable" and "ar1".
#' @param contrasts An optional list. See the contrasts.arg of
#'   \code{\link{model.matrix.default}}.
#' @param ... Further arguments passed to or from other methods.
#' @export
#' @return a list containing the following components
#' \item{d}{the length of the fixed size confidence set that we specify}
#' \item{n}{the current sample size when the stopping criterion is satisfied}
#' \item{is_stopped}{the label of sequential iterations stop or not. When the
#' value of is_stopped is TRUE, it means the iteration stops}
#' \item{beta_est}{the parameters that we estimate when the the iteration is
#' finished}
#' \item{cov}{the covariance matrix between the estimated parameters}
#' \item{rho}{estimate of correlation coefficient}
#' \item{nonZeroIdx}{the index of the non zero coefficient}
#' \item{corstr}{the correlation structure. The following are permitted:
#' "independence", "exchangeable" and "ar1"}
#' \item{family}{a description of the error distribution and link function to be
#' used in the model}
#'
#' @references {
#' Chen, Z., Wang, Z., & Chang, Y. I. (2019). Sequential adaptive variables and
#' subject selection for GEE methods. \emph{Biometrics}. doi:10.1111/biom.13160
#' }
#'
#' @seealso{
#'    \code{\link{seq_cat_model}} for categorical case
#'
#'    \code{\link{seq_bin_model}} for binary classification case
#'
#'    \code{\link{seq_ord_model}} for ordinal case.
#'
#'}
#'
#'
#' @examples
#' # generate the toy example
#' data <- gen_GEE_data(numClusters = 75, clusterSize = 5,
#'                     clusterCorstr = 'ar1', clusterRho = 0.3,
#'                     beta = c(1, -1.1, 1.5, -2, rep(0, 50)), family = gaussian(),
#'                     intercept = TRUE, xCorstr = 'ar1',
#'                     xCorRho = 0.5, xVariance = 0.2)
#' df <- data.frame(y = data$y, data$x)
#' clusterID <- data$clusterID
#' pool <- gen_GEE_data(numClusters = 8000, clusterSize = 5,
#'                      clusterCorstr = 'ar1', clusterRho = 0.3,
#'                      beta = c(1, -1.1, 1.5, -2, rep(0, 50)), family = gaussian(),
#'                      intercept = TRUE, xCorstr = 'ar1',
#'                      xCorRho = 0.5, xVariance = 0.2)
#' df_pool <- data.frame(y = pool$y, pool$x)
#' clusterID_pool <- pool$clusterID
#' d<- 0.25
#'
#' # use seq_GEE_model to generalized estimating equations case.
#' # You can remove #' to run the command.
#' # seqRes.ASED <- seq_GEE_model(y ~ .-1, data = df, clusterID = clusterID,
#' #                             data_pool = df_pool,  clusterID_pool = clusterID_pool,
#' #                             strategy = "D-optimal",  d = d, family = gaussian(), corstr = 'ar1')




seq_GEE_model <- function(formula, data=list(),
                          clusterID,
                          data_pool=list(), clusterID_pool,
                          strategy,
                          d=0.4,
                          family = stats::gaussian(link = "identity"),
                          corstr = "independence",
                          contrasts = NULL,
                          ...) {
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "clusterID"), names(mf), 0L)
  mp <- match(c("formula", "data_pool", "clusterID_pool"), names(mf), 0L)
  mpf <- mf[c(1L, mp)]
  mf <- mf[c(1L, m)]
  if (is.null(mf$clusterID)){
    mf$clusterID <- as.name("clusterID")
  }
  if (is.null(mpf$clusterID)){
    mpf$clusterID <- as.name("clusterID")
  }

  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())

  mpf$drop.unused.levels <- TRUE
  names(mpf)[3] <- "data"
  names(mpf)[4] <- "clusterID"

  mpf[[1L]] <- quote(stats::model.frame)
  mpf <- eval(mpf, parent.frame())

  clusterID <- stats::model.extract(mf, clusterID)
  if(is.null(clusterID)){
    stop("clusterID variable not found.")
  }

  clusterID_pool <- stats::model.extract(mpf, clusterID)
  if(is.null(clusterID_pool)){
    stop("clusterID variable of pool not found.")
  }

  y <- stats::model.response(mf, "numeric")
  x <- stats::model.matrix(attr(mf, "terms"), data=mf, contrasts)

  y_pool <- stats::model.response(mpf, "numeric")
  x_pool <- stats::model.matrix(attr(mpf, "terms"), data=mpf, contrasts)

  ## see determines intercept based on 'intercept' parameter
  # if(all(x[,1] ==1)){
  #   x <- x[,-1]
  # }
  #

  if(any(colSums(x) == 0)){

    cat("######## ERROR! ########\n")
    cat(colnames(x)[colSums(x) == 0])
    cat("\n")
    stop("The above factors are not found in the given observations")
  }

  if (is.character(family)) family <- get(family)
  if (is.function(family))  family <- family()

  LINKS <- c("identity", "logit")
  linkv <- match(family$link, LINKS, -1L)
  CORSTRS <- c("independence", "exchangeable", "ar1")
  corstrv <- match(corstr, CORSTRS, -1L)

  clusnew <- c(which(diff(as.numeric(clusterID_pool)) != 0), length(clusterID_pool))
  clusterSize_pool <- c(clusnew[1], diff(clusnew))

  n_pool <- length(clusterSize_pool)
  newClusterID_pool <- rep(1:n_pool, clusterSize_pool)

  isSelected <- vector("logical", n_pool)
  isSelected_long <- rep(isSelected, clusterSize_pool)
  n_selected <- 0

  #get_sample <- sampler(x_pool, clusterID_pool, strategy, family, corstr)

  n_pool <- length(unique(clusterID_pool))
  n_selected <- 0

  if (strategy == 'random'){
    while (n_selected < n_pool) {
      model <- evaluateGEEModel(family, corstr, y, x, clusterID, criterion = "QIC", theta = .76, mostVar = 5)
      stop <- is_stop_ASE(model$sandwich, d, model$nonZeroIdx, verbose=FALSE)

      if (stop$stop) break

      #sampling <- get_sample(model$beta, model$nonZeroIdx, model$call$M, model$rho)
      ind <- sample(which(!isSelected), 1)
      isSelected[ind] <- TRUE
      n_selected <- sum(isSelected)
      isSelected_long <- rep(isSelected, clusterSize_pool)
      idx <- which(clusterID_pool == ind)

      y <- c(y, y_pool[idx])
      x <- rbind(x, x_pool[idx, ])
      clusterID <- c(clusterID, clusterID_pool[idx])

      n_selected <- n_selected
    }

    results <- list(d          = d,
                    n          = length(unique(clusterID)),
                    is_stopped = stop,
                    beta_est      = model$beta,
                    cov        = model$sandwich,
                    rho        = model$rho,
                    nonZeroIdx = model$nonZeroIdx,
                    corstr     = corstr,
                    family     = family$family
    )
    class(results) <- c('seqGEE','list')
    return(results)
  }else{
    while (n_selected < n_pool) {
      model <- evaluateGEEModel(family, corstr, y, x, clusterID, criterion = "QIC", theta = .76, mostVar = 5)
      stop <- is_stop_ASE(model$sandwich, d, model$nonZeroIdx, verbose=FALSE)

      if (stop$stop) break

      #sampling <- get_sample(model$beta, model$nonZeroIdx, model$call$M, model$rho)
      ind <- D_optimal(x_pool[!isSelected_long,],
                       newClusterID_pool[!isSelected_long],
                       model$beta, model$nonZeroIdx,
                       model$call$M, model$rho, linkv, corstrv)
      isSelected[ind] <- TRUE
      n_selected <- sum(isSelected)
      isSelected_long <- rep(isSelected, clusterSize_pool)
      idx <- which(clusterID_pool == ind)

      y <- c(y, y_pool[idx])
      x <- rbind(x, x_pool[idx, ])
      clusterID <- c(clusterID, clusterID_pool[idx])

      n_selected <- n_selected
    }

    results <- list(d          = d,
                    n          = length(unique(clusterID)),
                    is_stopped = stop,
                    beta_est      = model$beta,
                    cov        = model$sandwich,
                    rho        = model$rho,
                    nonZeroIdx = model$nonZeroIdx,
                    corstr     = corstr,
                    family     = family$family
    )
    class(results) <- c('seqGEE','list')
    return(results)

  }


}
