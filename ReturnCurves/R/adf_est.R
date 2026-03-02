
.adf_est.class <- setClass("adf_est.class", representation(dataexp = "array",
                                                           w = "numeric",
                                                           method = "character",
                                                           q = "numeric",
                                                           qalphas = "numeric",
                                                           k = "numeric",
                                                           constrained = "logical",
                                                           tol = "numeric",
                                                           par_init = "numeric",
                                                           interval = "numeric",
                                                           adf = "numeric"))

#' An S4 class to represent the estimation of the Angular Dependence Function
#'
#' @slot dataexp A matrix containing the data on standard exponential margins.
#' @slot w Sequence of rays between \code{0} and \code{1}. Default is \code{NULL}, where a pre-defined grid is used.
#' @slot method String that indicates which method is used for the estimation of the angular dependence function. Must either be \code{"hill"}, to use the Hill estimator \insertCite{Hill1975}{ReturnCurves}, or \code{"cl"} to use the smooth estimator based on Bernstein-Bezier polynomials estimated by composite maximum likelihood.
#' @slot q \loadmathjax{} Marginal quantile used to define the threshold \mjeqn{u_\omega}{} of the min-projection variable \mjeqn{T^1}{} at ray \mjeqn{\omega}{} \mjeqn{\left(t^1_\omega = t_\omega - u_\omega | t_\omega > u_\omega\right)}{}, and/or Hill estimator \insertCite{Hill1975}{ReturnCurves}. Default is \code{0.95}.
#' @slot qalphas A vector containing the marginal quantiles used for the Heffernan and Tawn conditional extremes model \insertCite{HeffernanTawn2004}{ReturnCurves} for each variable, if \code{constrained = TRUE}. Default is \code{rep(0.95, 2)}.
#' @slot k Polynomial degree for the Bernstein-Bezier polynomials used for the estimation of the angular dependence function with the composite likelihood method \insertCite{MurphyBarltropetal2024}{ReturnCurves}. Default is \code{7}.
#' @slot constrained Logical. If \code{FALSE} (Default) no knowledge of the conditional extremes parameters is incorporated in the angular dependence function estimation. 
#' @slot tol Convergence tolerance for the composite maximum likelihood procedure. Success is declared when the difference of log-likelihood values between iterations does not exceed this value. Default is \code{0.0001}.
#' @slot par_init Initial values for the parameters \mjeqn{\beta}{} of the Bernstein-Bezier polynomials used for estimation of the angular dependence function with the composite likelihood method \insertCite{MurphyBarltropetal2024}{ReturnCurves}. Default is \code{rep(0, k-1)}.
#' @slot interval Maximum likelihood estimates \mjeqn{\hat{\alpha}^1_{x\mid y}}{} and \mjeqn{\hat{\alpha}^1_{y\mid x}}{} from the conditional extremes model if \code{constrained = TRUE}.
#' @slot adf A vector containing the estimates of the angular dependence function.
#' 
#' @references \insertAllCited{}
#' 
#' @keywords internal
adf_est.class <- function(dataexp, w, method, q, qalphas, k, constrained, tol, par_init, interval, adf){
  .adf_est.class(dataexp = dataexp,
                 w = w,
                 method = method,
                 q = q,
                 qalphas = qalphas,
                 k = k,
                 constrained = constrained,
                 tol = tol,
                 par_init = par_init,
                 interval = interval,
                 adf = adf)
}

#' Visualisation of the Angular Dependence Function estimates
#'
#' @description Plot method for an S4 object returned by \code{\link{adf_est}}. 
#'
#' @docType methods
#'
#' @param x An instance of an S4 class produced by \code{\link{adf_est}}.
#' 
#' @return A ggplot object showing a comparison between the Angular Dependence Function (ADF) estimates and the lower bound max\mjeqn{\lbrace \omega, 1-\omega\rbrace}{}.
#'
#' @rdname plotadfest
#'
#' @aliases plot,adf_est.class
#' 
#' @keywords internal
setMethod("plot", signature = list("adf_est.class"), function(x){
  w <- lb <- adf <- NULL # NULL them out to satisfy CRAN checks
  df <- data.frame("w" = x@w, "lb" = pmax(x@w, 1-x@w), "adf" = x@adf)
  coloursl <- c("Lower bound" = 1, "ADF estimates" = 2)
  ggplot(data = df, aes(x = w, y = lb, col = names(coloursl)[1])) + geom_line(linetype = "dashed") +
    geom_line(aes(x = w, y = adf, col = names(coloursl)[2])) +
    labs(x = expression(omega), y = expression(lambda(omega))) + 
    scale_color_manual(values = coloursl, 
                       guide = guide_legend(override.aes = list(linetype = c("solid", "dashed")))) +
    theme_minimal() + theme(legend.title = element_blank()) +
    ggtitle(expression("Estimation of" ~ hat(lambda)(omega)))
})

#' Estimation of the Angular Dependence Function (ADF)
#' 
#' @name adf_est
#' 
#' @description \loadmathjax{}
#' Estimation of the angular dependence function \mjeqn{\lambda(\omega)}{} introduced by \insertCite{WadsworthTawn2013;textual}{ReturnCurves}.
#' 
#' @docType methods
#' 
#' @param margdata An S4 object of class \code{margtransf.class}. See \code{\link{margtransf}} for more details. 
#' @param w Sequence of rays between \code{0} and \code{1}. Default is \code{NULL}, where a pre-defined grid is used.
#' @param method String that indicates which method is used for the estimation of the angular dependence function. Must either be \code{"hill"}, to use the Hill estimator \insertCite{Hill1975}{ReturnCurves}, or \code{"cl"} to use the smooth estimator based on Bernstein-Bezier polynomials estimated by composite maximum likelihood.
#' @param q Marginal quantile used to define the threshold \mjeqn{u_\omega}{} of the min-projection variable \mjeqn{T^1}{} at ray \mjeqn{\omega}{} \mjeqn{\left(t^1_\omega = t_\omega - u_\omega | t_\omega > u_\omega\right)}{}, and/or Hill estimator \insertCite{Hill1975}{ReturnCurves}. Default is \code{0.95}.
#' @param qalphas A vector containing the marginal quantile used for the Heffernan and Tawn conditional extremes model \insertCite{HeffernanTawn2004}{ReturnCurves} for each variable, if \code{constrained = TRUE}. Default is \code{rep(0.95, 2)}.
#' @param k Polynomial degree for the Bernstein-Bezier polynomials used for the estimation of the angular dependence function with the composite likelihood method \insertCite{MurphyBarltropetal2024}{ReturnCurves}. Default is \code{7}.
#' @param constrained Logical. If \code{FALSE} (default) no knowledge of the conditional extremes parameters is incorporated in the angular dependence function estimation. 
#' @param tol Convergence tolerance for the composite maximum likelihood procedure. Success is declared when the difference of log-likelihood values between iterations does not exceed this value. Default is \code{0.0001}.
#' @param par_init Initial values for the parameters \mjeqn{\beta}{} of the Bernstein-Bezier polynomials used for estimation of the angular dependence function with the composite likelihood method \insertCite{MurphyBarltropetal2024}{ReturnCurves}. Default is \code{rep(0, k-1)}.
#' 
#' @return An object of S4 class \code{adf_est.class}. This object returns the arguments of the function and two extra slots: 
#' \item{\code{interval}:}{A vector containing the maximum likelihood estimates from the conditional extremes model, \mjeqn{\hat{\alpha}^1_{x\mid y}}{} and \mjeqn{\hat{\alpha}^1_{y\mid x}}{}, if \code{constrained = TRUE}. If \code{constrained = FALSE}, then \code{c(0, 1)} is returned; we note that this has no meaningful interpretation as the estimation is performed in an unconstrained interval.}
#' \item{\code{adf}:}{A vector containing the estimates of the angular dependence function.}
#' 
#' @details The angular dependence function \mjeqn{\lambda(\omega)}{} can be estimated through a pointwise estimator, obtained with the Hill estimator, or via a smoother approach, obtained using Bernstein-Bezier polynomials and estimated via composite likelihood methods. 
#' 
#' Knowledge of the conditional extremes framework introduced by \insertCite{HeffernanTawn2004;textual}{ReturnCurves} can be incorporated by setting \code{constrained = TRUE}.
#' Let \mjeqn{\alpha^1_{x\mid y}=\alpha_{x\mid y} / (1+\alpha_{x\mid y})}{} and \mjeqn{\alpha^1_{y\mid x}=1 /(1+\alpha_{y\mid x})}{} with \mjeqn{\alpha_{x\mid y}}{} and \mjeqn{\alpha_{y\mid x}}{} 
#' being the conditional extremes parameters. After obtaining \mjeqn{\hat{\alpha}_{x\mid y}}{} and \mjeqn{\hat{\alpha}_{y\mid x}}{} via maximum likelihood estimation, 
#' \mjeqn{\lambda(\omega)=\max\lbrace \omega, 1-\omega\rbrace}{} for \mjeqn{\omega \in [0, \hat{\alpha}^1_{x\mid y})\cup (\hat{\alpha}^1_{y\mid x}, 1]}{} and    
#' is estimated as before for \mjeqn{\omega \in [\hat{\alpha}^1_{x\mid y},\hat{\alpha}^1_{y\mid x}]}{}. For more details see \insertCite{MurphyBarltropetal2024;textual}{ReturnCurves}.
#' 
#' @rdname adfestimation
#' 
#' @references \insertAllCited{}
#' 
#' @aliases adf_est
#' 
#' @examples
#' library(ReturnCurves)
#'
#' data(airdata)
#' 
#' n <- dim(airdata)[1]
#' 
#' margdata <- margtransf(airdata)
#'
#' lambda <- adf_est(margdata = margdata, method = "hill")
#'
#' plot(lambda)
#' 
#' # To see the the S4 object's slots
#' str(lambda)
#' 
#' # To access the estimates of the ADF
#' lambda@@adf
#' 
#' # If constrained = T, the MLE estimates for the conditional extremes model
#' # can be accessed as
#' lambda@@interval
#'
#' @export
#' 
adf_est <- function(margdata, w = NULL, method = c("hill", "cl"), q = 0.95, qalphas = rep(0.95, 2), k = 7, constrained = FALSE, tol = 0.0001, par_init = rep(0, k-1)){
  if(!inherits(margdata, "margtransf.class")){
    stop("The margdata argument needs to be an object of class margtransf.class.")
  }
  data <- margdata@dataexp
  qmarg <- margdata@qmarg
  if(is.null(dim(data)) || dim(data)[2] > 2){
    stop("Estimation of the ADF is only implemented for a bivariate setting.")
  }
  if(q < 0 | q > 1 | any(qalphas < 0) | any(qalphas > 1)){
    stop("Marginal quantiles need to be in [0, 1].")
  }
  if(is.null(w)){
    if(method == "cl"){
      w <- seq(0, 1, by = 0.01)
    }
    else if(method == "hill"){
      w <- seq(0, 1, by = 0.001)
    }
    else{
      stop("Method to estimate the ADF not implemented.")
    }
  }
  if(any(w < 0) | any(w > 1)){
    stop("Rays need to be in [0, 1].")
  }
  if(!method %in% c("hill", "cl")){
    stop("ADF needs to be estimated either through the Hill estimator or Composite likelihood estimator.")
  }
  if(q < max(qmarg) | any(qalphas < max(qmarg))){
    stop("Marginal quantiles need to be higher than the highest marginal quantile used for the marginal transformation.")
  }
  nas <- sum(is.na(data))
  if(nas > 0){
    warning(paste0("There are ", nas, " missing values in the data.\n These were removed."))
  }
  data <- data[complete.cases(data), ]
  result <- adf_est.class(dataexp = data, w = w, method = method, 
                          q = q, qalphas = qalphas, k = k, 
                          constrained = constrained, tol = tol, par_init = par_init, interval = double(), adf = double())
  
  if(constrained == FALSE){
    result@interval <- c(0, 1)
    if(method == "hill"){
      lambda_hill <- sapply(w, function(i) minproj_lambda(data, w = i, q_minproj = q)$lambdahill)
      lambda_hill <- properties(w = w, lambda = lambda_hill)
      result@adf <- lambda_hill
      return(result)
    }
    else{
      if(k < 1 | k %% 1 != 0){
        stop("The Bernstein-Bezier polynomial degree has to be a positive integer.")
      }
      if(length(par_init) != k - 1){
        stop(paste0("For Composite Likelihood estimation, the number of initial parameters should be equal to ", k - 1))
      }
      a <- 0
      b <- 1
      lam_end <- c(1, 1)
      basis <- bbp(w = w, k = k, a = a, b = b)$basis
      betacl <- minfunction_mle(w = w, data = data, a = a, b = b, lam_end = lam_end, k = k, q_minproj = q, tol = tol, par_init = par_init)
      lambda_cl <- basis %*% betacl
      lambda_cl <- properties(w = w, lambda = as.vector(lambda_cl))
      result@adf <- lambda_cl
      return(result)
    }
  }
  else{
    alphas <- heff_tawn_alphas(data = data, q = qalphas)
    a <- alphas[1]/(1 + alphas[1])
    b <- 1/(1 + alphas[2])
    result@interval <- c(a, b)
    indx <- w < a | w > b
    if(method == "hill"){
      lambda_hill <- c()
      if(sum(!indx) < 2){
        lambda_hill <- pmax(w, 1 - w)
        result@adf <- lambda_hill
        return(result)
      }
      lambda_hill[indx] <- pmax(w, 1 - w)[indx]
      lambda_hill[!indx] <- sapply(w[!indx], function(i) minproj_lambda(data, w = i, q_minproj = q)$lambdahill)
      lambda_hill <- properties(w = w, lambda = lambda_hill)
      result@adf <- lambda_hill
      return(result)
    }
    if(method == "cl"){
      if(k < 1 | k %% 1 != 0){
        stop("The Bernstein-Bezier polynomial degree has to be a positive integer.")
      }
      if(length(par_init) != k - 1){
        stop(paste0("For Composite Likelihood estimation, the number of initial parameters should be equal to ", k - 1))
      }
      lambda_cl <- c()
      if(sum(!indx) < 2){
        lambda_cl <- pmax(w, 1 - w)
        result@adf <- lambda_cl
        return(result)
      }
      else{
        lambda_cl[indx] <- pmax(w, 1 - w)[indx]
        basis <- bbp(w = w, k = k, a = a, b = b)$basis
        lam_end <- c(max(a, 1 - a), max(b, 1 - b))
        betacl <- minfunction_mle(w = w, data = data, a = a, b = b, lam_end = lam_end, k = k, q_minproj = q, tol = tol, par_init = par_init)
        lambda_cl[!indx] <- basis %*% betacl
        lambda_cl <- properties(w = w, lambda = as.vector(lambda_cl))
        result@adf <- lambda_cl
        return(result)
      }
    }
  }
}



