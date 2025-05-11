#' Predict Method for intsel fits
#' @description description
#' Obtains predictions from a fitted \code{intsel} object
#' @param object A fitted \code{intsel} object
#' @param newx Optional, a matrix in which to look for variables with which to predict. If ommitted, the original data is used.
#' @param type The type of prediction required. The default "\code{link}" is on the scale of the linear predictors; the alternative "response" is on the scale of the response variable.
#' @param ... Additional arguments passed to \code{\link{predict}}.
#' @return A matrix containing the prediction.
#' @rdname predict.intsel
#' @export
#' @examples
#' n <- 1000
#' p.int <- 5
#' p.noint <- 3
#' intercept <- TRUE
#' p.screen <- 5
#' 
#' p.int.expand <- p.int*(p.int-1)/2
#' p.main <- p.int + p.noint
#' x <- matrix(rnorm(n * p.main), nrow = n, ncol = p.main)
#' 
#' # true model
#' # logit(p) = 0.1 + 0.3 x1 + 0.3 x2 + 0.3 x8 + 0.2 * x1 * x2
#' 
#' beta.true <- rep(0, p.main)
#' beta.true[c(1, 2, p.main)] <- 0.3
#' eta <- x %*% beta.true + 0.2 * x[, 1] * x[, 2]
#' 
#' if (intercept) eta <- eta + 0.1
#' 
#' py <- 1/(1 + exp(-eta))
#' 
#' y <- rbinom(n, 1, py)
#' 
#' nlam <- 30
#' lambdas <- exp(seq(log(0.1), log(0.00005), length.out = nlam))
#' 
#' # All the pairwise two-way interactions for the first p.screen variables 
#' # are included in the model and screened in a data-driven manner.
#' fit <- intsel(x = x,
#'               y = y,
#'               p.screen = 5,
#'               intercept = intercept,
#'               lambda = lambdas)
#' str(predict(fit))

predict.intsel <- function(object, newx, type = "link", ...) {
  if (missing(newx)) {
    newx <- object$x
  } else {
    p.screen <- object$p.screen
    p.main <- ncol(object$x.original)
    if (ncol(newx) != p.main) {stop("Number of columns in newx is different from x used in intsel_cv.")}
    
    p <- p.main + p.screen*(p.screen - 1)/2
    
    two.way.intx <- unname(as.matrix(expand.grid(seq(p.screen), seq(p.screen))))
    two.way.intx <- two.way.intx[, 2:1]
    two.way.intx <- two.way.intx[two.way.intx[, 1] < two.way.intx[, 2], ]
    
    newx.int <- matrix(NA, nrow = nrow(newx), ncol = nrow(two.way.intx))
    for (i in seq(nrow(two.way.intx))) {
      newx.int[, i] <- newx[, two.way.intx[i, 1]] * newx[, two.way.intx[i, 2]]
    }
    
    newx <- cbind(newx, newx.int)
    if (object$intercept) {newx <- cbind(newx, 1)}
  }
  
  
  beta <- object$estimates
  
  if (type == "link") {
    pred <- newx %*% beta
  } else if (type == "response") {
    pred <- newx %*% beta
    pred <- 1/exp(-pred)
  } else {
    stop("Unsupported type.")
  }
  
  return(pred)
}