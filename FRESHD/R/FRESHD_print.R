#' @title Print Function for objects of Class FRESHD
#'
#' @description This function will print some information about the FRESHD object.
#'
#' @param x a FRESHD object
#' @param ... ignored
#'
 #' @examples
#' ##size of example
#' set.seed(42)
#' G <- 50; n <- c(65, 26, 13); p <- c(13, 5, 4)
#' sigma <-0.1
#' nlambda =30
#' ##marginal design matrices (Kronecker components)
#' x <- list()
#' for(i in 1:length(n)){x[[i]] <- matrix(rnorm(n[i] * p[i],0,sigma), n[i], p[i])}
#'
#' ##common features and effects
#' common_features <- rbinom(prod(p), 1, 0.1)
#' common_effects <- rnorm(prod(p), 0, 0.1) * common_features
#'
#' ##group response and fit
#' lambda <- exp(seq(0, -5, length.out = nlambda))
#' B <- array(NA, c(prod(p), nlambda, G))
#' y <- array(NA, c(n, G))
#' for(g in 1:G){
#' bg <- rnorm(prod(p), 0, 0.1) * (1 - common_features) + common_effects
#' Bg <- array(bg, p)
#' mu <- RH(x[[3]], RH(x[[2]], RH(x[[1]], Bg)))
#' y[,,, g] <- array(rnorm(prod(n), 0, var(mu)), dim = n) + mu
#' }
#'
#' ##fit model for range of lambda
#' system.time(fit <- maximin(y, x, penalty = "lasso", alg = "tos"))
#' Betahat <- fit$coef
#'
#' ##estimated common effects for specific lambda
#' modelno <- 20;
#' m <- min(Betahat[, modelno], common_effects)
#' M <- max(Betahat[, modelno], common_effects)
#' plot(common_effects, type = "h", ylim = c(m, M), col = "red")
#' lines(Betahat[, modelno], type = "h")
#'
#' @details
#'  A three-column data.frame with columns `sparsity`, `Df` and `lambda`.
#'  The `Df` column is the number of nonzero coefficients and `sparsity` is the 
#'  percentage   of zeros in the solution.
#' @return The data.frame above is silently returned
#' @method print FRESHD
#' @author Adam Lund
#' @export

print.FRESHD <- function(x, ...) {
out <- data.frame(sparsity = x$sparsity, Df = x$df, lambda = x$lambda)
print(out)
}
