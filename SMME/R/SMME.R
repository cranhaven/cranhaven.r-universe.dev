#' @name SMME
#' @aliases pga
#' @title Soft Maximin Estimation for Large Scale Heterogenous Data
#'
#' @description  Efficient procedure for solving the Lasso or SCAD penalized soft
#' maximin problem for large scale_y data. This software implements two proximal
#' gradient based algorithms (NPG and FISTA) to solve different forms of the soft
#' maximin problem from \cite{Lund et al., 2022}. 1) For general group specific
#' design the soft maximin problem is solved using the NPG algorithm.
#' 2) For fixed identical d-array-tensor design across groups, where \eqn{d = 1, 2, 3}, the
#' estimation procedure uses either the FISTA algorithm or the NPG algorithm and
#' is implemented for the following two cases; i) For a tensor design matrix the
#' algorithms use array arithmetic to speed up design matrix multiplications
#' using only the tensor components ii) For a wavelet design matrix the algorithms use
#' the pyramid algorithm to completely avoid the design matrix and speed up
#' design matrix multiplications.
#' Multi-threading is possible when openMP is available for R.
#'
#' Note this package SMME replaces the SMMA package.
#' @usage  softmaximin(x,
#'             y,
#'             zeta,
#'             penalty = c("lasso", "scad"),
#'             alg = c("npg", "fista"),
#'             nlambda = 30,
#'             lambda.min.ratio = 1e-04,
#'             lambda = NULL,
#'             scale_y = 1,
#'             penalty.factor = NULL,
#'             reltol = 1e-05,
#'             maxiter = 1000,
#'             steps = 1,
#'             btmax = 100,
#'             c = 0.0001,
#'             tau = 2,
#'             M = 4,
#'             nu = 1,
#'             Lmin = 0,
#'             lse = TRUE,
#'             nthreads = 2)
#'
#' @param x Either a list containing the G group specific design matrices of sizes
#' \eqn{n_i \times p_i} (general model),  a list containing the \eqn{d}
#' (\eqn{d \in \{ 1, 2, 3\}}) tensor components (tensor array model) or a string
#' indicating which wavelet design to use (wavelet array model), see \link{wt}
#' for options.
#' @param y list containing the G group specific response vectors of sizes
#' \eqn{n_i \times 1}. Alternatively for a model with identical tensor design
#' across G groups, \code{y}is an array of size \eqn{n_1 \times\cdots\times n_d \times G}
#' (\eqn{d \in \{ 1, 2, 3\}}) containing the response values.
#' @param zeta vector of strictly positive floats controlling  the softmaximin
#' approximation accuracy. When \code{length(zeta) > 1} the procedure will distribute
#' the computations using the \code{nthreads} parameter below when openMP is available.
#' @param penalty string specifying the penalty type. Possible values are
#' \code{"lasso", "scad"}.
#' @param alg string specifying the optimization algorithm. Possible values are
#' \code{"npg", "fista"}.
#' @param nlambda positive integer giving the number of \code{lambda} values.
#' Used when lambda is not specified.
#' @param lambda.min.ratio strictly positive float giving the smallest value for
#' \code{lambda}, as a fraction of \eqn{\lambda_{max}}; the (data dependent)
#' smallest value for which all coefficients are zero. Used when lambda is not
#' specified.
#' @param lambda A sequence of strictly positive floats used  as penalty parameters.
#' @param scale_y strictly positive number that the response \code{y} is multiplied with.
#' @param penalty.factor a length \eqn{p} vector of positive floats that are
#' multiplied with each element in \code{lambda} to allow differential penalization
#' on the coefficients. For tensor models an array of size \eqn{p_1 \times \cdots \times p_d}.
#' @param reltol strictly positive float giving the convergence tolerance.
#' @param maxiter positive integer giving the maximum number of  iterations
#' allowed for each \code{lambda} value.
#' @param steps strictly positive integer giving the number of steps used in the
#' multi-step adaptive lasso algorithm for non-convex penalties. Automatically
#' set to 1 when \code{penalty = "lasso"}.
#' @param btmax strictly positive integer giving the maximum number of backtracking
#' steps allowed in each iteration. Default is \code{btmax = 100}.
#' @param c strictly positive float used in the NPG algorithm. Default is
#' \code{c = 0.0001}.
#' @param tau strictly positive float used to control the stepsize for NPG.
#' Default is \code{tau = 2}.
#' @param M positive integer giving the look back for the NPG. Default is \code{M = 4}.
#' @param nu strictly positive float used to control the stepsize. A  value less
#' that 1 will decrease the stepsize and a value larger than one will increase it.
#' Default is \code{nu = 1}.
#' @param Lmin non-negative float used by the NPG algorithm to control the
#' stepsize. For the default  \code{Lmin = 0} the maximum step size is the same
#' as for the FISTA algorithm.
#' @param lse logical variable indicating whether to use the log-sum-exp-loss.  TRUE is
#' default and yields the loss below and  FALSE yields the exponential of this.
#' @param nthreads integer giving the number of threads to use when  openMP
#' is available. Default is 2.
#' @details Consider modeling heterogeneous data \eqn{y_1,\ldots, y_n} by dividing
#' it into \eqn{G} groups \eqn{\mathbf{y}_g = (y_1, \ldots, y_{n_g})},
#' \eqn{g \in \{ 1,\ldots, G\}} and then using a linear model
#' \deqn{
#' \mathbf{y}_g = \mathbf{X}_gb_g + \epsilon_g, \quad g \in \{1,\ldots, G\},
#' }
#' to model the group response. Then \eqn{b_g} is a group specific \eqn{p\times 1}
#' coefficient, \eqn{\mathbf{X}_g} an \eqn{n_g\times p} group design matrix and
#' \eqn{\epsilon_g} an \eqn{n_g\times 1} error term. The objective is to estimate
#' a common coefficient \eqn{\beta} such that \eqn{\mathbf{X}_g\beta} is a robust
#' and good approximation to \eqn{\mathbf{X}_gb_g} across groups.
#'
#' Following \cite{Lund et al., 2022}, this objective may be accomplished by
#' solving the soft maximin estimation problem
#' \deqn{
#' \min_{\beta}\frac{1}{\zeta}\log\bigg(\sum_{g = 1}^G \exp(-\zeta \hat V_g(\beta))\bigg)
#'  + \lambda  \Vert\beta\Vert_1, \quad \zeta > 0,\lambda \geq 0.
#' }
#' Here \eqn{\zeta} essentially controls the amount of pooling across groups
#' (\eqn{\zeta \sim 0} effectively ignores grouping and pools observations) and
#' \deqn{
#' \hat V_g(\beta):=\frac{1}{n_g}(2\beta^\top \mathbf{X}_g^\top
#' \mathbf{y}_g-\beta^\top \mathbf{X}_g^\top \mathbf{X}_g\beta),
#' }
#' is the empirical explained variance, see \cite{Lund et al., 2022} for more
#' details and references.
#'
#' The function \code{softmaximin} solves the soft maximin estimation problem in
#' large scale settings for a sequence of penalty parameters
#' \eqn{\lambda_{max}>\ldots >\lambda_{min}>0} and a sequence of strictly positive
#' softmaximin  parameters \eqn{\zeta_1, \zeta_2,\ldots}.
#'
#' The implementation also solves the
#' problem above with the penalty given by the SCAD penalty, using the multiple
#' step adaptive lasso procedure to loop over the inner proximal algorithm.
#'
#' Two optimization algorithms  are implemented in the SMME packages;
#' a non-monotone proximal gradient (NPG) algorithm and a fast iterative soft
#' thresholding algorithm (FISTA).
#'
#' The implementation is particularly efficient for models where the design is
#' identical across groups i.e. \eqn{\mathbf{X}_g = \mathbf{X}}
#' \eqn{\forall g \in \{1, \ldots, G\}} in the following two cases:
#' i) first if \eqn{\mathbf{X}} has tensor structure i.e.
#' \deqn{
#' \mathbf{X} = \bigotimes_{i=1}^d \mathbf{M}_i
#' }
#' for marginal \eqn{n_i\times p_i} design matrices \eqn{\mathbf{M}_1,\ldots, \mathbf{M}_d}
#' , \eqn{d \in \{ 1, 2, 3\}}, \code{y} is a \eqn{d + 1} dimensional response array
#' and  \code{x} is a list containing the \eqn{d} marginal matrices
#' \eqn{\mathbf{M}_1,\ldots, \mathbf{M}_d}. In this case \code{softmaximin} solves
#' the soft maximin problem using minimal memory by way of tensor optimized
#' arithmetic, see also \code{\link{RH}}.
#' ii) second, if the design matrix \eqn{\mathbf{X}} is the inverse matrix of an
#' orthogonal wavelet transform \code{softmaximin}  solves the soft maximin problem
#'  given the \eqn{d + 1} dimensional response array \code{y} and
#'    \code{x} the name of the wavelet family \code{\link{wt}},  using the
#'    pyramid algorithm to compute multiplications
#'  involving \eqn{\mathbf{X}}.
#'
#' Note that when multiple values for \eqn{\zeta} is provided it is  possible to
#' distribute the computations across CPUs if openMP is available.
#'
#' @return An object with S3 Class "SMME".
#' \item{spec}{A string indicating the array dimension (1, 2 or 3) and the penalty.}
#' \item{coef}{A  \code{length(zeta)}-list of \eqn{p \times} \code{nlambda}
#' matrices containing the estimates of the model coefficients (\eqn{\beta}) for
#' each \code{lambda}-value for which the procedure converged.}
#' \item{lambda}{A \code{length(zeta)}-list vectors containing the sequence of
#' penalty values used in the estimation procedure for which the procedure converged.}
# \item{Obj}{A \code{length(zeta) > 1} a \code{length(zeta)}-list  matrices
# containing the objective values for each iteration and each model for which
# the procedure converged.}
#' \item{df}{A  \code{length(zeta)}-list of vectors indicating the nonzero model
#' coefficients for each value of \code{lambda} for which the procedure converged.}
#' \item{dimcoef}{An integer giving the number \eqn{p} of model parameters.
#' For array data a vector giving the dimension of the model coefficient array \eqn{\beta}.}
#' \item{dimobs}{An integer giving the number of observations. For array data a
#' vector giving the dimension of the observation (response) array \code{Y}.}
#' \item{dim}{Integer indicating the dimension of of the array model. Equal to 1
#' for non array.}
#' \item{wf}{A string indicating the wavelet name if used.}
#' \item{diagnostics}{A list of length 3. Item \code{iter} is a \code{length(zeta)}-list
#' of vectors containing  the number of   iterations for each \code{lambda} value
#' for which the algorithm converged. Item \code{bt_iter}  is a  \code{length(zeta)}
#' vector with total number of backtracking steps performed across all (converged)
#' \code{lambda} values for given  \code{zeta} value. Key \code{bt_enter} is a
#' \code{length(zeta)} vector with  total number of times backtracking is initiated
#' across all (converged)  \code{lambda} values for given \code{zeta} value.}
#' \item{endmod}{Vector of length \code{length(zeta)} with the number of
#' models fitted for each \code{zeta}.}
#' \item{Stops}{Convergence indicators.}
#' @author  Adam Lund
#'
#' Maintainer: Adam Lund, \email{adam.lund@@math.ku.dk}
#'
#' @references
#' Lund, A., S. W. Mogensen and N. R. Hansen (2022). Soft Maximin Estimation for
#' Heterogeneous Data. \emph{Scandinavian Journal of Statistics}, vol. 49, no. 4,
#' pp. 1761-1790.
#' url = {https://doi.org/10.1111/sjos.12580}
#'
#' @keywords package
#'
#' @examples
#' #Non-array data
#'
#' ##size of example
#' set.seed(42)
#' G <- 10; n <- sample(100:500, G); p <- 60
#' x <- y <- list()
#'
#' ##group design matrices
#' for(g in 1:G){x[[g]] <- matrix(rnorm(n[g] * p), n[g], p)}
#'
#' ##common features and effects
#' common_features <- rbinom(p, 1, 0.1) #sparsity of comm. feat.
#' common_effects <- rnorm(p) * common_features
#'
#' ##group response
#' for(g in 1:G){
#' bg <- rnorm(p, 0, 0.5) * (1 - common_features) + common_effects
#' mu <- x[[g]] %*% bg
#' y[[g]] <- rnorm(n[g]) + mu
#' }
#'
#' ##fit model for range of lambda and zeta
#' system.time(fit <- softmaximin(x, y, zeta = c(0.1, 1), penalty = "lasso", alg = "npg"))
#' betahat <- fit$coef
#'
#' ##estimated common effects for specific lambda and zeta
#' zetano <- 2
#' modelno <- dim(betahat[[zetano]])[2]
#' m <- min(betahat[[zetano]][ , modelno], common_effects)
#' M <- max(betahat[[zetano]][ , modelno], common_effects)
#' plot(common_effects, type = "p", ylim = c(m, M), col = "red")
#' lines(betahat[[zetano]][ , modelno], type = "h")
#'
#' #Array data
#' ##size of example
#' set.seed(42)
#' G <- 50; n <- c(30, 20, 10); p <- c(7, 5, 4)
#'
#' ##marginal design matrices (Kronecker components)
#' x <- list()
#' for(i in 1:length(n)){x[[i]] <- matrix(rnorm(n[i] * p[i]), n[i], p[i])}
#'
#' ##common features and effects
#' common_features <- rbinom(prod(p), 1, 0.1) #sparsity of comm. feat.
#' common_effects <- rnorm(prod(p),0,0.1) * common_features
#'
#' ##group response
#'  y <- array(NA, c(n, G))
#' for(g in 1:G){
#' bg <- rnorm(prod(p), 0, .1) * (1 - common_features) + common_effects
#' Bg <- array(bg, p)
#' mu <- RH(x[[3]], RH(x[[2]], RH(x[[1]], Bg)))
#' y[,,, g] <- array(rnorm(prod(n)), dim = n) + mu
#' }
#'
#' ##fit model for range of lambda and zeta
#' system.time(fit <- softmaximin(x, y, zeta = c(1, 10, 100), penalty = "lasso",
#'             alg = "npg"))
#' betahat <- fit$coef
#'
#' ##estimated common effects for specific lambda and zeta
#' zetano <- 1
#' modelno <- dim(betahat[[zetano]])[2]
#' m <- min(betahat[[zetano]][, modelno], common_effects)
#' M <- max(betahat[[zetano]][, modelno], common_effects)
#' plot(common_effects, type = "p", ylim = c(m, M), col = "red")
#' lines(betahat[[zetano]][ , modelno], type = "h")
#'
#' #Array data and wavelets
#' ##size of example
#' set.seed(42)
#' G <- 50; p <- n <- c(2^3, 2^4, 2^5);
#'
#' ##common features and effects
#' common_features <- rbinom(prod(p), 1, 0.1) #sparsity of comm. feat.
#' common_effects <- rnorm(prod(p), 0, 1) * common_features
#'
#' ##group response
#' y <- array(NA, c(n, G))
#' for(g in 1:G){
#' bg <- rnorm(prod(p), 0, 0.1) * (1 - common_features) + common_effects
#' Bg <- array(bg, p)
#' mu <- iwt(Bg)
#' y[,,, g] <- array(rnorm(prod(n), 0, 0.5), dim = n) + mu
#' }
#'
#' ##fit model for range of lambda and zeta
#' system.time(fit <- softmaximin(x = "la8", y, zeta = c(0.1, 1, 10),
#'                                 penalty = "lasso", alg = "fista"))
#' betahat <- fit$coef
#'
#' ##estimated common effects for specific lambda and zeta
#' zetano <- 3
#' modelno <- dim(betahat[[zetano]])[2]
#' m <- min(betahat[[zetano]][, modelno], common_effects)
#' M <- max(betahat[[zetano]][, modelno], common_effects)
#' plot(common_effects, type = "p", ylim = c(m, M), col = "red")
#' lines(betahat[[zetano]][ , modelno], type = "h")

#' @export
#' @useDynLib SMME, .registration = TRUE
#' @importFrom Rcpp evalCpp
softmaximin <- function(
               x,
               y,
               zeta,
               penalty = c("lasso", "scad"),
               alg = c("npg", "fista"),
               nlambda = 30,
               lambda.min.ratio = 1e-04,
               lambda = NULL,
               scale_y = 1,
               penalty.factor = NULL,
               reltol = 1e-05,
               maxiter = 1000,
               steps = 1,
               btmax = 100,
               c = 0.0001,
               tau = 2,
               M = 4,
               nu = 1,
               Lmin = 0,
               lse = TRUE,
               nthreads = 2){
wf = "not used"
wave = 0
J = 0
dimglam = 0
if(scale_y != 1){
  if(is.list(y)){y <- lapply(y, function(x) x * scale_y)
 }else{y <- y * scale_y}
 if(!is.null(lambda)){lambda <- lambda * scale_y}
}

if(sum(alg == c("npg", "fista")) != 1){
stop(paste("algorithm must be correctly specified"))
}

if(alg == "npg"){alg <- 1}else{alg <- 0}

if(lse == TRUE){ll <- 1}else{ll <- 0}

if(c <= 0){stop(paste("c must be strictly positive"))}

if(Lmin < 0){stop(paste("Lmin must be positive"))}

if(mean(zeta <= 0) > 0){stop(paste("all zetas must be strictly positive"))}

if(sum(penalty == c("lasso", "scad")) != 1){
stop(paste("penalty must be correctly specified"))
}

if(!is.null(penalty.factor)){
if(min(penalty.factor) < 0){stop(paste("penalty.factor must be positive"))}
}

if("list" %in% class(y)){##general
  Z <- lapply(y, as.matrix)
  rm(y)
  array = 0
  dimglam <- 0
  G = length(Z)
  n <- sum(sapply(Z, length))
  p <- dim(x[[1]])[2]
  alg = 1 #npg
  if(is.null(penalty.factor)){
    penalty.factor <- as.matrix(rep(1, p))
  }else{
    penalty.factor <- as.matrix(penalty.factor)

    }
  #check to make sure y is compaitble with x in every gropu...todo

  }else if("array" %in% class(y)){#array

array = 1

if(is.character(x)){# wavelet
wf = x
wave = 1
x<-list()
dimglam = length(dim(y)) - 1
if(mean(round(log(dim(y)[1:dimglam], 2)) == log(dim(y)[1:dimglam], 2)) != 1){
  stop("data is not dyadic!")
  }
if(dimglam == 1){
  p1 = n1 = dim(y)[1]
  p2 = n2 = 1
  p3 = n3 = 1
  J= log(p1, 2)
}else if(dimglam == 2){
  p1 = n1 = dim(y)[1]
p2 = n2 = dim(y)[2]
p3 = n3 = 1
J = log(min(p1, p2), 2)
}else{
  p1 = n1 = dim(y)[1]
  p2 = n2 = dim(y)[2]
  p3 = n3 = dim(y)[3]
  J=    log(min(p1, p2, p3), 2)
}
p <- n <- n1 * n2 * n3
   x[[1]] <- matrix(n1, 1, 1)
  x[[2]] <- matrix(n2, 1, 1)
  x[[3]] <- matrix(n3, 1, 1)
}else{ ##general array tensor design
dimglam <- length(x)
if(dimglam != length(dim(y)) - 1){stop(paste("x and y not compatible"))}
if (dimglam > 3){
stop(paste("the dimension of the model must be 1, 2 or 3!"))
}else if (dimglam == 1){
x[[2]] <- matrix(1, 1, 1)
x[[3]] <- matrix(1, 1, 1)
}else if (dimglam == 2){
x[[3]] <- matrix(1, 1, 1)
}
dimx <- rbind(dim(x[[1]]), dim(x[[2]]), dim(x[[3]]))
n1 <- dimx[1, 1]
n2 <- dimx[2, 1]
n3 <- dimx[3, 1]
p1 <- dimx[1, 2]
p2 <- dimx[2, 2]
p3 <- dimx[3, 2]
n <- prod(dimx[,1])
p <- prod(dimx[,2])

}
G <- dim(y)[length(dim(y))]

Z <- list()

for(i in 1:G){

  if(dimglam == 1){

    tmp <- matrix(y[,  i], n1, n2 * n3)

  }else if(dimglam == 2){

    tmp <- matrix(y[, ,  i], n1, n2 * n3)

  }else if(dimglam == 3){

    tmp <- matrix(y[, , , i], n1, n2 * n3)

  }

  Z[[i]] <- tmp

}

#if(is.null(penalty.factor)){penalty.factor <- matrix(1, p1, p2 * p3)}

if(is.null(penalty.factor)){

  penalty.factor <- matrix(1, p1, p2 * p3)

}else if(length(penalty.factor) != p){

  stop(
    paste("number of elements in penaltyfactor (", length(penalty.factor),") is not equal to the number of coefficients (", p,")", sep = "")
  )

}else {

  if(min(penalty.factor) < 0){stop(paste("penaltyfactor must be positive"))}

  penalty.factor <- matrix(penalty.factor, p1, p2 * p3)

}

}else{
  stop(paste("response y must be list or array"))
}

if(length(penalty.factor) != p){

stop(
paste("number of elements in penalty.factor (", length(penalty.factor),") is not equal to the number of coefficients (", p,")", sep = "")
)

}

if(penalty == "lasso"){steps <- 1}

if(is.null(lambda)){

makelamb <- 1
lambda <- rep(NA, nlambda)

}else{

makelamb <- 0
nlambda <- length(lambda)

}

res <- pga(x,
           Z,
           penalty,
           zeta,
           c,
           lambda, nlambda, makelamb, lambda.min.ratio,
           penalty.factor,
           reltol,
           maxiter,
           steps,
           btmax,
           M,
           tau,
           nu,
           alg,
           array,
           ll,
           Lmin,
           nthreads          ,
           wave,
           J,
           dimglam,
           wf)
endmodelno <- drop(res$endmodelno) #converged models since c++ is zero indexed

if(mean(res$Stops[2, ]) > 0){
zs <- which(res$Stops[2, ] != 0)

warning(paste("maximum number of inner iterations (",maxiter,") reached for model no.",
              paste(endmodelno[zs] + 1, collapse = " ")," for zeta(s)",
              paste(zeta[zs], collapse = " ")))

}

if(mean(res$Stops[3, ]) > 0){
  zs <- which(res$Stops[3, ] != 0)

warning(paste("maximum number of backtraking steps reached for model no.",
              paste(endmodelno[zs] + 1, collapse = " ")," for zeta(s)",
              paste(zeta[zs], collapse = " ")))
}

if(res$openMP == 1){message(paste("Multithreading was enabled using", nthreads, "threads"))}
out <- list()
class(out) <- "SMME"
if(array){
if(wave == 0){
spec <- paste(dimglam,"-dimensional", penalty," penalized array model with", G , "groups")
}else{
spec <- paste(dimglam,"-dimensional", penalty," penalized array model with", G , "groups")
}
}else{
spec <- paste(penalty," penalized linear model with", G , "groups")
}
Obj <- iter <- coef <- lambda <- df <- list()
for(z in 1:length(zeta)){
coef[[z]] <- as.matrix(res$Beta[ , 1:endmodelno[z], z]) / scale_y
lambda[[z]] <- res$lambda[1:endmodelno[z], z] / scale_y
df[[z]] <- res$df[1:endmodelno[z], z]
iter[[z]] <- res$Iter[1:endmodelno[z], z]
#Obj[[z]] <- res$Obj[, 1:endmodelno[z] ,1]
}

out$spec <- spec
out$coef <- coef
out$zeta <- zeta
out$lambda <- lambda
out$df <- df
#out$Obj <- Obj
if(array == 1){
out$dimcoef <- c(p1, p2, p3)[1:dimglam]
out$dimobs <- c(n1, n2, n3)[1:dimglam]
}else{
out$dimcoef <- p
out$dimobs <- n
}
out$dim = dimglam
out$wf = wf
out$endmod <- endmodelno
out$diagnostics <- list(iter = iter, bt_iter = res$btiter, bt_enter = res$btenter)
out$Stops <- res$Stops

return(out)

}

