#' @name maximin
#' @aliases FRESHD
#' @title Maximin signal estimation
#'
#' @description Efficient procedure for solving the maximin estimation problem 
#' with identical design across groups, see (Lund, 2022).
#'
#' @usage maximin(y,
#'         x,
#'         penalty = "lasso", 
#'         alg ="aradmm",
#'         kappa = 0.99,
#'         nlambda = 30,
#'         lambda_min_ratio = 1e-04,
#'         lambda = NULL,
#'         penalty_factor = NULL,
#'         standardize = TRUE,
#'         tol = 1e-05,
#'         maxiter = 1000,
#'         delta = 1,
#'         gamma = 1,
#'         eta = 0.1,
#'         aux_par = NULL)
#'
#' @param y Array of size \eqn{n_1 \times\cdots\times n_d \times G} containing
#' the response values.
#' @param x Either i) the design matrix, ii) a list containing the Kronecker
#' components (2 or 3) if  the design matrix has Kronecker structure or iii) a 
#' string indicating the name of the wavelet to use (see {\code{\link{wt}}} for options)
#' @param penalty string specifying the penalty type. Possible values are
#' \code{"lasso"}.
#' @param alg string specifying the optimization algorithm. Possible values are
#' \code{"admm", "aradmm", "tos", "tosacc"}.
#' @param kappa Strictly positive float controlling the maximum sparsity in the 
#' solution. Only used with  ADMM type algorithms. Should be between 0 and 1.
#' @param nlambda Positive integer giving the number of \code{lambda} values.
#' Used when lambda is not specified.
#' @param lambda_min_ratio strictly positive float giving the smallest value for
#'\code{lambda}, as a fraction of \eqn{\lambda_{max}}; the (data dependent) 
#' smallest value for which all coefficients are zero. Used when lambda is not 
#' specified.
#' @param lambda Sequence of strictly positive floats used as penalty parameters.
#' @param penalty_factor A vector of length \eqn{p} containing positive floats that are 
#' multiplied with each element in \code{lambda} to allow differential penalization 
#' on the coefficients. For tensor models an array of size \eqn{p_1 \times \cdots \times p_d}.
#' @param standardize Boolean indicating if response \code{y} should be scaled. 
#' Default is TRUE to avoid numerical problems. 
#' @param tol Strictly positive float controlling the convergence tolerance.
#' @param maxiter Positive integer giving the maximum number of iterations
#' allowed for each \code{lambda} value.
#' @param delta Positive float controlling the step size in the algorithm.
#' @param gamma Positive float controlling the relaxation parameter in the 
#' algorithm. Should be between 0 and 2.
#' @param eta Scaling parameter for the step size in the accelerated TOS algorithm. 
#' Should be between 0 and 1.
#' @param aux_par Auxiliary parameters for the algorithms. 
#' 
#' @details For \eqn{n} heterogeneous data points divided into \eqn{G} equal sized
#' groups with \eqn{m<n} data points in each, let \eqn{y_g=(y_{g,1},\ldots,y_{g,m})}
#' denote the vector  of observations in group \eqn{g}. For a \eqn{m\times p}
#' design matrix \eqn{X} consider the model
#' \deqn{y_g=Xb_g+\epsilon_g}
#' for \eqn{b_g} a random group specific coefficient vector and \eqn{\epsilon_g}
#' an error term, see Meinshausen and Buhlmann (2015). For the model above following 
#' Lund (2022) this package solves the maximin estimation problem
#' \deqn{\min_{\beta} -\hat V_g(\beta)) + \lambda\Vert\beta\Vert_1,\lambda \ge 0}
#' where
#' \deqn{\hat V_g(\beta):=\frac{1}{n}(2\beta^\top X^\top y_g - \beta^\top X^\top X\beta),}
#' is the empirical explained variance in group \eqn{g}. See \cite{Lund, 2022}
#' for more details and references.
#'
#' The package solves the problem using different algorithms depending on \eqn{X}:
#'
#' i) If \eqn{X} is orthogonal (e.g. the inverse wavelet transform) either
#' an ADMM algorithm (standard or relaxed) or an adaptive relaxed
#' ADMM (ARADMM) with auto tuned step size is used, see Xu et al (2017).
#'
#' ii) For general \eqn{X}, a three operator splitting (TOS) algorithm
#' is implemented, see Damek and Yin (2017). Note if  the design is 
#' tensor structured, \eqn{X = \bigotimes_{i=1}^d X_i} for \eqn{d\in\{1, 2,3\}}, 
#' the procedure  accepts a list containing the tensor components (matrices).
#' 
#' @return An object with S3 Class "FRESHD".
#' \item{spec}{A string indicating the array dimension (1, 2 or 3) and the penalty.}
#' \item{coef}{A \eqn{p_1\cdots p_d \times} \code{nlambda} matrix containing the
#' estimates of the model coefficients (\code{beta}) for each \code{lambda}-value.}
#' \item{lambda}{A vector containing the sequence of penalty values used in the
#' estimation procedure.}
# \item{Obj}{A matrix containing the objective values for each iteration and each model.}
#' \item{df}{The number of nonzero coefficients for each value of \code{lambda}.}
#' \item{dimcoef}{A vector giving the dimension of the model coefficient array
#' \eqn{\beta}.}
#' \item{dimobs}{A vector giving the dimension of the observation (response) array \code{Y}.}
#' \item{dim}{Integer indicating the dimension of of the array model. Equal to 1
#' for non array.}
#' \item{wf}{A string indicating the wavelet name if used.}
#' \item{diagnostics}{A list where item \code{iter} is a vector containing  the 
#' number of iterations for each \code{lambda} value for which the algorithm 
#' converged. Item \code{stop_maxiter} is 1 if maximum iterations is reached 
#' otherwise zero. Item \code{stop_sparse} is 1 if  maximum sparsity is reached 
#' otherwise zero.}
# \item{endmod}{Vector of length \code{length(zeta)} with the number of
# models fitted for each \code{zeta}.}
#' 
#'
#' @author Adam Lund
#'
#' Maintainer: Adam Lund, \email{adam.lund@@math.ku.dk}
#'
#' @references
#' Lund, Adam (2022). Fast Robust Signal Estimation for Heterogeneous data.
#' \emph{In preparation}.
#'
#' Meinshausen, N and P. B{u}hlmann (2015). Maximin effects in inhomogeneous large-scale data.
#' \emph{The Annals of Statistics}. 43, 4, 1801-1830.
#'
#' Davis, Damek and Yin, Wotao, (2017). A three-operator splitting scheme and its
#' optimization applications. \emph{Set-valued and variational analysis}. 25, 4,
#' 829-858.
#'
#' Xu, Zheng and Figueiredo, Mario AT and Yuan, Xiaoming and Studer, Christoph and Goldstein, Tom
#' (2017). Adaptive relaxed admm: Convergence theory and practical implementation.
#' \emph{Proceedings of the IEEE Conference on Computer Vision and Pattern Recognition}
#' 7389-7398.
#'
#' @keywords package
#'
#' @examples
#' ## general 3d tensor design matrix
#' set.seed(42)
#' G <- 20; n <- c(65, 26, 13)*3; p <- c(13, 5, 4)*3
#' sigma <- 1
#'
#' ##marginal design matrices (Kronecker components)
#' x <- list()
#' for(i in 1:length(n)){x[[i]] <- matrix(rnorm(n[i] * p[i], 0, sigma), n[i], p[i])}
#'
#' ##common features and effects
#' common_features <- rbinom(prod(p), 1, 0.1)
#' common_effects <- rnorm(prod(p), 0, 0.1) * common_features
#'
#' ##simulate group response
#' y <- array(NA, c(n, G))
#' for(g in 1:G){
#' bg <- rnorm(prod(p), 0, 0.1) * (1 - common_features) + common_effects
#' Bg <- array(bg, p)
#' mu <- RH(x[[3]], RH(x[[2]], RH(x[[1]], Bg)))
#' y[,,, g] <- array(rnorm(prod(n), 0, var(mu)), dim = n) + mu
#' }
#'
#' ##fit model for range of lambda
#' system.time(fit <- maximin(y, x, penalty = "lasso", alg = "tosacc"))
#'
#' ##estimated common effects for specific lambda
#' modelno <- 10
#' betafit <- fit$coef[, modelno]
#' plot(common_effects, type = "h", ylim = range(betafit, common_effects), col = "red")
#' lines(betafit, type = "h")
#'
#' ##size of example
#' set.seed(42)
#' G <- 50; p <- n <- c(2^6, 2^5, 2^6);
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
#' y[,,, g] <- array(rnorm(prod(n),0, 0.5), dim = n) + mu
#' }
#'
#' ##orthogonal wavelet design with 1d data
#' G = 50; N1 = 2^10; p = 101; J = 2; amp = 20; sigma2 = 10
#' y <- matrix(0, N1, G)
#' z <- seq(0, 2, length.out = N1)
#' sig <- cos(10 * pi * z) + 1.5 * sin(5 * pi * z)
#'
#' for (i in 1:G){
#' freqs <- sample(1:100, size = J, replace = TRUE)
#' y[, i] <- sig * 2 + rnorm(N1, sd = sqrt(sigma2))
#' for (j in 1:J){
#' y[, i] <- y[, i] + amp * sin(freqs[j] * pi * z + runif(1, -pi, pi))
#' }
#' }
#'
#' system.time(fit <- maximin(y, "la8", alg = "aradmm", kappa = 0.9))
#' mmy <- predict(fit, "la8")
#' plot(mmy[,1], type = "l")
#' lines(sig, col = "red")
#'
#' @export
#' @useDynLib FRESHD, .registration = TRUE
#' @importFrom Rcpp evalCpp
maximin <-function(y,
                   x,
                   penalty = "lasso", #ridge....
                   alg ="aradmm",
                   kappa = 0.99,
                   nlambda = 30,
                   lambda_min_ratio = 1e-04,
                   lambda = NULL,
                   penalty_factor = NULL,
                   standardize = TRUE,
                   tol = 1e-05,
                   maxiter = 1000,
                   delta = 1,
                   gamma = 1,
                   eta = 0.1,
                   aux_par = NULL){
  
default_aux_par = list("stopcond" = "fpr", "orthval" = 0.2, "gamma0" = 1.5,
                       "gmh" = 1.9, "gmg" = 1.1, "minval" = 1e-10, "epsiloncor" = 0.2,
                       "Tf" = 2)
for(k in names(default_aux_par)){
if(is.null(aux_par[[k]])){
aux_par[[k]] = default_aux_par[[k]]
}
}

#todo: need algo design consistency check
if(!(alg %in% c("admm", "aradmm", "tos" , "tosacc"))){
stop(paste("algorithm must be correctly specified"))
}

tauk = 1 / delta
gamk = gamma
wf = "not used"
J = 0
if(standardize){
vary <- var(c(y))
y <- y / vary
if(!is.null(lambda)){lambda <- lambda / vary}
}else{vary = 1}

dimdata <- length(dim(y)) - 1
if (dimdata > 3){
  stop(paste("the dimension of the model must be 1, 2 or 3!"))
  }
  
G <- dim(y)[dimdata + 1]
if(is.character(x)){# wavelet

if(mean(round(log(dim(y)[1:dimdata], 2)) == log(dim(y)[1:dimdata], 2)) != 1){
  stop("data is not dyadic!")
}
wf = x

if(!(wf %in% c("haar", "d4", "??","mb4","fk4","d6","fk6", "d8","fk8", "la8","mb8",
               "bl14","fk14", "d16","la16","mb16", "la20","bl20","fk22", "mb24"))){
stop(paste("x (wavelet) not correctly specified"))
}

if(!(alg %in% c("admm", "aradmm"))){#check orthogonality
warning(paste("Note alg is changed from", alg, "to aradmm as x is a wavelet design"))
alg = "aradmm"
}

rm(x)
x <- list()
p1  <- n1  <- dim(y)[1]
p2 <- n2  <- 1
p3  <- n3  <- 1
if(dimdata == 1){
J= log(p1, 2)
}else if(dimdata == 2){
p2  <- n2  <- dim(y)[2]
J  <- log(min(p1, p2), 2)
}else{
p2  <- n2  <- dim(y)[2]
p3  <- n3  <- dim(y)[3]
J  <- log(min(p1, p2, p3), 2)
}
p <- n <- n1 * n2 * n3

x[[1]] <- matrix(n1, 1, 1)#not used
x[[2]] <- matrix(n2, 1, 1)#not used
x[[3]] <- matrix(n3, 1, 1)#not used

dims = matrix(c(n1, n2, n3, p1, p2, p3), 3, 2)

}else{#general/custom design

if(alg %in% c("admm", "aradmm")){
warning(paste("alg =", alg, "and x is not know to be orthognal. The", alg,  "algorithm has unknown behavior if the design x is not orthogonal"))
}
    
if(dimdata == 1){
if(is.matrix(x)){
x <- list(x, matrix(1, 1, 1), matrix(1, 1, 1))
}else{
x <- list(x[[1]], matrix(1, 1, 1), matrix(1, 1, 1))
}
}else if(dimdata == 2){
x <- list(x[[1]], x[[2]], matrix(1, 1, 1))
}
dims <- rbind(dim(x[[1]]), dim(x[[2]]), dim(x[[3]]))
n1 <- dims[1, 1]
n2 <- dims[2, 1]
n3 <- dims[3, 1]
p1 <- dims[1, 2]
p2 <- dims[2, 2]
p3 <- dims[3, 2]
n <- prod(dims[,1])
p <- prod(dims[,2])

}
y <- array(y, c(n1, n2 * n3, G))
if(is.null(lambda)){

makelamb <- 1
lambda <- rep(NA, nlambda)

}else{
nlambda<-length(lambda)
makelamb <- 0

}

if(is.null(penalty_factor)){

penalty_factor <- matrix(1, p1, p2 * p3)

}else if(prod(dim(penalty_factor)) != p){

stop(
paste("number of elements in penalty_factor (", length(penalty_factor),") is not equal to the number of coefficients (", p,")", sep = "")
)

}else {

if(min(penalty_factor) < 0){stop(paste("penalty_factor must be positive"))}

penalty_factor <- matrix(penalty_factor, p1, p2 * p3)

}

res <- solveMMP(dims,
                x[[1]], x[[2]], x[[3]],
                y,
                penalty,
                kappa,
                lambda,
                nlambda, makelamb, lambda_min_ratio, penalty_factor,
                tol,
                maxiter,
                alg,
                aux_par$"stopcond",#tos
                aux_par$"orthval",
                aux_par$"gamma0",
                aux_par$"gmh",
                aux_par$"gmg",
                aux_par$"minval",
                aux_par$"epsiloncor",
                aux_par$"Tf",
                wf,
                J,
                dimdata,
                tauk,
                gamk,
                eta)

endmodelno <- res$endmodelno + 1 #converged models since c++ is zero indexed
Iter <- res$Iter

maxiterpossible <- sum(Iter > 0)
maxiterreached <- sum(Iter >= (maxiter - 1))

if(maxiterreached > 0){

warning(paste("maximum number of inner iterations (",maxiter,") reached ",
maxiterreached," time(s) out of ", maxiterpossible," possible"))

}
endmodelno <- res$endmodelno + 0#converged models since c++ is zero indexed

if(alg %in% c("admm", "aradmm")){
  modelseq <- endmodelno:1
}else{
    modelseq <- 1:endmodelno
    }
out <- list()
class(out) <- "FRESHD"
out$spec <- paste("", dimdata,"-dimensional ", penalty," penalized model")
out$coef <- as.matrix(res$coefs[ , modelseq]) * vary
out$lambda <- res$lambda[modelseq] * vary
out$df <- res$df[modelseq]
out$sparsity <- 1 - res$df[modelseq] / p
out$dimcoef <- c(p1, p2, p3)[1:dimdata]
out$dimobs <- c(n1, n2, n3)[1:dimdata]
out$dim = dimdata
out$wf = wf
out$endmod <- endmodelno
out$diagnostics <- list(iter = Iter[modelseq], stop_maxiter = res$Stops[1],
                        stop_sparse = res$Stops[2])
#out$tauk <- res$tauk
#out$gammak <- res$gamk
#out$PhitY<-res$PhitY
return(out)

}

