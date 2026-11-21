#' Kernel Independent Component Analysis
#'
#' The kernel ICA method by Bach and Jordan (see references).
#' The contrast function was written in C++ under use of the Eigen3 library for computational speed.
#' The package ManifoldOptim is utilized for minimization of the contrast function on the Stiefel manifold.
#'
#' @param x A numeric matrix, where each column contains the measurements of a mixed data source.
#' @param variant Either \code{"kcca"} or \code{"kgv"}.
#' @param kernel Either \code{"gauss"} or \code{"hermite"}.
#' @param nstarts The number of restarts of the kernel ICA method with a default value of one. Ignored, if the starting values
#' in parameter \code{init} are set manually.
#' @param eps Numeric precision parameter for the approximation of the kernel matrices.
#' @param sigma Numeric value of the kernel variance. Default value is 1 for a given x with less than 1000 rows, otherwise 0.5.
#' @param kappa Numeric dimming parameter. Default value is \code{2e^-2} for a given x with less than 1000 rows, otherwise \code{2e^-3}.
#' @param hermite_rank Integer. Rank of the hermite polynomial with a default value of 3. Ignored, when \code{kernel} was set to \code{"gauss"}.
#' @param init A list of \eqn{p \times p} orthogonal matrices, which are the starting points for the optimization in the Stiefel manifold. By default
#' a number of orthogonal matrices specified in parameter \code{nstarts} is generated.
#' @param solver_params An object returned from the method \code{ManifoldOptim::get.solver.params} which can be given several parameters for the optimization.
#' @param optim_method The optimization method used in the Stiefel manifold. Default value is \code{"RSD"}. This value is directly passed to \code{ManifoldOptim::manifold.optim}.
#'
#' @return A class of type \code{bss} containing the following values:
#' \describe{
#' \item{Xmu}{the restored mean values}
#' \item{S}{The unmixed data}
#' \item{W}{The unmixing matrix}
#' \item{cmin}{The smallest resulting contrast function value of all kernel ICA runs}
#' }
#'
#' @details
#' Several points need to be considered when using \code{kernel_ica}:
#' \itemize{
#' \item To comply with the notions of the JADE package, model \eqn{\boldsymbol{X} = \boldsymbol{S} \boldsymbol{A}'}
#' with a \eqn{n \times p} source matrix \eqn{\boldsymbol{S}} and a \eqn{p \times p} mixing matrix \eqn{\boldsymbol{A}} is assumed.
#' \item The returned unmixing matrix \eqn{\boldsymbol{W}} is found such that
#' \eqn{\boldsymbol{X} \boldsymbol{W}' = \boldsymbol{S} \boldsymbol{A}' \boldsymbol{W}'}  results in the desired independent data.
#' \item It is not possible to reconstruct the original order of the sources nor their sign.
#' \item The contrast function which is to be minimized can have several local optimal.
#' Therefore setting the \code{nstart} parameter to a larger value than one or
#' instead providing more matrices in \code{init} for several starts should be considered.
#' \item Kernel ICA is started for each element given in the list \code{init} separately
#' and returns the best result by the lowest resulting value of the contrast function.
#' }
#'
#' @references
#' Kernel ICA implementation in Matlab and C by F. Bach:\cr
#' \href{https://www.di.ens.fr/~fbach/kernel-ica/index.htm}{https://www.di.ens.fr/~fbach/kernel-ica/index.htm}
#'
#' Francis R. Bach, Michael I. Jordan\cr
#' \emph{Kernel independent component analysis}\cr
#' Journal of Machine Learning Research 2002\cr
#' \doi{10.1162/153244303768966085}
#'
#' Sean Martin, Andrew M. Raim, Wen Huang, Kofi P. Adragni\cr
#' \emph{ManifoldOptim: An R Interface to the ROPTLIB Library for Riemannian Manifold Optimization}\cr
#' Journal of Statistical Software 2020\cr
#' \doi{10.18637/jss.v093.i01}
#'
#' @seealso
#' \code{ManifoldOptim::manifold.optim}\cr
#' \code{ManifoldOptim::get.solver.params}
#'
#' @example examples/ex_kernel_ica.R
#'
#' @import Rcpp
#' @importFrom methods new
#' @importFrom stats cov
#' @export
kernel_ica <-
  function(x,
           variant = c("kgv", "kcca"),
           kernel = c("gauss", "hermite"),
           nstarts = 1,
           eps = 0.0001,
           sigma = ifelse(ncol(x) < 1000, 1, 0.5),
           kappa = ifelse(ncol(x) < 1000, 2e-2, 2e-3),
           hermite_rank = 3,
           init = MD_distant_matrices(p = ncol(x), n = nstarts),
           solver_params = ManifoldOptim::get.solver.params(),
           optim_method = "RSD")
  {
    variant <- match.arg(variant)
    if (!(variant %in% c("kgv", "kcca"))) {
      stop("variant argument not valid.")
    }
    
    kernel <- match.arg(kernel)
    if (!(kernel %in% c("gauss", "hermite"))) {
      stop("kernel argument not valid.")
    }
    
    # when a matrix is provided instead of a list with a matrix entry.
    if (is.matrix(init)) {
      init <- list(init)
    }
    # if the argument is neither a matrix nor a list
    else if (!(is.list(init))) {
      stop("invalid argument for init")
    }
    
    p <- ncol(x)
    for (i in seq_along(init)) {
      if (any(dim(init[[i]]) != c(p, p))) {
        stop("wrong dimension of input matrices.")
      }
    }
    
    x <- scale(x, center = TRUE, scale = FALSE)
    x_cov_eigen <- eigen(cov(x), symmetric = TRUE)
    
    whiten <-
      x_cov_eigen$vectors %*% diag(x_cov_eigen$values ^ (-1 / 2)) %*% t(x_cov_eigen$vectors)
    x_whitened <- x %*% whiten
    
    
    par1 = switch(variant,
                  "kcca" = "c",
                  "kgv" = "v")
    
    if (kernel == "gauss") {
      prob <-
        methods::new(mod$KernelICAProblem, x_whitened, eps, kappa, par1, sigma)
    } else{
      # in the case of the Hermite kernel, a different constructor with one additional parameter is called
      prob <-
        methods::new(mod$KernelICAProblem,
                     x_whitened,
                     eps,
                     kappa,
                     par1,
                     sigma,
                     hermite_rank)
    }
    
    
    mani_params <-
      ManifoldOptim::get.manifold.params()
    mani.defn <- ManifoldOptim::get.stiefel.defn(p, p)
    
    
    cmin <- Inf
    Wmin <- matrix(1, nrow = p, ncol = p)
    for (i in seq_along(init)) {
      res <- ManifoldOptim::manifold.optim(
        prob,
        mani.defn,
        method = optim_method,
        mani.params = mani_params,
        solver.params = solver_params,
        x0 = t(init[[i]])
      )
      if (res$fval < cmin) {
        Wmin <- matrix(res$xopt, p)
        cmin <- res$fval
      }
    }
    
    retVal <- list(
      Xmu = attributes(x)$`scaled:center` %*% Wmin,
      S = x %*% Wmin,
      W = t(whiten %*% Wmin),
      cmin = cmin
    )
    class(retVal) <- "bss"
    return(retVal)
  }
