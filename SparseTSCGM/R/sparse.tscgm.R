#' @import glasso
#' @import longitudinal
#' @import huge
#' @import MASS
#' @import mvtnorm
#' @import network
#' @import abind
#' @import stats
NULL
#' Sparse time series chain graphical models
#'
#' Computes sparse vector autoregressive coefficient matrices of order 1 and 2 
#' and precision matrix estimates for time series chain graphical models using 
#' SCAD or LASSO penalties. In time series chain graphs, \code{directed} edges 
#' are identified by nonzero entries of the autoregressive coefficients matrix 
#' and \code{undirected} edges are identified by nonzero entries of the precision matrix.
#'
#' @param data Longitudinal data format (matrix or data.frame).
#' @param lam1 Numeric. Scalar or vector of tuning parameter values for the precision matrix penalty. 
#'   If \code{NULL}, the program generates a sequence based on \code{nlambda}.
#' @param lam2 Numeric. Scalar or vector of tuning parameter values for the autoregression matrix penalty. 
#'   If \code{NULL}, a sequence is generated based on \code{nlambda}.
#' @param nlambda Integer. Number of tuning parameter values to generate if \code{lam1} or \code{lam2} are \code{NULL}. Default is 10.
#' @param model Character. Order of the vector autoregressive model. Choices: \code{"ar1"} or \code{"ar2"}.
#' @param penalty Character. Type of penalty to use. Choices: \code{"scad"} (default) or \code{"lasso"}.
#' @param optimality Character. Information criterion for model selection. Choices:
#'   \code{"NULL"} (no selection), \code{"bic"}, \code{"bic_ext"}, \code{"bic_mod"}, \code{"aic"}, \code{"gic"}.
#' @param control List. Control parameters for the algorithm:
#'   \describe{
#'     \item{maxit.out}{Maximum outer iterations (default 5).}
#'     \item{maxit.in}{Maximum inner iterations (default 50).}
#'     \item{tol.out}{Convergence tolerance (default 1e-4).}
#'     \item{silent}{TRUE/FALSE, whether to print progress messages (default TRUE).}
#'   }
#'
#' @details
#' For description of the objective functions and computational details, see Abegaz and Wit (2013).
#'
#' @return A list containing:
#'   \describe{
#'     \item{theta}{Estimated precision matrix. Nonzero entries represent undirected edges.}
#'     \item{gamma}{Estimated autoregressive coefficient matrix. Nonzero entries represent directed edges.}
#'     \item{lam1.opt}{Optimal tuning parameter for the precision matrix.}
#'     \item{lam2.opt}{Optimal tuning parameter for the autoregression matrix.}
#'     \item{min.ic}{Minimum value of the selected information criterion.}
#'     \item{tun.ic}{Matrix of tuning parameters and corresponding information criterion values.}
#'     \item{lam1.seq}{Sequence of precision matrix tuning parameters.}
#'     \item{lam2.seq}{Sequence of autoregression matrix tuning parameters.}
#'     \item{s.theta}{Sparsity level of the precision matrix.}
#'     \item{s.gamma}{Sparsity level of the autoregression matrix.}
#'   }
#'
#' @references
#' Fentaw Abegaz and Ernst Wit (2013). Sparse time series chain graphical models 
#' for reconstructing genetic networks. \emph{Biostatistics}, 14(3), 586–599.
#' 
#' Rothman, Levina, and Zhu (2010). Sparse multivariate regression with 
#' covariance estimation. \emph{Journal of Computational and Graphical Statistics}, 19, 947–962.
#'
#' @export
#'
#' @examples
#' seed <- 321
#' datas <- sim.data(model="ar1", time=10, n.obs=10, n.var=5, seed=seed, prob0=0.35, network="random")
#' data.fit <- datas$data1
#' prec_true <- datas$theta
#' autoR_true <- datas$gamma
#' 
#' res.tscgm <- sparse.tscgm(data=data.fit, lam1=NULL, lam2=NULL, nlambda=NULL, 
#'   model="ar1", penalty="scad", optimality="bic_mod", 
#'   control=list(maxit.out=10, maxit.in=100))
#' 
#' # Estimated sparse precision and autoregression matrices
#' prec <- res.tscgm$theta
#' autoR <- res.tscgm$gamma
#' 
#' # Optimal tuning parameter values
#' lambda1.opt <- res.tscgm$lam1.opt
#' lambda2.opt <- res.tscgm$lam2.opt
#' 
#' # Sparsity levels
#' sparsity_theta <- res.tscgm$s.theta
#' sparsity_gamma <- res.tscgm$s.gamma
#' 
#' # Graphical visualization
#' oldpar <- par(mfrow=c(2,2))
#' plot.tscgm(datas, mat="precision", main="True precision matrix")
#' plot.tscgm(res.tscgm, mat="precision", main="Estimated precision matrix")
#' plot.tscgm(datas, mat="autoregression", main="True autoregression coef. matrix")
#' plot.tscgm(res.tscgm, mat="autoregression", main="Estimated autoregression coef. matrix")
#' par(oldpar)

sparse.tscgm <- function(data=data, lam1=NULL, lam2=NULL, nlambda=NULL, 
            model=c("ar1","ar2"), penalty=c("scad","lasso"),          
            optimality=c("NULL","bic","bic_ext","bic_mod","aic","gic"),
            control=list())
{
   if(is.longitudinal(data)==TRUE)
  {
   n_obs = get.time.repeats(data)$repeats[[1]]
   tps = get.time.repeats(data)$time
   p_num = dim(data)[2]
   time = dim(data)[1]/n_obs
   xy.data <-array(NA, c(time,p_num,n_obs))
   data <- as.matrix(data)
   for(i in 1:n_obs){
     for(t in 1:time){
       cc <- 1+(t-1)*n_obs + (i-1)
       xy.data[t,,i] <- data[cc,]
     }
   }          
  } else{ 
   ##warning("Data format is not longitudinal.", "\n")
   stop("Data format is not longitudinal.")
  }

  model = match.arg(model)
  penalty = match.arg(penalty)
   ################
   data.prep <-  pre.tscgm(xy.data=xy.data, time=time, model=model)
   xty <- data.prep$xty
   xtx <- data.prep$xtx
   yty <- data.prep$yty
   xtxt <- data.prep$xtxt
   xtx2 <- data.prep$xtx2
   yty2 <- data.prep$yty2
   p <- data.prep$p
   T <- data.prep$T
   n <- data.prep$n
   q <- data.prep$q
   ###########

   ###################
  if(is.null(lam1) | is.null(lam2) )  {
    lam.sequence <- lam.generate(model=model, nlambda=NULL, xty=xty, xtx=xtx, yty=yty,T=T, n=n)
    lam1 <- lam.sequence$lam1
    lam2 <- lam.sequence$lam2
  }
   ###################

     
    optimality = match.arg(optimality)
    nobic = (length(lam1) + length(lam2) == 2)
    doms=(length(lam1)+length(lam2) > 2)
    if(!is.list(control))
	stop("control is not a list")
	setting <- list(maxit.out = 5, maxit.in = 50, tol.out = 1e-04, silent = TRUE)
	nmsSetting <- names(setting)
	setting[(nms <- names(control))] <- control
	if(length(noNms <- nms[!nms %in% nmsSetting]))
	warning("unknow names in control: ",paste(noNms,collapse =", "))
 	if(nobic != 2 & optimality == "nosel" )
	stop("Specify positive scalar values for the tuning parameters")
	if(doms > 2 & optimality != "nosel")
	stop("Specify vector of positive decreasing values for the tuning parameters")
	if(setting$maxit.out < 1 )
	stop("read the documentation for 'maxit.out' more carefully")
	if(setting$maxit.in < 1 )
	stop("read the documentation for 'maxit.in' more carefully")
	if(setting$tol.out <= 0)
	stop("read the documentation for 'tol.out' more carefully")
	if(!setting$silent %in% c("TRUE","FALSE"))
	stop("read the documentation for 'silent' more carefully")

    doNULL = nobic & (optimality == "NULL")
    dobic =  doms & (optimality == "bic")
    dobic1 =  doms & (optimality == "bic_ext")
    dobic2 =  doms & (optimality == "bic_mod")
    dobic3 =  doms & (optimality == "aic")
    dobic4 =  doms & (optimality == "gic")
    gamma = NULL
    theta = NULL


    if (doNULL) {
        tmp.out = compute.sparse.tscgm(penalty=penalty, T=T, n=n, p=p, q=q, xty=xty, xtx=xtx, yty=yty,
       xtxt=xtxt, xtx2=xtx2, yty2=yty2, lam1=lam1, lam2=lam2,
       optimality = "NULL", setting=setting)
         }
    else if (dobic) {
        tmp.out = sparse.tscgm.bic(penalty=penalty,T=T, n=n, p=p, q=q, xty=xty, xtx=xtx, yty=yty,
            xtxt=xtxt, xtx2=xtx2, yty2=yty2, lam1=lam1, lam2=lam2,
            optimality = "bic", setting=setting)
    }
    else if (dobic1) {
        tmp.out = sparse.tscgm.bic(penalty=penalty,T=T, n=n, p=p, q=q, xty=xty, xtx=xtx, yty=yty,
            xtxt=xtxt, xtx2=xtx2, yty2=yty2, lam1=lam1, lam2=lam2,
            optimality = "bic_ext", setting=setting)
    }
    else if (dobic2) {
        tmp.out = sparse.tscgm.bic(penalty=penalty,T=T, n=n, p=p, q=q, xty=xty, xtx=xtx, yty=yty,
            xtxt=xtxt, xtx2=xtx2, yty2=yty2, lam1=lam1, lam2=lam2,
            optimality = "bic_mod", setting=setting)
    }
    else if (dobic3) {
        tmp.out = sparse.tscgm.bic(penalty=penalty,T=T, n=n, p=p, q=q, xty=xty, xtx=xtx, yty=yty,
            xtxt=xtxt, xtx2=xtx2, yty2=yty2, lam1=lam1, lam2=lam2,
            optimality = "aic", setting=setting)
    }
    else if (dobic4) {
        tmp.out = sparse.tscgm.bic(penalty=penalty,T=T, n=n, p=p, q=q, xty=xty, xtx=xtx, yty=yty,
            xtxt=xtxt, xtx2=xtx2, yty2=yty2, lam1=lam1, lam2=lam2,
            optimality = "gic", setting=setting)
    }
    #if(q <= 50) {
    gamma = tmp.out$gamma
    gamma = gamma*(1*(abs(gamma) > 0.01))

    theta = tmp.out$theta
    theta = theta*(1*(abs(theta) > 0.01))
    #}
    # else if(q > 50) {
    #gamma = tmp.out$gamma
    #gamma = gamma*(1*(abs(gamma) > 0.05))

   # theta = tmp.out$theta
   # theta = theta*(1*(abs(theta) > 0.05))
   # }
    
    lam1.opt = tmp.out$lam1.opt
    lam2.opt = tmp.out$lam2.opt
    lam1.seq = tmp.out$lam1.seq
    lam2.seq = tmp.out$lam2.seq
    s.gamma  =  tmp.out$s.gamma
    s.theta = tmp.out$s.theta
    tun.ic = tmp.out$tun.ic
    min.ic = tmp.out$min.ic
    if(model=="ar1") {colnames(gamma) <- colnames(data)}
    else if(model=="ar2"){
     colnames(gamma) <- colnames(data)
     rownames(gamma) <- c(colnames(data), colnames(data))
     }
	   colnames(theta) <- rownames(theta) <- colnames(data)
	   ###
     #gamma <- t(gamma)
    out = list(gamma = gamma, theta = theta, lam1.opt=lam1.opt, lam2.opt=lam2.opt,
      lam1.seq=lam1.seq, lam2.seq=lam2.seq,
      min.ic=min.ic, tun.ic=tun.ic, s.gamma=s.gamma,  s.theta=s.theta)
    class(out)="sparse.tscgm"
	return(out)
}

  
