#' An S4 method to detect the change-points in a high-dimensional GARCH process.
#' @name garch.seg-class
#' @rdname garch.seg-class
#' @description An S4 method to detect the change-points in a high-dimensional GARCH process using the DCBS methodology described in Cho and Korkas (2018). If a \code{tvMGarch} is specified then it returns a \code{tvMGarch} object is returned. Otherwise a list of features is returned.
#' @param x Input data matrix, with each row representing the component time series.
#' @param p Choose the ARCH order. Default is 1.
#' @param q Choose the GARCH order. Default is 0.
#' @param f The dampening factor. If NULL then \code{f} is selected automatically. Default is NULL.
#' @param off.diag If \code{TRUE} allows to look at the cross-sectional correlation structure.
#' @param dw The length of boundaries to be trimmed off.
#' @param do.pp Allows further post processing of the estimated change-points to reduce the risk of undersegmentation.
#' @param Bsim Number of bootstrap samples for threshold selection. Default is 200.
#' @param sig.level Indicates the quantile of bootstrap test statistics to be used for threshold selection. Default is 0.05.
#' @param do.parallel Number of copies of R running in parallel, if \code{do.parallel = 0}, \%do\% operator is used, see also \link{foreach}.
#' @param object A \code{tvMGarch} object. Not necessary if \code{x} is used.
#' @references
#' Cho, Haeran, and Karolos Korkas. "High-dimensional GARCH process segmentation with an application to Value-at-Risk." arXiv preprint arXiv:1706.01155 (2018).
#' @examples
#' #pw.CCC.obj <- new("simMGarch")
#' #pw.CCC.obj@d=10
#' #pw.CCC.obj@n=1000
#' #pw.CCC.obj@changepoints=c(250,750)
#' #pw.CCC.obj <- pc_cccsim(pw.CCC.obj)
#' #dcs.obj=garch.seg(x=empirObj@y,do.parallel = 4)
#' @import Rcpp foreach doParallel parallel iterators fGarch
#' @importFrom stats as.formula
#' @useDynLib segMGarch, .registration = TRUE
#' @export
#' @aliases garch.seg garch.seg-class garch.seg-methods
setGeneric(name="garch.seg",
           def=function(object,x, p=1, q=0, f=NULL, sig.level=0.05, 
                        Bsim=200, off.diag=TRUE, dw=NULL, do.pp=TRUE,do.parallel=4)
           {
             standardGeneric("garch.seg")
           }
)
#' @rdname garch.seg-class
setMethod(f="garch.seg",signature = c("ANY"), definition = function(object=NULL,x, p=1, q=0, f=NULL, sig.level=0.05, 
            Bsim=200, off.diag=TRUE, dw=NULL, do.pp=TRUE,do.parallel=4) {
    formula <- as.formula(paste( paste0("~garch(",p,",",q,")",sep="")))
    burnin <- 100; gam <- 0; mby <- NULL; tby <- NULL;eps <- NULL
    n <- dim(x)[1]; T <- dim(x)[2]
    if(is.null(dw)) dw <- round(2*log(T))
    if(is.null(eps)) eps <- 1e-2*min(abs(x))
    fi <- func.input(x, formula, p, q, f, eps, off.diag)
    ttx <- fi$ttx; res <- fi$res; c.mat <- fi$c.mat; sgn <- fi$sgn; f <- fi$f
    #if(boot.op==1 & (is.null(prob) || (prob<=0 | prob>=1))) prob <- min(.5, 1/mean(apply(res, 1, function(z){g <- get.gg(z, M=M); ((g[2]/g[1])^2)^(1/3)*T^(1/5)})))	
    d <- nrow(ttx); len <- ncol(ttx)
    #if(is.null(gam)) gam <- log(d)
    if(is.null(mby)) mby <- round(log(d))
    if(is.null(tby)) tby <- round(log(len))
    #if(is.null(rule)) rule <- round(log(len, 2)/2)
    mat <- make.tree(ttx, op=2, gam=0, dw, rule=NULL)$mat
    ns <- func.null.stat(mat, op=2, res, T, c.mat, prob=NULL, burnin, p, q, Bsim, boot.op=3, f, eps, off.diag, sgn, mby, tby, gam=gam)
    k <- 1; pos.seq <- c()
    while(k <= ncol(mat)){
      pos <- mean(mat[5, k]>ns[k, ])
      pos.seq <- c(pos.seq, pos)
      if(pos < 1-sig.level-(do.pp)*sig.level){
        l <- mat[6, k]+1; rm.ind <- mat[1, k]
        while(l <= max(mat[6, ])){
          ind <- which(mat[6, ]==l & is.element(mat[1, ], c(rm.ind*2-1, rm.ind*2)))
          if(length(ind) > 0){
            rm.ind <- mat[1, ind]
            mat <- mat[, -ind, drop=FALSE]
            l <- l+1
          } else{
            break
          }
        }
      } 
      k <- k+1
    }
    tree <- list(matrix(0, 6, 1))
    mat <- rbind(mat, pos.seq); mat <- mat[, pos.seq >= 1-sig.level-(do.pp)*sig.level, drop=FALSE]; pos.seq <- pos.seq[pos.seq >= 1-sig.level-(do.pp)*sig.level]
    if(dim(mat)[2] > 0){
      for(l in 1:length(unique(mat[6, ]))){
        j <- unique(mat[6, ])[l]
        for(ncc in 1:sum(mat[6, ]==j)){
          k <- sum(mat[6, ] < j) + ncc
          if(length(tree)<j) tree <- c(tree, list(matrix(0, 6, 0)))
          tree[[l]] <- matrix(c(tree[[l]], matrix(0, 6, 1)), 6, ncc)
          tree[[l]][1, ncc] <- mat[1, k]
          tree[[l]][2, ncc] <- mat[2, k]
          tree[[l]][3, ncc] <- mat[3, k]
          tree[[l]][4, ncc] <- mat[4, k]					
          tree[[l]][5, ncc] <- mat[8, k]
          tree[[l]][6, ncc] <- mat[5, k]
        }
      }
    }
    est.cps <- mat[3, ]
    if(length(est.cps) > 1){
      sc <- sort(est.cps, decreasing=FALSE, index.return=TRUE)
      est.cps <- sc$x; pos.seq <- pos.seq[sc$ix]
    } 
    if(do.pp && length(est.cps)>1){
      brks <- c(0, est.cps, len)
      pp.mat <- c()
      for(b in 1:length(est.cps)){
        #			s <- brks[b]+1*(b==1)+floor(min(dw, min(diff(brks))/3))*(b>1); e <- brks[b+2]-floor(min(dw, min(diff(brks))/3))
        rad <- min(diff(brks[b:(b+2)]))
        s <- brks[b+1]-rad+1; e <- brks[b+1]+rad
        stat <- func_density(ttx[, s:e], gam)$res[brks[b+1]-s+1, 2]
        pp.mat <- cbind(pp.mat, c(b, s, brks[b+1], e, stat))
      }
      pp.ns <- func.null.stat(pp.mat, op=2, res, T, c.mat, prob=NULL, burnin, p, q, Bsim, boot.op=3, f, eps, off.diag, sgn, mby, tby, gam,do.parallel)
      pp.pos.seq <- c()
      for(b in 1:length(est.cps)) pp.pos.seq <- c(pp.pos.seq, mean(pp.mat[5, b] > pp.ns[b, ]))
      pp.est.cps <- pp.mat[3, apply(cbind(pos.seq, pp.pos.seq), 1, max) >= 1-sig.level]
      
    } else pp.est.cps <- est.cps
    ls <- list(fi=fi, op=2, tree=tree, est.cps=est.cps, pp.est.cps=pp.est.cps)	
    return(ls)
  }
)
#' @rdname garch.seg-class
setMethod(f="garch.seg",signature = c("tvMGarch"), definition = function(object, p=1, q=0, f=NULL, sig.level=0.05, 
                                                                    Bsim=200, off.diag=TRUE, dw=NULL, do.pp=TRUE,do.parallel=4) {
  formula <- as.formula(paste( paste0("~garch(",p,",",q,")",sep="")))
  x = object@in_sample_y = object@y[,1:(floor(dim(object@y)[2]*(1-object@out_of_sample_prop)))]
  object@out_of_sample_y = t(tail(t(object@y),object@out_of_sample_prop*dim(object@y)[2]))
  burnin <- 100; gam <- 0; mby <- NULL; tby <- NULL;eps <- NULL
  n <- dim(x)[1]; T <- dim(x)[2]
  if(is.null(dw)) dw <- round(2*log(T))
  if(is.null(eps)) eps <- 1e-2*min(abs(x))
  fi <- func.input(x, formula, p, q, f, eps, off.diag)
  ttx <- fi$ttx; res <- fi$res; c.mat <- fi$c.mat; sgn <- fi$sgn; f <- fi$f
  #if(boot.op==1 & (is.null(prob) || (prob<=0 | prob>=1))) prob <- min(.5, 1/mean(apply(res, 1, function(z){g <- get.gg(z, M=M); ((g[2]/g[1])^2)^(1/3)*T^(1/5)})))	
  d <- nrow(ttx); len <- ncol(ttx)
  #if(is.null(gam)) gam <- log(d)
  if(is.null(mby)) mby <- round(log(d))
  if(is.null(tby)) tby <- round(log(len))
  #if(is.null(rule)) rule <- round(log(len, 2)/2)
  mat <- make.tree(ttx, op=2, gam=0, dw, rule=NULL)$mat
  ns <- func.null.stat(mat, op=2, res, T, c.mat, prob=NULL, burnin, p, q, Bsim, boot.op=3, f, eps, off.diag, sgn, mby, tby, gam=gam)
  k <- 1; pos.seq <- c()
  while(k <= ncol(mat)){
    pos <- mean(mat[5, k]>ns[k, ])
    pos.seq <- c(pos.seq, pos)
    if(pos < 1-sig.level-(do.pp)*sig.level){
      l <- mat[6, k]+1; rm.ind <- mat[1, k]
      while(l <= max(mat[6, ])){
        ind <- which(mat[6, ]==l & is.element(mat[1, ], c(rm.ind*2-1, rm.ind*2)))
        if(length(ind) > 0){
          rm.ind <- mat[1, ind]
          mat <- mat[, -ind, drop=FALSE]
          l <- l+1
        } else{
          break
        }
      }
    } 
    k <- k+1
  }
  tree <- list(matrix(0, 6, 1))
  mat <- rbind(mat, pos.seq); mat <- mat[, pos.seq >= 1-sig.level-(do.pp)*sig.level, drop=FALSE]; pos.seq <- pos.seq[pos.seq >= 1-sig.level-(do.pp)*sig.level]
  if(dim(mat)[2] > 0){
    for(l in 1:length(unique(mat[6, ]))){
      j <- unique(mat[6, ])[l]
      for(ncc in 1:sum(mat[6, ]==j)){
        k <- sum(mat[6, ] < j) + ncc
        if(length(tree)<j) tree <- c(tree, list(matrix(0, 6, 0)))
        tree[[l]] <- matrix(c(tree[[l]], matrix(0, 6, 1)), 6, ncc)
        tree[[l]][1, ncc] <- mat[1, k]
        tree[[l]][2, ncc] <- mat[2, k]
        tree[[l]][3, ncc] <- mat[3, k]
        tree[[l]][4, ncc] <- mat[4, k]					
        tree[[l]][5, ncc] <- mat[8, k]
        tree[[l]][6, ncc] <- mat[5, k]
      }
    }
  }
  est.cps <- mat[3, ]
  if(length(est.cps) > 1){
    sc <- sort(est.cps, decreasing=FALSE, index.return=TRUE)
    est.cps <- sc$x; pos.seq <- pos.seq[sc$ix]
  } 
  if(do.pp && length(est.cps)>1){
    brks <- c(0, est.cps, len)
    pp.mat <- c()
    for(b in 1:length(est.cps)){
      #			s <- brks[b]+1*(b==1)+floor(min(dw, min(diff(brks))/3))*(b>1); e <- brks[b+2]-floor(min(dw, min(diff(brks))/3))
      rad <- min(diff(brks[b:(b+2)]))
      s <- brks[b+1]-rad+1; e <- brks[b+1]+rad
      stat <- func_density(ttx[, s:e], gam)$res[brks[b+1]-s+1, 2]
      pp.mat <- cbind(pp.mat, c(b, s, brks[b+1], e, stat))
    }
    pp.ns <- func.null.stat(pp.mat, op=2, res, T, c.mat, prob=NULL, burnin, p, q, Bsim, boot.op=3, f, eps, off.diag, sgn, mby, tby, gam,do.parallel)
    pp.pos.seq <- c()
    for(b in 1:length(est.cps)) pp.pos.seq <- c(pp.pos.seq, mean(pp.mat[5, b] > pp.ns[b, ]))
    pp.est.cps <- pp.mat[3, apply(cbind(pos.seq, pp.pos.seq), 1, max) >= 1-sig.level]
    
  } else pp.est.cps <- est.cps
  ls <- list(fi=fi, op=2, tree=tree, est.cps=est.cps, pp.est.cps=pp.est.cps)	
  object@changepoints = pp.est.cps
  return(object)
}
)