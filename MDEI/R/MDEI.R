## Primary fitting function; wrapper function for this one is below
fit.singlesubsample <- function(y0, treat0, X0, replaceme0, Xmat0, samplesplit0, nthreads.ranger) {
  ## Partial out X's ----
  y <- y0
  treat <- treat0
  X <- X0
  replaceme <- replaceme0
  Xmat <- Xmat0
  samplesplit <- samplesplit0
  
  ses.theta <- ses.tau <- NULL
  treat.partial <- partialOut(treat, X, replaceme, nthreads.ranger)
  y.partial <- partialOut(y, cbind(X), replaceme, nthreads.ranger)
  if(!samplesplit){
    treat.partial <- .5*(treat.partial[replaceme==1]+treat.partial[replaceme==2])
    y.partial <- .5*(y.partial[replaceme==1]+y.partial[replaceme==2])
    treat.partial <- c(treat.partial, treat.partial)
    y.partial <- c(y.partial, y.partial)
    
  }
  Ey.x <- y - y.partial
  
  

  treatmat.theta <- bs.me(treat.partial, "treatment")
  treatmat.tau <- dbs.me(treat.partial)
  
  keeps <- which(as.vector(checkcor(treatmat.theta, .9) == 1))
  treatmat.theta <- treatmat.theta[, keeps]
  treatmat.tau <- treatmat.tau[, keeps]
  
  colnames.treat <- paste("treat", 1:ncol(treatmat.theta), sep = "")
  
  ## Calculate correlations
  # tic("Creating bases")
  bases.obj <-
    createBases(replaceme,
                Xmat,
                y.partial,
                treatmat.theta,
                treatmat.tau,
                ratio = 50)
  
  ## 
  
  n.a <- sum(replaceme == 2)
  p.a <- ncol(bases.obj$Msubsamp)
  alpha.seq <- seq(max(n.a * log(p.a), 10 * p.a), p.a, length = 10)
  
  X.Construct <- cbind(1, bases.obj$MConstruct)[replaceme==2,]
  X.Construct1 <- cbind(1, bases.obj$MConstruct)[replaceme==1,]
  X.Construct2 <- cbind(1, bases.obj$MConstruct)[replaceme==2,]
  
  XpX.Construct <-crossprod(X.Construct2)
  g1 <-
    GCV(y.partial[replaceme == 2],
        X.Construct2,
        alphas = alpha.seq,
        tol = 1e-6*sd(y.partial))
  

  beta.sp <- as.vector(g1$beta)
  diag(XpX.Construct) <- diag(XpX.Construct) + g1$Etausqinv
  hii <- rowSums((X.Construct%*%ginv(XpX.Construct))*X.Construct)
  errs.loo <- #lm.beta$res/(1-hii)
    as.vector((y.partial[replaceme == 2]-X.Construct%*%beta.sp)/(1-hii))

  ## Try loo tau estimates
  y.loo <- X.Construct%*%beta.sp+errs.loo*sample(c(-1,1),length(errs.loo),TRUE)
  g2 <-
    GCV(y.loo,
        X.Construct2,
        alphas = alpha.seq,
        tol = 1e-6*sd(y.partial))
  #beta.sp <- as.vector(g2$beta)
  XpX.Construct <-crossprod(X.Construct2)
  diag(XpX.Construct) <- diag(XpX.Construct) + g2$Etausqinv
  hii <- rowSums((X.Construct%*%ginv(XpX.Construct))*X.Construct)
  errs.loo <- errs.loo/(1-hii)
  
  if(!samplesplit)  {
    errs.insamp <- as.vector(y.partial[replaceme == 2]-X.Construct%*%beta.sp)
    var.beta <- ginv(XpX.Construct) %*% (crossprod( X.Construct1*errs.insamp))%*%ginv(XpX.Construct)
    ses.theta <- rowSums((X.Construct%*%var.beta)*X.Construct)^.5
    X.ConstructDerivative <- cbind(1, bases.obj$MConstructDerivative)
    ses.tau <- rowSums((X.ConstructDerivative[replaceme==2,]%*%var.beta)*X.ConstructDerivative[replaceme==2,])^.5
  }
  
  
  # tic("Gathering coefficients")
  beta.sparse <- beta.sp[-1][abs(beta.sp[-1]) > 1e-2*sd(y)]
  cormat.sparse <- as.matrix(bases.obj$cormat[,abs(beta.sp[-1]) > 1e-2*sd(y)]+1)
  cormat.sparse[4, ] <- beta.sparse
  
   coefnames.sparse <- apply(as.matrix(cormat.sparse[1:3,]), 2, FUN=function(z){
    c1 <- colnames(treatmat.theta)[z[1]]
    c2 <- colnames(Xmat)[z[2]]
    c3 <- colnames(Xmat)[z[3]]
    
    paste(c1,c2,c3,sep=" x ")
  })
   
   
   colnames(cormat.sparse) <- coefnames.sparse
    # toc()
  
  
  fits.curr <- cbind(1, bases.obj$MConstruct) %*% beta.sp
  te.curr <- cbind(0, bases.obj$MConstructDerivative) %*% beta.sp 
  ## Variance calculations ----
  
  # Variance of fitted value
  
  res.sq <- (y.partial - fits.curr)
  res.sq <- (res.sq - mean(res.sq[replaceme == 1])) ^ 2
  treat2 <- treat
  var1 <-
    ranger(res.sq ~ .,
           data = data.frame(treat2, X),
           case.weights = 1 * (replaceme == 1),
           num.threads = nthreads.ranger)
  
  numvar <- 25
  var.treatperm <- 0
  for (i in 1:numvar) {
    treat2 <- sample(treat)
    var.treatperm <-
      var.treatperm + predict(var1, data = data.frame(treat2, X))[[1]] / numvar
  }
  # var.tau <- abs(var.treatperm[replaceme==2]-var1$predictions[replaceme==2])
  var.tau <-
    pmax(var.treatperm[replaceme == 2] - var1$predictions[replaceme == 2], 0)
  output <-
    list(
      "theta.pred" = fits.curr[replaceme == 2],
      "tau.pred" = te.curr[replaceme == 2],
      "var.theta" = var1$predictions[replaceme == 2],
      "var.tau" = var.tau,
      "y.partial" = y.partial[replaceme == 2],
      "Ey.x" = Ey.x[replaceme == 2],
      "errs.loo" = errs.loo,
      "cormat.sparse" = cormat.sparse,
      "ses.tau" = ses.tau,
      "ses.theta" = ses.theta
    )
  
  
  return(output)
}


#' MDEI function
#'
#' Implements the Method of Direct Estimation and Inference
#' @param y The outcome variable, a vector.
#' @param treat The treatment variable, a vector.
#' @param X A matrix of covariates.
#' @param splits Number of repeated cross-fitting steps to implement.
#' @param alpha The desired level of the confidence band.
#' @param samplesplit Whether to use a sample splitting approach. Default is \code{TRUE}.
#' @param conformal Whether to generate a conformal bands or use a critical value from the
#' normal approximation.  Default is \code{TRUE}..
#' @param nthreads.ranger Number of threads used internally by the \code{ranger} function 
#' for random forests.  Default is \code{NULL}.
#' @param verbose 	An optional logical value. If \code{TRUE} information 
#' on the number of split samples completed is printed. Default is \code{TRUE}.
#' @export
#'
#' @examples
#' n <- 100
#'
#' X <- matrix(rnorm(n*1), nrow = n)
#' treat <- rnorm(n)
#' y <- treat^2 + X[,1] + rnorm(n)
#'
#' # Be sure to run with more splits than this.  We recommend
#' # at least 10-50 initially, for exploratory analyses, with several hundred for 
#' # publication quality. For large sample sizes, these numbers may be adjusted down.
#' # These are only recommendations.
#' # Threads are set to 1 to pass CRAN checks, but we suggest leaving it at the default
#' # which ranger takse as the total number available.
#' set.seed(1)
#' m1 <- MDEI(y, treat, X, splits=1, alpha=.9, nthreads.ranger = 1)
#'
#' # Accuracy
#' cor(m1$tau.est, treat*2)
#' cor(m1$theta.est, treat^2)
#'
#' # Coverage
#' mean(apply(m1$CIs.tau-2*treat,1,prod)<0)


#' @return \describe{
#' \item{tau.est}{The estimated marginal effect.}
#' \item{CIs.tau}{Upper and lower values of conformal confidence band.}
#' \item{critical.values}{Conformal critical values.}
#' \item{Ey.x}{Mean of outcome given only covariates.}
#' \item{coefficients}{The list of all nonparametric bases and the proportion of sample splits that they were selected.}
#' \item{internal}{Internal objects used for development and diagnostics.}
#'}
#'
#' @references Ratkovic, Marc and Dustin Tingley.  2023. "Estimation and Inference on Nonlinear and Heterogeneous Effects."  The Journal of Politics.
#'
#' @rdname MDEI

MDEI <- function(y,
                 treat,
                 X,
                 splits = 10,
                 alpha = .9,
                 samplesplit = TRUE,
                 conformal = TRUE,
                 nthreads.ranger = NULL,
                 verbose = TRUE) {
  if(samplesplit==FALSE){
    splits <- 1
    y <- c(y,y)
    treat<- c(treat,treat)
    X <- rbind(X, X)
  }
  n <- length(treat)
  X0 <- X
  X <- apply(X, 2, rank)
  if(length(colnames(X))!=ncol(X)) colnames(X) <- colnames(X0) <- paste("X",1:ncol(X), sep="_")
  
  Xmat.spline <-
    matrix(NA, nrow = n, ncol = ncol(X) * 27 + 10)
  colnames(Xmat.spline) <- paste("init",1:ncol(Xmat.spline),sep="_")
  for (i.X in 1:ncol(X)) {
    col.start <- which(is.na(Xmat.spline[1, ]))[1]
    bmat <- as.matrix(bs.me(X[, i.X], colnames(X)[i.X] ))
    if(ncol(bmat)==1) Xmat.spline[ ,col.start]  <- bmat
    col.stop <- ncol(bmat) + col.start - 1
    Xmat.spline[, col.start:col.stop] <- bmat
    colnames(Xmat.spline)[col.start:col.stop] <- colnames(bmat)
  }
  
  Xmat.spline <- Xmat.spline[, is.finite(Xmat.spline[1, ])]
  Xmat <- Xmat.spline
  keeps <- which(as.vector(checkcor(Xmat, .9)) == 1)
  Xmat <- cbind(1, Xmat[, keeps])
  colnames(Xmat)[1] <- "Intercept"
  
  ## Containers ----
  
  errs.loo.run <- Ey.x.run <-
    y.partial.run <-
    theta.run <-
    tau.run <-
    thetavar.run <- tauvar.run <- matrix(NA, nrow = n, ncol = splits)
  
  coefmat <- NULL
  ## Now, start split sample here ----
  
  for (i.runs in 1:splits) {
    replaceme <- rep(1, n)
    replaceme[1:floor(n / 2)] <- 2
    if(samplesplit==TRUE) replaceme <- sample(replaceme)
    
    singlefit.2 <- fit.singlesubsample(y, treat, X, replaceme, Xmat, samplesplit, nthreads.ranger)
    singlefit.1 <-
      fit.singlesubsample(y, treat, X, 3 - replaceme, Xmat, samplesplit,nthreads.ranger)
    #
    theta.run[replaceme == 1, i.runs] <- singlefit.1$theta.pred
    tau.run[replaceme == 1, i.runs] <- singlefit.1$tau.pred
    thetavar.run[replaceme == 1, i.runs] <- singlefit.1$var.theta
    tauvar.run[replaceme == 1, i.runs] <- singlefit.1$var.tau
    y.partial.run[replaceme == 1, i.runs] <- singlefit.1$y.partial
    Ey.x.run[replaceme == 1, i.runs] <- singlefit.1$Ey.x
    errs.loo.run[replaceme == 1, i.runs] <- singlefit.1$errs.loo
      
    theta.run[replaceme == 2, i.runs] <- singlefit.2$theta.pred
    tau.run[replaceme == 2, i.runs] <- singlefit.2$tau.pred
    thetavar.run[replaceme == 2, i.runs] <- singlefit.2$var.theta
    tauvar.run[replaceme == 2, i.runs] <- singlefit.2$var.tau
    y.partial.run[replaceme == 2, i.runs] <- singlefit.2$y.partial
    Ey.x.run[replaceme == 2, i.runs] <- singlefit.2$Ey.x
    errs.loo.run[replaceme == 2, i.runs] <- singlefit.2$errs.loo
    
    coefmat <- cbind(coefmat,singlefit.1$cormat.sparse,singlefit.2$cormat.sparse)
    
    if(verbose) cat("Finished with cross-fit", i.runs, "\n")
  }
  
  se.theta <-
    (apply(thetavar.run, 1, hl.mean) + apply(theta.run, 1, hl.var)) ^ .5
  # ts.theta <- (y.partial.run - theta.run) / se.theta
  if(!samplesplit) {
    se.theta <- c(singlefit.1$ses.theta, singlefit.1$ses.theta)
   se.theta <-
       (se.theta^2 + apply(thetavar.run, 1, hl.mean)) ^ .5
  }
   ts.theta <- errs.loo.run / se.theta
  
  critical.value.theta <- quantile(abs(ts.theta), alpha)
  if(conformal == FALSE) critical.value.theta <- qnorm((1-alpha)/2)
  CIs.theta <-
    apply(y.partial.run, 1, hl.mean) + critical.value.theta * cbind(-se.theta, se.theta)
  
  se.tau <- (apply(tauvar.run, 1, hl.mean) + apply(tau.run, 1, hl.var)) ^
    .5
  if(!samplesplit) {
    se.tau <- c(singlefit.1$ses.tau, singlefit.1$ses.tau)
     se.tau <- (se.tau^2 + apply(tauvar.run, 1, hl.var)) ^ .5
  }
  
  critical.value.tau <- critical.value.theta+1#(critical.value.theta ^ 2 + 1) ^ .5
  if(conformal == FALSE) critical.value.tau <- qnorm((1-alpha)/2)
  
  CIs.tau <-
    rowMeans(tau.run) + critical.value.tau * cbind(-se.tau, se.tau)
  
  if(!samplesplit){
    CIs.tau <- CIs.tau[replaceme==1,]
    CIs.theta <- CIs.theta[replaceme==1,]
    tau.run <- as.matrix(tau.run[replaceme==1,])
    theta.run <- as.matrix(theta.run[replaceme==1, ])
  }
  
  coefs.ave <- sort(tapply(coefmat[4,],colnames(coefmat), sum))/(splits*2)
  prop.count <- sort(tapply(coefmat[4,],colnames(coefmat), length))/(splits*2)
  
  coefs.return <- data.frame(names(coefs.ave),coefs.ave,prop.count)
  coefs.return <- coefs.return[sort(prop.count,decreasing = T, index.return = T)$ix, ]
  rownames(coefs.return) <- NULL
  
  allobjs <- mget(ls())
  output <- list(
    "tau.est" = apply(tau.run, 1, hl.mean),
    "CIs.tau" = CIs.tau,
    "theta.est" = apply(theta.run, 1, hl.mean),
    "CIs.theta" = CIs.theta,
    "critical.values" = list("theta" = critical.value.theta, "tau" =
                               critical.value.tau),
    "Ey.x" = rowMeans(Ey.x.run),
    "coefficients"=coefs.return,
    "internal"= allobjs
  )
  
  class(output) <- "MDEI"
  return(output)
}
