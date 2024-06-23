#' @title The sequential logistic regression model for binary classification
#'   problem.
#'
#' @description
#' \code{seq_bin_model} estimates the the effective variables and chooses the
#' subjects sequentially by the logistic regression model for the binary
#' classification case with adaptive shrinkage estimate method.
#'
#' @details
#' seq_bin_model is a binary logistic regression model that estimetes the
#' effective variables and determines the samples sequentially from original
#' training data set using adaptive shrinkage estimate given the fixed size
#' confidence set. It's a sequential method that we select sample one by one
#' from data pool. Once it stops, it means we select the enough samples that
#' satisfy the stopping criterion and we can conclude which are the effective
#' variables and its corresponding values and the number of the samples we
#' select.
#' @param startnum The initial number of subjects from original dataset.
#' @param data.clust Large list obtained through k-means clustering. The samples
#'   of the element(data.clust[[1]]) in the data.clust is closer to each other
#'   compared to another element.
#' @param xfix A dataframe that each row is a sample,each column represents an
#'   independent variable. The sample has the minimum variance from each cluster
#'   of the data.clust to represent the all samples for the corresponding
#'   cluster.
#' @param yfix Numeric vector consists of 0 or 1. The length of yfix must be the
#'   same as the xfix.
#' @param d A numeric number specifying the length of the fixed size
#'   confidence set for our model. Note that the smaller the d, the larger
#'   the sample size and the longer the time costs. The default value is 0.5.
#' @param criterion A character string that determines the model selection
#'   criterion to be used, matching one of 'BIC' or 'AIC. The default value is
#'   'BIC'.
#' @param pho A numeric number used in subject selection according to the
#'   D-optimality. That is, select the  first (rho * length(data)) data from the
#'   unlabeled data set and add it to the uncertainty set. The default value is
#'   0.05.
#' @param ptarget A numeric number that help to choose the samples. The default
#'   value is 0.5
#' @export
#' @return a list containing the following components
#' \item{d}{the length of the fixed size confidence set that we specify}
#' \item{n}{the current sample size when the stopping criterion is satisfied}
#' \item{is_stopped}{the label of sequential iterations stop or not. When the value
#' of is_stopped is 1, it means the iteration stops}
#' \item{beta_est}{the parameters that we estimate when the the iteration is
#' finished}
#' \item{cov}{the covariance matrix between the estimated parameters}
#'
#'
#' @references {
#' Wang Z, Kwon Y, Chang YcI (2019). Active learning for binary classification
#' with variable selection.  arXiv preprint arXiv:1901.10079.
#' }
#'
#' @seealso{
#'    \code{\link{seq_GEE_model}} for generalized estimating equations case
#'
#'    \code{\link{seq_bin_model}} for binary classification case
#'
#'    \code{\link{seq_ord_model}} for ordinal case.
#'}
#'
#' @examples
#' # generate the toy example. You should remove '#' to
#' # run the following command.
#' # library(doMC)
#' # registerDoMC(9)
#' # library(foreach)
#' beta <- c(-1,1,0,0)
#' N <- 10000
#' nclass <- 1000
#' seed <- 123
#' data  <- gen_bin_data(beta,N,nclass,seed)
#' xfix <- data[['X']]
#' yfix <- data[['y']]
#' data.clust <- data[['data.clust']]
#' startnum <- 24
#' d <- 0.75
#'
#' # use seq_bin_model to binary classification problem. You can remove '#' to
#' # run the command.
#' # results <- seq_bin_model(startnum, data.clust, xfix, yfix, d,
#' #                          criterion = "BIC", pho = 0.05, ptarget = 0.5)


seq_bin_model <- function(startnum, data.clust, xfix, yfix, d = 0.5, criterion = "BIC",
                          pho = 0.05, ptarget = 0.5){
  if (is.null(xfix) || is.null(yfix)) {
      stop("xfix and yfix must have data")
  }else {
    if (!identical(nrow(xfix), length(yfix))) {
      stop("length(xfix) must be equal to yfix")
    }
  }
  X=NULL
  Y=NULL
  for(i in 1:startnum)
  {
    tmp <- data.clust[[i]]
    n1 <- dim(tmp)[1]
    loc <- sample(1:n1)[1]
    tmp <- tmp[loc,]
    Y <- c(Y,tmp[1])
    X <- rbind(X,tmp[-1])
  }
  data <- data.frame(y=Y,x=X)
  tmp <- stats::glm(y~.-1,data=data,family=stats::binomial("logit"))
  bhat <- stats::coef(tmp)
  z <- c(X%*%bhat)
  tmp <- exp(z)/((1+exp(z))^2)
  W <- diag(tmp)
  seq.res <- ase_seq_logit(Y=Y, X=X, d=d, criterion=criterion)
  is_stopped <- seq.res$is_stopped
  n <- startnum
  index <- (n+1):dim(xfix)[1];
  index_c <- 1:dim(xfix)[2];
  while( ((is_stopped<0.5) ) & n <(dim(xfix)[1]) )
  {
    n <- n+1;
    Xtemp  <-  X;
    Ytemp  <-  Y;
    YR  <-  yfix[index];
    XR  <-  xfix[index,];
    pnum <- dim(X)[2];
    betahat <- seq.res$betahat
    paraloc <- abs(betahat)>1.0e-10
    index_d <- index
    tmp <- rep(0,pnum)
    tmp[paraloc] <- 1
    In <- diag(tmp)

    temp <- foreach(i=1:length(index_d),.combine=rbind) %dopar%  {
      X <- rbind(Xtemp, xfix[index_d[i],])
      z <- c(X%*%betahat)
      tmp <- exp(z)/((1+exp(z))^2)
      W <- diag(tmp)
      tmp <- (t(X)%*%W%*%X)[paraloc,paraloc,drop=FALSE]
      tmp <- c(i,det(tmp))

    }
    temp <- temp[order(temp[,1]),]
    temp <- order(temp[,2],decreasing=TRUE)
    index_d <- index_d[temp[1:round(length(index_d)*pho)]]
    XR <- xfix[index_d,,drop=F]
    curr_v  <-  index_c[paraloc]
    if (length(curr_v) > 1)
    {curr_p  <-  XR[,curr_v] %*% betahat[curr_v]}
    if (length(curr_v)== 1){
      curr_p  <-  XR[,curr_v] * betahat[curr_v]}
    if (length(curr_v)==0){
      curr_p  <-  XR[, 1] * 0}
    curr_p <- exp(curr_p)
    curr_p <- curr_p/(1+curr_p)
    index_u <- order(abs(curr_p-ptarget),decreasing=FALSE)
    mloc <- index_d[index_u[1]]

    tmp <- data.clust[[mloc]]
    n1 <- dim(tmp)[1]
    loc <- sample(1:n1)[1]
    tmp <- tmp[loc,]
    X <- rbind(Xtemp, tmp[-1])
    Y <- c(Ytemp, tmp[1])
    index <- setdiff(index, as.vector(mloc))
    #################### sequentially update stage ###################################
    z <- c(X%*%betahat)
    tmp <- exp(z)/((1+exp(z))^2)
    W <- diag(tmp)
    if(min(eigen(t(X)%*%W%*%X)$values)>1.0e-5)
    {

      if(is_stopped < 0.5)
      {
        seq.res <- ase_seq_logit(Y=Y,X=X,d=d, criterion=criterion)
        is_stopped <- seq.res$is_stopped
      }


      if((is_stopped>0.5) ) { break}

    }

  }

  results <- list(d=d,
                  n = n,
                  is_stopped = is_stopped,
                  beta_est = betahat,
                  cov     = seq.res$cov
                  )
  class(results) <- c('seqbin','list')
  return(results)

}

