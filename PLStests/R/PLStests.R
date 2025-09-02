#' @import glmnet
#' @import harmonicmeanp
#' @import MASS
#' @import psych
#'

PLStest_GLM <- function(y, x, b0=2, np=10) {

  n = nrow(x)
  p = ncol(x)
  b0_tmp = b0
  num_h = length(b0_tmp)

  LST_pval_cauchy <- rep(NA,num_h)
  LST_pval_single <- rep(NA,num_h)
  LST_pval_hmp <- rep(NA,num_h)
  LST_pval_betan <- rep(NA,num_h)

  #lasso
  mod_cv0 <- glmnet::cv.glmnet(x, y, family = "binomial", maxit=100000,intercept = F)
  lamuda <- c("lambda.min", "lambda.1se")[2]
  beta_n <- coef(mod_cv0, s = lamuda)[-1]
  beta_none0 <- seq(1:ncol(x))[as.numeric(beta_n) != 0]

  for (id_h in 1:num_h) {
    b0 = b0_tmp[id_h]
    tryCatch({

      len <- length(beta_none0)
      #bandwidth
      h <- b0*(n^(-1/(4+len)))
      #projection vector, redefine  sigma to avoid case2
      sigma1 <- diag(1, p)
      mu <- rep(0, p)
      beta_pro <- mvrnorm(n=np,mu=mu,Sigma=sigma1)
      #normalization
      beta_pro <- beta_pro/sqrt(rowSums(beta_pro^2))
      if(len == 0){
        pred.lasso = predict(mod_cv0, newx = x,type="response",s=mod_cv0$lambda.1se)
        pred.lasso = matrix(unname(pred.lasso),ncol = 1)
        U <- y-pred.lasso
        beta_pro <- beta_pro
      }else{
        X_S <- x[,beta_none0]
        fit.model = glm(y~X_S-1,family = binomial(link = "logit"))
        pred = predict(fit.model, newx = X_S,type="response")
        pred = matrix(unname(pred),ncol = 1)
        beta_hat = unname(fit.model$coefficients)

        U <- y-pred
        #new x
        X_SC <- x[,-beta_none0]
        x_new <- cbind(X_S,X_SC)
        x <- x_new
        beta_hat <- c(beta_hat,rep(0,(p-length(beta_none0))))
        beta_hat <- beta_hat/sqrt(sum(beta_hat^2))
        beta_pro <- rbind(beta_pro,beta_hat)
      }
      #residual matrix ，ui*uj
      errormat <- U%*%t(U)
      #the number of projections
      pro_num <- nrow(beta_pro)
      #p-value matrix
      pval_matrix <- matrix(nrow=pro_num,ncol=1)
      for(q in 1:pro_num){
        X_beta <- x%*%beta_pro[q,]
        #kernel function matrix
        X_beta_mat <-((X_beta)%*%matrix(1,1,n)- matrix(1,n,1)%*%(t(X_beta)))/h
        kermat <-(1/sqrt(2*pi))*exp(-(X_beta_mat^2)/2)
        #test statistics
        Tn <- (sum(kermat*errormat)-tr(kermat*errormat))/sqrt(2*(sum((kermat*errormat)^2)-tr((kermat*errormat)^2)))
        pval <- 1-pnorm(Tn)
        pval_matrix[q,] <- pval
      }
      #single
      pval_single <- pval_matrix[1,1]
      #betan
      if(len == 0){
        pval_betan <- NA
      }else{
        pval_betan <- pval_matrix[pro_num,1]
      }
      pval_betan <- pval_matrix[pro_num,1]
      #cauchy combination
      pval_cauchy <- 1- pcauchy(mean(tan((0.5-pval_matrix)*pi)))
      #harmonic p-value
      if(any(pval_matrix==0)){
        pval_hmp <- 0
      }else{
        pval_hmp <- pharmonicmeanp(1/mean(1/pval_matrix),pro_num)
      }

      #power
      result2 <- c(pval_cauchy, pval_single,pval_hmp,pval_betan)
      LST_pval_cauchy[id_h] = result2[1]
      LST_pval_single[id_h] = result2[2]
      LST_pval_hmp[id_h] = result2[3]
      LST_pval_betan[id_h] = result2[4]
      #error situation
    }, error = function(e) {
      #power
      print(e)
    })
  }

  #result
  return(
    data.frame(
      h = b0_tmp,
      T_cauchy = LST_pval_cauchy,
      T_alpha = LST_pval_single,
      T_hmp = LST_pval_hmp,
      T_beta = LST_pval_betan
    )
  )
}

PLStest_LM <- function(y, x, b0=2, np=10) {

  n = nrow(x)
  p = ncol(x)
  b0_tmp = b0
  num_h = length(b0_tmp)
  LST_pval_cauchy <- rep(NA,num_h)
  LST_pval_single <- rep(NA,num_h)
  LST_pval_hmp <- rep(NA,num_h)
  LST_pval_betan <- rep(NA,num_h)

  #lasso
  mod_cv0 <- glmnet::cv.glmnet(x, y, family = "gaussian",intercept = F)
  lamuda <- c("lambda.min", "lambda.1se")[2]
  beta_n <- coef(mod_cv0, s = lamuda)[-1]
  beta_none0 <- seq(1:ncol(x))[as.numeric(beta_n) != 0]
  for (id_h in 1:num_h) {
    b0 = b0_tmp[id_h]
    tryCatch({

      len <- length(beta_none0)
      #bandwidth
      h <- b0*(n^(-1/(4+len)))
      #projection vector, redefine  sigma to avoid case2
      sigma1 <- diag(1, p)
      mu <- rep(0, p)
      beta_pro <- mvrnorm(n=np,mu=mu,Sigma=sigma1)
      #normalization
      beta_pro <- beta_pro/sqrt(rowSums(beta_pro^2))
      if(len == 0){
        pred.lasso = predict(mod_cv0, newx = x,type="response",s=lamuda)
        pred.lasso = matrix(unname(pred.lasso),ncol = 1)
        U <- y-pred.lasso
        beta_pro <- beta_pro
      }else{
        X_S <- x[,beta_none0]
        fit.model = glm(y~X_S-1,family = gaussian(link = "identity"))
        pred = predict(fit.model, newx = X_S,type="response")
        pred = matrix(unname(pred),ncol = 1)
        beta_hat = unname(fit.model$coefficients)
        U <- y-pred
        #new x
        X_SC <- x[,-beta_none0]
        x_new <- cbind(X_S,X_SC)
        x <- x_new
        beta_hat <- c(beta_hat,rep(0,(p-length(beta_none0))))
        beta_hat <- beta_hat/sqrt(sum(beta_hat^2))
        beta_pro <- rbind(beta_pro,beta_hat)
      }
      #residual matrix ，ui*uj
      errormat <- U%*%t(U)
      #the number of projections
      pro_num <- nrow(beta_pro)
      #p-value matrix
      pval_matrix <- matrix(nrow=pro_num,ncol=1)
      for(q in 1:pro_num){
        X_beta <- x%*%beta_pro[q,]
        #kernel function matrix
        X_beta_mat <-((X_beta)%*%matrix(1,1,n)- matrix(1,n,1)%*%(t(X_beta)))/h
        kermat <-(1/sqrt(2*pi))*exp(-(X_beta_mat^2)/2)
        #test statistics
        Tn <- (sum(kermat*errormat)-tr(kermat*errormat))/sqrt(2*(sum((kermat*errormat)^2)-tr((kermat*errormat)^2)))
        pval <- 1-pnorm(Tn)
        pval_matrix[q,] <- pval
      }
      #single
      pval_single <- pval_matrix[1,1]
      #betan
      if(len == 0){
        pval_betan <- NA
      }else{
        pval_betan <- pval_matrix[pro_num,1]
      }
      pval_betan <- pval_matrix[pro_num,1]
      #cauchy combination
      pval_cauchy <- 1- pcauchy(mean(tan((0.5-pval_matrix)*pi)))
      #harmonic p-value
      if(any(pval_matrix==0)){
        pval_hmp <- 0
      }else{
        pval_hmp <- pharmonicmeanp(1/mean(1/pval_matrix),pro_num)
      }

      #power
      result2 <- c(pval_cauchy, pval_single, pval_hmp, pval_betan)
      LST_pval_cauchy[id_h] = result2[1]
      LST_pval_single[id_h] = result2[2]
      LST_pval_hmp[id_h] = result2[3]
      LST_pval_betan[id_h] = result2[4]
      #error situation
    }, error = function(e) {
      print(e)
    })
  }

  #result
  return(
    data.frame(
      h = b0_tmp,
      T_cauchy = LST_pval_cauchy,
      T_alpha = LST_pval_single,
      T_hmp = LST_pval_hmp,
      T_beta = LST_pval_betan
    )
  )
}


#' Model checking for high dimensional generalized linear models based on random projections
#'
#' The function can test goodness-of-fit of a low- or high-dimensional
#' generalized linear model (GLM) by detecting the presence of nonlinearity in
#' the conditional mean function of y given x using the statistics proposed by paper xx.
#' The outputs are p-value of  statisitics.
#'
#' @param y : y Input matrix with \code{n} rows, 1-dimensional response vector
#' @param x : x Input matrix with \code{n} rows, each a \code{p}-dimensional observation vector.
#' @param family : Must be "gaussian" or "binomial" for linear or logistic regression model.
#' @param b0 : a paramter to set bindwith, the default value may better for real data analysing.
#' @param np : the number of random projections.
#'
#' @return a list with five parameters returned. \code{h} stand for \code{b_0}.
#' T_alpha: the p value of our statistics by random projection. T_beta: the p value of our statistic by
#'  estimated projection. T_cauchy and T_hmp are p value of two combinational method proposed by
#'  Liu and Xie (2020) and Wilson (2019) respectively. each method combines p values of \code{np} random
#'  projections. when the estimated projection is zero, the value set be NA.
#' @references
#' Chen, W., Liu, J., Peng, H., Tan, F., & Zhu, L. (2024). Model checking for high dimensional generalized linear models based on random projections. arXiv [Stat.ME]. Retrieved from http://arxiv.org/abs/2412.10721
#' @importFrom stats binomial coef glm pcauchy pnorm predict rnorm gaussian

#' @export
#'
#' @examples
#'
#' set.seed(100)
#' data("sonar_mines")
#' x = sonar_mines[,-1]
#' y = sonar_mines$y
#'
#' ## make y as 0 or 1 for logistic regression
#' class1 = "R"
#' class2 ="M"
#' y = as.character(y)
#' y[y==class1]=1
#' y[y==class2]=0
#' y = as.numeric(y)
#' y = matrix(y,ncol = 1)
#'
#' ## scale x  and make data to be matrix
#' data_test_x = x
#' data_test_x = as.matrix(data_test_x)
#' data_test_y = as.matrix(y)
#' data_test_x = scale(data_test_x)
#' PLStests(data_test_y,data_test_x,family="binomial")
#'
#'
PLStests <- function(y,x,family,b0=2,np=10){

  if (is.data.frame(x)) {
    message("Input is a data.frame. Converting to matrix...")
    x = as.matrix(x,ncol=1)
  } else if (is.matrix(x)) {
    # message("Input x is already a matrix.")
  } else {
    stop("Input must be a data.frame or a matrix.")
  }

  if (is.data.frame(y)) {
    message("Input is a data.frame. Converting to matrix...")
    y = as.matrix(y,ncol=1)
  } else if (is.matrix(x)) {
    # message("Input y is already a matrix.")
  } else {
    stop("Input must be a data.frame or a matrix.")
  }

  if (dim(x)[1]!=dim(y)[1]){
    stop("The rows of inputs x and y must be equal")
  }


  if(family=="gaussian"){
    result = PLStest_LM(y,x,b0,np)
  }

  if(family=="binomial"){
    result = PLStest_GLM(y,x,b0,np)
  }

  result <- as.list(result)


  return(result)
}
