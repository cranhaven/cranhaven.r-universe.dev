#' Imputation using generalized linear models for missing values
#'
#' The function performs imputation using generalized linear models for missing values in a dataset. It fits these models for each specified response variable separately, utilizing other specified variables, and returns the estimated coefficients and predicted values for each variable. The function handles different distribution families,
#' such as Gaussian, Binomial, and Ordinal, for GLM estimation.
#'
#'
#' @import MASS
#' @import glmnet
#' @import pryr
#' @import doParallel
#' @import foreach
#' @import glmnetcr
#' @import Matrix
#' @importFrom stats predict fitted glm deviance coef
#' @param X Data matrix containing all the variables that may contain missing values.
#' @param ind_y A vector specifying the indices of response variables in the dataset.
#' @param ind_x A vector specifying the indices of predictor variables in the dataset. By default, it is set to -ind_y, which means all variables other than the response variables are considered as predictors.
#' @param miss A logical matrix indicating the missing values in the dataset.
#' @param newdata Data matrix for which imputed values are required. It should have the same column names as the original dataset.
#' @param family A character indicating the distribution family of the GLM. Possible values are "gaussian" (default), "binomial", and "ordinal".
#'
#' @return A list containing the imputed values for each response variable. \item{B}{A matrix of estimated coefficients, where each column contains the coefficients for a response variable, and each row corresponds to a predictor variable (including the intercept term)}
#' \item{PRED}{A matrix of predicted values (or imputations), where each column contains the predicted values for a response variable, and each row corresponds to an observation in the newdata (if provided)}
#' @author Fei Xue and Annie Qu
#' @export
#'
#' @examples
#'
#' library(MASS)
#'
#' # Number of subjects
#' n <- 700
#'
#' # Number of total covariates
#' p <- 40
#'
#' # Number of missing groups of subjects
#' ngroup <- 4
#'
#' # Number of data sources
#' nsource <- 4
#'
#' # Starting indexes of covariates in data sources
#' cov_index=c(1, 13, 25, 37)
#'
#' # Starting indexes of subjects in missing groups
#' sub_index=c(1, 31, 251, 471)
#'
#' # Indexes of missing data sources in missing groups, respectively ('NULL' represents no missing)
#' miss_source=list(NULL, 3, 2, 1)
#'
#' # Create a design matrix
#' set.seed(1)
#' sigma=diag(1-0.4,p,p)+matrix(0.4,p,p)
#' X <- mvrnorm(n,rep(0,p),sigma)
#'
#' # Introduce some block-wise missing
#' for (i in 1:ngroup) {
#'   if (!is.null(miss_source[[i]])) {
#'     if (i==ngroup) {
#'       if (miss_source[[i]]==nsource) {
#'         X[sub_index[i]:n, cov_index[miss_source[[i]]]:p] = NA
#'       } else {
#'         X[sub_index[i]:n, cov_index[miss_source[[i]]]:(cov_index[miss_source[[i]]+1]-1)] = NA
#'       }
#'     } else {
#'       if (miss_source[[i]]==nsource) {
#'         X[sub_index[i]:(sub_index[i+1]-1), cov_index[miss_source[[i]]]:p] = NA
#'       } else {
#'         X[sub_index[i]:(sub_index[i+1]-1), cov_index[miss_source[[i]]]:
#'         (cov_index[miss_source[[i]]+1]-1)] = NA
#'       }
#'     }
#'   }
#' }
#'
#' # Define missing data pattern
#' miss <- is.na(X)
#' # Choose response and predictor variables
#' ind_y <- 25:36
#' ind_x <- 13:24
#' # Data that need imputation
#' newdata <- X[31:250,]
#' # Use the function
#' result <- imputeglm.predict(X = X, ind_y = ind_y, ind_x = ind_x, miss = miss, newdata = newdata)


imputeglm.predict <- function (X, ind_y, ind_x=-ind_y, miss, newdata, family="gaussian") {
  ny=length(ind_y)
  nx=length(X[1,ind_x])+1
  B=matrix(0,nx,ny)
  PRED=matrix(0,dim(newdata)[1],ny)
  for (l in 1:ny) {
    ind_obs=!miss[,ind_y[l]]

    x.t=X[ind_obs,ind_x]
    y.t=X[ind_obs,ind_y[l]]
    x.train=x.t[apply(miss[ind_obs,ind_x],1,sum)==0,]
    y.train=y.t[apply(miss[ind_obs,ind_x],1,sum)==0]

    data0=data.frame(y.train, x.train)
    newx=as.data.frame(newdata)
    colnames(newx)=colnames(data0)[-1]
    if (family=="ordinal") {
      data0[,1]=as.factor(data0[,1])
      if (dim(x.train)[1]>dim(x.train)[2]) {
        fit=polr(y.train~., data = data0)
        coeff=c(0,fit$coef)                       ##### The intercept may be wrong
        pred=as.numeric(as.character(predict(fit, type = 'class', newdata=newx)))
      } else {
        fit <- glmnetcr(data0[,-1], data0[,1], maxit=500)
        select = select.glmnetcr(fit)
        coeff=fit$beta[1:nx,select]               ##### The intercept may be wrong
        pred=as.numeric(fitted(fit, newx=newx, s=select)$class)
      }
    } else if (family=="binomial") {
      data0[,1]=as.factor(data0[,1])
      if (dim(x.train)[1]>dim(x.train)[2]) {
        fit=glm(y.train~., family=family, data=data0)
        coeff=fit$coef
        prob=predict(fit,newdata=newx, type = 'response')
        pred=rep(as.numeric(levels(data0$y.train)[1]),dim(newdata)[1])
        pred[prob>0.5]=as.numeric(levels(data0$y.train)[2])
      } else {
        fit=glmnet(x.train, y.train, family=family)
        k <- fit$df
        n <- fit$nobs
        select=which.min(log(n)*k+deviance(fit))

        coeff=t(as.numeric(coef(fit, s=select)))
        pred=as.numeric(predict(fit, newx=newdata, s=select, type = 'class'))
      }
    } else {
      if (dim(x.train)[1]>dim(x.train)[2]) {
        fit=glm(y.train~., family=family, data=data0)
        coeff=fit$coef
        pred=predict(fit,newdata=newx)
      } else {
        cvfit <- cv.glmnet(x.train, y.train, nfolds=3, family=family)
        fit <- cvfit$glmnet.fit
        coeff=t(as.numeric(coef(fit, s=cvfit$lambda.min)))
        pred=predict(fit, newx=newdata, s=cvfit$lambda.min)
      }
    }
    B[,l]=coeff
    PRED[,l]=pred
  }
  returnlist=list("B"=B, "PRED"=PRED)
  return(returnlist)
}
