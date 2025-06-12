#'@name LavaCvxr
#'@aliases  LavaCvxr
#'
#' @title Lava Estimation for the Sum of Sparse and Dense Signals(3 Methods).
#'
#' @description The lava estimation is used to recover signals that is the sum of a sparse signal and a dense signal.
#' The post-lava method corrects the shrinkage bias of lava.
#' The model is Y=X*B+error,
#'  where B can be decomposed into B(theta)=dense part(beta)+sparse part(delta).
#'  Lava solves the following problem: min_[beta,delta] 1/n*|Y-X*(beta+delta)|_2^2+lamda2*|beta|_2^2+lambda1*|delta|_1.
#'  The final estimator is theta, which is theta=beta+delta. Both tuning parameters lambda1 and lambda2 are chosen using the K-fold cross-validation.
#'
#' @details If you choose \code{'Profile'} method or \code{'Iteration'} method, we recommend using a relatively long vector of Lambda1
#' (e.g., 50 or 100 values),
#' but a short vector of Lambda2 (e.g., within 10).
#' Higher dimensions of Lambda2 substantially increase the computational time because a 'for' loop is called for Lambda2.
#' \code{'Profile'} and \code{'Iteration'} depends on the 'Lavash' function in 'Lavash' package. For more details, please see the document for 'Lavash'.
#'
#' @param  X n by p data matrix, where n and p respectively denote the sample size and the number of regressors.
#' @param Y n by 1 matrix of outcome.
#' @param K the K fold cross validation.
#' @param Lambda1 If you choose \code{'Profile'} or \code{'Iteration'}, \code{'Lambda1'} should be a vector of candidate values
#' to be evaluated in the cross validation to find an optimal \code{'Lambda1'}. If you choose \code{'LavaCvxr'}, \code{'Lambda1'}
#' can be a vector (go through the cross validation to get an optimal value) or an any specific value you choose (without going through
#' the cross validation part).
#' @param Lambda2 If you choose \code{'Profile'} or \code{'Iteration'}, \code{'Lambda2'} should be a vector of candidate values
#' to be evaluated in the cross validation to find an optimal \code{'Lambda2'}. If you choose \code{'LavaCvxr'}, \code{'Lambda2'}
#' can be a vector (go through the cross validation to get an optimal value) or an any specific value you choose (without going through
#' the cross validation part).
#' @param method choose among \code{'Profile'}, \code{'Iteration'} and \code{'LavaCvxr'}. \code{'Profile'} computes using the profiled lasso method.
#' \code{'Iteration'} computes using iterating lasso and ridge. \code{'LavaCvxr'} computes using CVXR method to calculate. \code{'Profile'}
#' and \code{'Iteration'} depends on the 'Lavash' function in 'Lavash' package. For more details, please see the document for 'Lavash'.
#' @param Maxiter the maximum number of iterations. The default value is 50. Only used when 'Iteration' is selected.
#'
#' @return An \code{'output_list'} containing the following components:
#' \item{lava_dense}{parameter estimate of the dense component using lava.}
#'\item{lava_sparse}{parameter estimate of the sparse component using lava.}
#'\item{lava_estimate}{lava_estimate=lava_dense+lave_sparse: final parameter estimate using lava.}
#'\item{postlava_dense}{parameter estimate of the dense component using post-lava.}
#'\item{postlava_sparse}{parameter estimate of the sparse component using post-lava.}
#'\item{postlava_estimaate}{postlava_estimate=postlava_dense+postlava_sparse: final parameter estimate using post-lava.}
#'\item{LAMBDA}{[lambda1lava,lambda2lava, lambda1post, lambda2post]: These are the CV-chosen for optimal \code{'Lambda1'} and \code{'Lambda2'}
#'for lava and post-lava or the specific value that you choose without going through the cross validation part.}
#'
#' @import Lavash
#' @importFrom  CVXR Variable
#' @importFrom CVXR sum_squares
#' @importFrom CVXR cvxr_norm
#' @importFrom CVXR Minimize
#' @importFrom CVXR Problem
#' @importFrom CVXR solve
#' @importFrom  pracma mldivide
#' @importFrom pracma pinv
#' @references Chernozhukov, V., Hansen, C., and Liao, Y. (2017) "A lava attack on the recovery of sums of dense and sparse signals", Annals of Statistics, 45, 39-76
#' @author Victor Chernozhukov, Christian Hansen, Yuan Liao, Jaeheon Jung, Yang Liu
#' @keywords lava
#'
#' @examples
#' N <- 20
#' P <- 10
#' K<-5
#'
#' X <- matrix(rnorm(n = N * P, mean = 0, sd = 3), nrow = N, ncol = P)
#'beta_true <- as.matrix(rep(x = 0, times = P) )
#'delta_true <- as.matrix(rep(x = 0, times = P))
#'beta_true[1:P]<-0.1
#'delta_true[1:4] <- c(2, -2, 3, 6)
#'Y <- X%*%delta_true+X%*%beta_true + rnorm(N, mean = 0, sd = 2)
#'
#'lambda1<-seq(0.01,2,by=6/20)
#'lambda2<-c(0.01,0.07,0.2,0.7,3,10,60,1000,6000)
#'
#'lava_result<-LavaCvxr(X,Y,K,lambda1,lambda2,method=c('Profile'), Maxiter=50)
#'
#'lava_result$lava_dense
#'lava_result$lava_sparse
#'lava_result$lava_estimate
#'lava_result$postlava_dense
#'lava_result$postlava_sparse
#'lava_result$postlava_estimate
#'lava_result$LAMBDA
#'
#'@export
LavaCvxr=function(X,Y,K,Lambda1,Lambda2,method=c("Profile","Iteration","LavaCvxr"), Maxiter=50){
  #### check for X
  if(is.matrix(X)< 1){stop("X should be a matrix with 2 or more columns")}
  dimX = dim(X)
  if(is.null(dimX) | (dimX[2] <= 1)){stop("X should be a matrix style with 2 or more columns")}

  #### check for Y
  if(is.matrix(Y)< 1){stop("Y should be a n by 1 matrix style")}
  dimY = dim(Y)
  if(dimX[1] != dimY[1]){stop("number of observations in Y is not equal to the number of rows of X")}

  #### check for K
  if((dimY[1]%%K) != 0 | K< 1){stop("K should be a natural number which divides the number of observations in Y without leaving a remainder")}

  #### check for methods
  if(method != "Profile" & method != "Iteration" & method !="LavaCvxr"){stop("method should be one of profile, iteration or LavaCvxr")}

  #### check for Maxiter
  if((Maxiter%%1) != 0 | Maxiter< 1){stop("Maxiter should be a natural number")}

  #### three methods
  if(method=="Iteration"){
    if(is.vector(Lambda1)< 1){stop("Lambda1 should be a vector style")}
    if(is.vector(Lambda2)< 1){stop("Lambda1 should be a vector style")}
    Lavash_result1<-Lavash::Lavash(X,Y,K,Lambda1,Lambda2,method="iteration", Maxiter)
    output_list<-list("lava_dense"=Lavash_result1$lava_dense,"lava_sparse"=Lavash_result1$lava_sparse,"lava_estimate"=Lavash_result1$lava_estimate,"postlava_dense"=Lavash_result1$postlava_dense,"postlava_sparse"=Lavash_result1$postlava_sparse,"postlava_estimate"=Lavash_result1$post_lava,"LAMBDA"=Lavash_result1$LAMBDA)

  }else if(method=="Profile"){
    if(is.vector(Lambda1)< 1){stop("Lambda1 should be a vector style")}
    if(is.vector(Lambda2)< 1){stop("Lambda1 should be a vector style")}
    Lavash_result2<-Lavash::Lavash(X,Y,K,Lambda1,Lambda2,method="profile", Maxiter)
    output_list<-list("lava_dense"=Lavash_result2$lava_dense,"lava_sparse"=Lavash_result2$lava_sparse,"lava_estimate"=Lavash_result2$lava_estimate,"postlava_dense"=Lavash_result2$postlava_dense,"postlava_sparse"=Lavash_result2$postlava_sparse,"postlava_estimate"=Lavash_result2$post_lava,"LAMBDA"=Lavash_result2$LAMBDA)

  }else if(method=="LavaCvxr"){
    n <- length(Y)
    p <- length(X[1,])
    residual_array<-array(dim=c(K,length(Lambda1),length(Lambda2)))
    elavapost<-array(dim=c(K,length(Lambda1),length(Lambda2)))
    beta_index=seq(1,3*length(Lambda1),3)
    delta_index=seq(2,3*length(Lambda1),3)
    theta_index=seq(3,3*length(Lambda1),3)

    for (k in 1:K) {
      # training data: size n-n/K
      Y1<-as.matrix(Y[-c(((k-1)*n/K+1):(k*n/K)),1])
      X1<-X[-c(((k-1)*n/K+1):(k*n/K)),]

      # validation data n/K
      vY<-as.matrix(Y[((k-1)*n/K+1):((k-1)*n/K+n/K),1])
      vX<-X[((k-1)*n/K+1):((k-1)*n/K+n/K),]

      for (l in 1:length(Lambda2)) {
        # training for lava
        #### Lava_CVXR
        lava_fit <- function(lambda1,y,x,lambda2) {
          n <- length(y)
          p <- ncol(x)

          #    <<Define_Parameters>>
          beta<- CVXR::Variable(p)
          delta<- CVXR::Variable(p)

          #    <<Build_Penalty_Terms>>
          penalty_term2 <- CVXR::sum_squares(beta)
          penalty_term1 <- CVXR::cvxr_norm(delta, 1)
          #    <<Compute_Fitted_Value>>
          y_hat <- x%*%delta+x%*%beta
          #    <<Build_Objective>>
          objective <- CVXR::sum_squares(y - y_hat) / n + lambda1 * penalty_term1 + lambda2 * penalty_term2
          #    <<Define_and_Solve_Problem>>
          prob <- CVXR::Problem(Minimize(objective))
          result <- CVXR::solve(prob, verbose = FALSE)
          beta_hat <- result$getValue(beta)
          delta_hat <- result$getValue(delta)
          #    <<Return_Results>>
          ZERO_THRESHOLD= 1e-6
          ## Zero out stuff before returning
          beta_hat[abs(beta_hat) < ZERO_THRESHOLD] <- 0.0
          delta_hat[abs(delta_hat) < ZERO_THRESHOLD] <- 0.0
          beta_hat=beta_hat
          delta_hat=delta_hat
          output=list( "beta_hat" = beta_hat,
                       "delta_hat" = delta_hat,
                       "theta_hat"=beta_hat+delta_hat)
        }
        lava_fit_result<- sapply(Lambda1,lava_fit, y=Y1, x=X1,lambda2=Lambda2[l])
        beta1=unlist(lava_fit_result[beta_index])
        beta2=matrix(beta1,p,length(Lambda1))
        delta1=unlist(lava_fit_result[delta_index])
        delta2=matrix(delta1,p,length(Lambda1))
        theta1=unlist(lava_fit_result[theta_index])
        theta2=matrix(theta1,p,length(Lambda1))

        # validation for lava
        residual<-vX%*%theta2-vY%*%matrix(1,1,length(Lambda1))  # each column is a vector of LAVA residuals fitted on the validation data. (again, there are L columns,
        # where L=length(Lambda1) represents the number of tried lambda1 values in the range of lambda1)
        residual_array[k,,l] <-colSums((residual^2))  # sum of residual squares # 1 by L; elava_array(k,b,l)  k: K fold, b: Lambda1's choice, l: Lambda2's choice
        rm(residual)

        # post lava
        residual_post<-matrix(0,nrow(vX),length(Lambda1))
        for (j in 1:length(Lambda1)) {
          use_index<-abs(delta2[,j])>1e-6
          if(sum(use_index)>0){
            XJ1<-X1[,use_index];
            newY1<-Y1-X1%*%beta2[,j]
            # In below,pracma::mldivide((t(XJ1)%*%XJ1),XJ1)%*%newY1  represents the estimated nonzero elements of the sparse components,using post lava
            # vX%*%beta2[,j] represents the fitted dense part on the validation data
            residual_post[,j]<-vX[,use_index]%*%(pracma::mldivide((t(XJ1)%*%XJ1),t(XJ1))%*%newY1)+vX%*%beta2[,j]-vY # each column is a vector of post-lava residuals fitted on the validation data.
            rm(use_index,XJ1,newY1)
          } else{
            residual_post[,j]<-vX%*%beta2[,j]-vY
          }

        }  ## for

        elavapost[k,,l]<-colSums((residual_post^2)) # sum of residual squares
        rm(residual_post)

      }  ## l

    }  ## K

    # By now we have evaluated all the lambda1 and lambda2 for all the K possible splitted data sets
    # lava fit by choosing the best lambda1 and lambda2, and then refitting the entire data set.
    cross_lava<-matrix(0,length(Lambda2),ncol(theta2))
    for(l in 1:length(Lambda2)){
      cross_lava[l,]<-colMeans(as.matrix(residual_array[,,l])) # cross_lava(l,g): l: lambda2(l)   g:  lambda1(g)
    }  ## l
    a<-matrix(0,1,ncol(cross_lava)); b<-matrix(0,1,ncol(cross_lava))

    for(h in 1:ncol(cross_lava)){
      a[1,h]<-min(cross_lava[,h])
      b[1,h]<-which.min(cross_lava[,h])
    }
    c<-min(a); d<-which.min(a)
    # optimal choice of lambda1 and lambda2 for lava
    final_Lambda2<-Lambda2[b[1,d]]
    final_Lambda1<-Lambda1[d]

    # Post-lava fit by choosing the best lambda1 and lambda2, and then refitting the entire data set.
    cross_postlava<-matrix(0,length(Lambda2),ncol(theta2))
    for(l in 1:length(Lambda2)){
      cross_postlava[l,]<-colMeans(as.matrix(elavapost[,,l])) # cross_postlava: l: Lambda2(l)   g:  Lambda1(g)
    }
    a2<-matrix(0,1,ncol(cross_postlava)) ; b2<-matrix(0,1,ncol(cross_postlava))
    for(i in 1:ncol(cross_postlava)){
      a2[1,i]<-min(cross_postlava[,i])
      b2[1,i]<-which.min(cross_postlava[,i])
    }
    c2<-min(a2); d2<-which.min(a2)
    final_post_Lambda2<-Lambda2[b2[1,d2]]
    final_post_Lambda1<-Lambda1[d2] # optimal choice of lambda1 and lambda2 for post lava

    result2 <- lava_fit(y=Y, x=X,  lambda1=final_Lambda1, lambda2=final_Lambda2)
    result3 <- lava_fit(y=Y, x=X,  lambda1=final_post_Lambda1, lambda2=final_post_Lambda2)
    use<-abs(result3$delta_hat)>1e-6
    post_sparse<-matrix(0,p,1)
    if(sum(use)>0){
      XJ<-X[,use]
      post_sparse[use]<-pracma::pinv(t(XJ)%*%XJ)%*%t(XJ)%*%(Y-X%*%result3$beta_hat) # post-lava estimator of the sparse component
    }
    post_lavatheta<-result3$beta_hat+post_sparse # final estimator of post-lava

    #clear row and column names
    rownames(result2$beta_hat) <- NULL
    colnames(result2$beta_hat) <- NULL

    rownames(result2$delta_hat) <- NULL
    colnames(result2$delta_hat) <- NULL

    rownames(result2$theta_hat) <- NULL
    colnames(result2$theta_hat) <- NULL

    rownames(result3$beta_hat) <- NULL
    colnames(result3$beta_hat) <- NULL

    rownames(post_sparse) <- NULL
    colnames(post_sparse) <- NULL

    rownames(post_lavatheta) <- NULL
    colnames(post_lavatheta) <- NULL

    #outputs:
    LAMBDA<-c(final_Lambda1, final_Lambda2, final_post_Lambda1, final_post_Lambda2)
    output_list<-list("lava_dense"=result2$beta_hat,"lava_sparse"=result2$delta_hat,"lava_estimate"=result2$theta_hat,"postlava_dense"=result3$beta_hat,"postlava_sparse"=post_sparse,"postlava_estimate"=post_lavatheta,"LAMBDA"=LAMBDA)
    rm(list=setdiff(ls(), "output_list"))

  }#### LavaCvxr method
  return(output_list)
}## final function



























