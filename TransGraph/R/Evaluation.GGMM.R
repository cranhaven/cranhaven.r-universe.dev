#' Evaluation function for the estimated Gaussian graphical mixture models.
#'
#' @author Mingyang Ren <renmingyang17@mails.ucas.ac.cn>.
#' @references Ren, M., Zhang S., Zhang Q. and Ma S. (2020). Gaussian Graphical Model-based Heterogeneity Analysis via Penalized Fusion. Biometrics.
#' @usage Evaluation.GGMM(data, mu_hat, Theta_hat, Mu0, Theta0, M0, L.mat, L0, prob)
#'
#' @description Evaluation function for the estimated Gaussian graphical mixture models.
#' @param data The target data, a n * p matrix, where n is the sample size and p is data dimension.
#' @param mu_hat M0_hat * p matrix, the estimated mean vectors of M0_hat subgroups.
#' @param Theta_hat p * p * M0_hat array, the estimated precision matrices of M0_hat subgroups.
#' @param Mu0 M0 * p matrix, the true mean vectors of M0 subgroups.
#' @param Theta0 p * p * M0 array, the true precision matrices of M0 subgroups.
#' @param M0 The true number of subgroups
#' @param L.mat The estimated clustering results.
#' @param L0 The true clustering results.
#' @param prob The estimated subgroup proportion.
#'
#'
#'
#' @return The vector including:
#'             K: The estimated number of subgroups.
#'             CE: The sub-grouping error
#'             CME: The mean squared error (MSE) for the mean vectors.
#'             PME: The mean squared error (MSE) for the precision matrices.
#'             TPR/FPR: The true and false positive rates for the off-diagonal elements of the precision matrices.
#' @export
#'
#' @import EvaluationMeasures
#'
#'
#'

Evaluation.GGMM = function(data, mu_hat, Theta_hat, Mu0, Theta0, M0, L.mat, L0, prob){

  ## -----------------------------------------------------------------------------------------------------------------
  ## The name of the function: Esti.error
  ## -----------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Gauging performance of the proposed and alternative approaches
  ## -----------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages:
  ##            R functions: f.den.vec()
  ## -----------------------------------------------------------------------------------------------------------------

  p = dim(mu_hat)[2]
  K_hat = dim(mu_hat)[1]
  n_all = dim(data)[1]
  if(K_hat == M0){
    num = rep(0,M0)
    numk = NULL
    for (k in 1:M0) {
      errork = apply((t(mu_hat) - Mu0[k,])^2,2,sum)
      numk = which(errork == min(errork))[1]
      num[k] = numk
    }
    mu_hat = mu_hat[num,]
    Theta_hat = Theta_hat[,,num]
    #  CME & PME
    CME = sqrt(sum((mu_hat - Mu0)^2))/M0
    PME = sqrt(sum((Theta_hat - Theta0)^2))/M0

    #  TPR & FPR
    TPk = (apply((Theta_hat!=0) + (Theta0!=0) == 2,3,sum) - p) / (apply(Theta0!=0,3,sum) - p)
    TPk[(is.na(TPk))] = 1
    TPR = sum(TPk) / M0
    FPk = apply((Theta_hat!=0) + (Theta0==0) == 2,3,sum) / apply(Theta0==0,3,sum)
    FPk[(is.na(FPk))] = 0
    FPR = sum(FPk[(!is.na(FPk))]) / M0

    #  CE
    f.mat = matrix(0,n_all,M0)
    L.mat = matrix(0,n_all,M0)
    for(k.ind in 1:K_hat) {
      f.mat[,k.ind]=f.den.vec( data, as.numeric(mu_hat[k.ind,]), Theta_hat[,,k.ind] )
    }
    for(k.ind in 1:M0) {
      for(i in 1:n_all) {
        L.mat[i,k.ind] = prob[k.ind] * f.mat[i,k.ind] / prob %*% f.mat[i,]
      }
    }
    member = apply(L.mat,1,which.max)

    aa = L0
    cap_matrix0 = matrix(0,n_all,n_all)
    for(i in 1:(n_all-1)){
      for (j in (i+1):(n_all)) {
        cap_matrix0[i,j] <- as.numeric(aa[i] == aa[j])
      }
    }
    aa = member
    cap_matrix1 = matrix(0,n_all,n_all)
    for(i in 1:(n_all-1)){
      for (j in (i+1):(n_all)) {
        cap_matrix1[i,j] <- as.numeric(aa[i] == aa[j])
      }
    }
    CE = sum(abs(cap_matrix1-cap_matrix0)) / (n_all*(n_all-1)/2)
  } else{
    num = rep(0,K_hat)
    for (k in 1:K_hat) {
      mu_hatk = mu_hat[k,]
      errork.mu = apply((t(Mu0) - mu_hatk)^2,2,sum)
      Theta_hatk = Theta_hat[,,k]
      errork.Theta = apply((Theta0 - rep(Theta_hatk,M0))^2,3,sum)
      errork = errork.mu + errork.Theta
      numk = which(errork == min(errork))[1]
      num[k] = numk
    }
    Mu0.re = Mu0[num,]
    Theta0.re = Theta0[,,num]
    if(K_hat == 1){
      Mu0.re = as.matrix(t(Mu0.re))
      Theta0.re = as.array(Theta0.re)
      dim(Theta0.re) <- c(p,p,K_hat)
    }

    #  CME & PME
    CME = sqrt(sum((mu_hat - Mu0.re)^2))/K_hat
    PME = sqrt(sum((Theta_hat - Theta0.re)^2))/K_hat

    #  TPR & FPR
    TPk = (apply((Theta_hat!=0) + (Theta0.re!=0) == 2,3,sum) - p) / (apply(Theta0.re!=0,3,sum) - p)
    TPk[(is.na(TPk))] = 1
    TPR = sum(TPk) / K_hat
    FPk = apply((Theta_hat!=0) + (Theta0.re==0) == 2,3,sum) / apply(Theta0.re==0,3,sum)
    FPk[(is.na(FPk))] = 0
    FPR = sum(FPk[(!is.na(FPk))]) / K_hat

    #  CE
    num = rep(0,M0)
    numk = NULL
    L.hat = rep(0,n_all)
    for (k in 1:M0) {
      errork = apply((t(mu_hat) - Mu0[k,])^2,2,sum)
      numk = which(errork == min(errork))[1]
      num[k] = numk
      L.hat[which(L0 == k)] = numk
    }
    if(K_hat < M0){L.hat=L0}

    f.mat = matrix(0,n_all,K_hat)
    L.mat = matrix(0,n_all,K_hat)
    for(k.ind in 1:K_hat) {
      f.mat[,k.ind]=f.den.vec( data, as.numeric(mu_hat[k.ind,]), Theta_hat[,,k.ind] )
    }
    for(k.ind in 1:K_hat) {
      for(i in 1:n_all) {
        L.mat[i,k.ind] = prob[k.ind] * f.mat[i,k.ind] / prob %*% f.mat[i,]
      }
    }
    member = apply(L.mat,1,which.max)

    aa = L.hat
    cap_matrix0 = matrix(0,n_all,n_all)
    for(i in 1:(n_all-1)){
      for (j in (i+1):(n_all)) {
        cap_matrix0[i,j] <- as.numeric(aa[i] == aa[j])
      }
    }
    aa = member
    cap_matrix1 = matrix(0,n_all,n_all)
    for(i in 1:(n_all-1)){
      for (j in (i+1):(n_all)) {
        cap_matrix1[i,j] <- as.numeric(aa[i] == aa[j])
      }
    }
    CE = sum(abs(cap_matrix1-cap_matrix0)) / (n_all*(n_all-1)/2)
  }
  index = as.data.frame(t(c(K_hat,CE,CME,PME,TPR,FPR)))
  names(index) = c("K","CE","CME","PME","TPR","FPR")

  return(index)
}
