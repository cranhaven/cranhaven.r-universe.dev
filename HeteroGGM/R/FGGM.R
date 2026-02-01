#' Fused Gaussian graphical model.
#'
#' @author Mingyang Ren, Sanguo Zhang, Qingzhao Zhang, Shuangge Ma. Maintainer: Mingyang Ren <renmingyang17@mails.ucas.ac.cn>.
#' @references Ren, M., Zhang S., Zhang Q. and Ma S. (2020). Gaussian Graphical Model-based Heterogeneity Analysis via Penalized Fusion. Biometrics, Published Online.
#' @usage FGGM(data, K, lambda1 = 0.5, lambda2 = 0.2, lambda3 = 2, a = 3, rho = 1,
#'             eps = 5e-2, niter = 20, maxiter=10, maxiter.AMA=5, initialization=T,
#'             initialize, average=F, asymmetric=T, local_appro=T,
#'             penalty = "MCP", theta.fusion=T)
#'
#' @description The base function of Gaussian graphical model-based heterogeneity analysis via penalized fusion: identifying the order of subgroups and reconstructing the network structure.
#' @param data n * p matrix, the design matrix.
#' @param K Int, a selected upper bound of K_0.
#' @param lambda1 A float value, the tuning parameter controlling the sparse of the mean parameter.
#' @param lambda2 A float value, the tuning parameter controlling the sparse of the precision matrix.
#' @param lambda3 A float value, the tuning parameter controlling the number of subgroup.
#' @param a A float value, regularization parameter in MCP, the default setting is 3.
#' @param rho A float value, the penalty parameter in ADMM algorithm of updating precision matrix Theta, the default setting is 1.
#' @param eps A float value, algorithm termination threshold.
#' @param niter Int, maximum number of cycles of the EM algorithm, the default setting is 20.
#' @param maxiter Int, maximum number of cycles of the ADMM algorithm.
#' @param maxiter.AMA Int, maximum number of cycles of the AMA algorithm.
#' @param initialization The logical variable, whether to calculate the initial value, the default setting is T, if initialization = F, the initial value uses initialize.
#' @param initialize A given initial value used if initialization = F.
#' @param average The logical variable, whether to use averaging when integrating parameters that are identified as identical subgroups, the default setting is F, which means the estimated parameters for the subgroup with the largest sample size among the subgroups identified as identical subgroups is used as the final parameter for this subgroup.
#' @param asymmetric The logical variable, symmetry of the precision matrices or not, the default setting is T.
#' @param local_appro The logical variable, whether to use local approximations when updating mean parameters, the default setting is T.
#' @param penalty The type of the penalty, which can be selected from c("MCP", "SCAD", "lasso").
#' @param theta.fusion Whether or not the fusion penalty term contains elements of the precision matrices. The default setting is T.
#'
#' @return A list including all estimated parameters and the BIC value.
#' @export
#'
#' @examples
#' \donttest{
#' n <- 200              # The sample size of each subgroup
#' p <- 20               # The dimension of the precision matrix
#' K0 <- 3               # The true number of subgroups
#' N <- rep(n,K0)        # The sample sizes of K0 subgroups
#' K <- 6                # The given upper bound of K0.
#'
#' ################ The true parameters ################
#' mue <- 1.5
#' nonnum <- 4
#' mu01 <- c(rep(mue,nonnum),rep(-mue,nonnum),rep(0,p-2*nonnum))
#' mu02 <- c(rep(mue,2*nonnum),rep(0,p-2*nonnum))
#' mu03 <- c(rep(-mue,2*nonnum),rep(0,p-2*nonnum))
#'
#' # Power law network
#' set.seed(2)
#' A.list <- Power.law.network(p,s=5,I2=c(1),I3=c(2))
#' Theta01 <- A.list$A1
#' Theta02 <- A.list$A2
#' Theta03 <- A.list$A3
#' sigma01 <- solve(Theta01)
#' sigma02 <- solve(Theta02)
#' sigma03 <- solve(Theta03)
#' Mu0.list <- list(mu01,mu02,mu03)
#' Sigma0.list <- list(sigma01,sigma02,sigma03)
#' Theta0.list <- list(Theta01,Theta02,Theta03)
#'
#' ################ Generating simulated data ################
#' whole.data <- generate.data(N,Mu0.list,Theta0.list,Sigma0.list)
#'
#' PP = FGGM(whole.data$data, K, lambda1 = 0.22, lambda2 = 0.12, lambda3 = 1.83)
#' mu_hat=PP$mu; Theta_hat=PP$Xi; L.mat = PP$L.mat0
#' group = PP$group; prob = PP$prob0; bic = PP$bic; member = PP$member
#' K0_hat = as.numeric(dim(Theta_hat)[3])
#' K0_hat
#' }
#'
FGGM <- function(data, K, lambda1 = 0.5, lambda2 = 0.2, lambda3 = 2, a = 3, rho = 1,
                eps = 5e-2, niter = 20, maxiter=10, maxiter.AMA=5, initialization=T, initialize,
                average=F, asymmetric=T, local_appro=T, penalty = "MCP", theta.fusion=T){

  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## The name of the function: FGGM
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            The key function of Gaussian graphical model-based heterogeneity analysis via penalized fusion:
  ##            identifying K_0 and reconstructing the network structure.
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages:
  ##            R functions: Update_Theta()    AMA_XI()   mcp_d()   S_soft()    MCP_soft()    f.den.vec()    Symmetrize()    cut_diff_ama()
  ##            R packages: NbClust
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ data: n * p matrix, the design matrix.
  ## @ K: int, a selected upper bound of K_0.
  ## @ lambda1: a float value, the tuning parameter controlling the sparse of the mean parameter.
  ## @ lambda2: a float value, the tuning parameter controlling the sparse of the precision matrix.
  ## @ lambda3: a float value, the tuning parameter controlling the number of subgroup.
  ## @ a: a float value, regularization parameter in MCP, the default setting is 3.
  ## @ rho: a float value, the penalty parameter in ADMM algorithm of updating precision matrix Theta, the default setting is 1.
  ## @ eps: a float value, algorithm termination threshold.
  ## @ niter: int, Maximum number of cycles of the EM algorithm, the default setting is 20.
  ## @ maxiter: int, Maximum number of cycles of the ADMM algorithm.
  ## @ maxiter.AMA: int, Maximum number of cycles of the AMA algorithm.
  ## @ initialization: the logical variable, whether to calculate the initial value, the default setting is T,
  ##                                     if initialization = F, the initial value uses initialize.
  ## @ initialize: A given initial value used if initialization = F.
  ## @ average: the logical variable, whether to use averaging when integrating parameters that are identified as identical subgroups,
  ## @          the default setting is F, which means the estimated parameters for the subgroup with the largest sample size among
  ## @          the subgroups identified as identical subgroups is used as the final parameter for this subgroup.
  ## @ asymmetric: the logical variable, symmetry of the precision matrices or not, the default setting is T.
  ## @ local_appro: the logical variable, whether to use local approximations when updating mean parameters, the default setting is T.
  ## @ theta.fusion: Whether or not the fusion penalty term contains elements of the precision matrices.
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Output:
  ## A list FGGM_res including:
  ## @ mu: K0_hat * p matrix, the estimated mean vectors of K0_hat subgroups (K0_hat is the number of identified subgroups).
  ## @ Theta: p * p * K0_hat array, the estimated precision matrices of K0_hat subgroups.
  ## @ Xi: p * p * K0_hat array, the estimated dual variables of the precision matrices in ADMM.
  ## @ niter: int, the actual number of cycles of the algorithm.
  ## @ diff_Xi: a float value, the L2-norm difference between subgroups.
  ## @ prob0: K0_hat * 1 vector, the estimated mixture probabilities of subgroups.
  ## @ L.mat0: n * K0_hat matrix, the estimated probability that each sample belongs to each subgroup.
  ## @ Theta0: p * p * K array, the estimated original precision matrices of given K subgroups.
  ## @ Xi0: p * p * K array, the estimated original dual variables of the precision matrices of given K subgroups in ADMM.
  ## @ mu0: K * p matrix, the estimated original mean vectors of K subgroups.
  ## @ group: a list, the sequence number partition of original K subgroups compressed to K0_hat.
  ## @ bic: a float value, the BIC value corresponding the choice of given tuning parameters.
  ## @ fit.error: a float value, the value of the loss function (without penalty function) corresponding the choice of given tuning parameters.
  ## @ df: a float value, the penalty value for non-zero parameters corresponding the choice of given tuning parameters.
  ## ------------------------------------------------------------------------------------------------------------------------------------------

  n <- as.integer(dim(data)[1])
  p <- as.integer(dim(data)[2])
  K_c <- combn(K,2)
  if (initialization) {
    out.initial = initialize_fuc.dbscan(data,K)
    prob = out.initial$prob
    mu = out.initial$Mu
    Theta = out.initial$Theta
    memb = out.initial$memb
    L.mat = matrix(0,n,K)
    for(jj in 1:n) L.mat[jj, memb[jj]]=1
  } else {
    Theta = initialize$Theta
    mu = initialize$Mu
    prob = initialize$prob
    L.mat = initialize$L.mat
    memb = apply(L.mat,1,which.max)
  }

  # EM algorithm
  f.mat = matrix(0,n,K)
  L.mat.old = matrix(0,n,K)
  mu.old = matrix(0, K, p)
  Theta.old = array(0, dim = c(p, p, K))

  t = 0
  diff_mu = 10
  diff_theta = 10
  while( diff_theta >= eps  && t < niter )
  {
    prob.old = prob
    mu.old = mu
    Theta.old = Theta
    L.mat.old = L.mat

    # calculate pdfs
    for(k.ind in 1:K) {
      f.mat[,k.ind]=f.den.vec( data, as.numeric(mu.old[k.ind,]), Theta.old[,,k.ind] )
    }

    # update L and pi
    for(k.ind in 1:K) {
      for(i in 1:n) {
        L.mat[i,k.ind] = prob.old[k.ind] * f.mat[i,k.ind] / prob.old %*% f.mat[i,]
      }
      prob[k.ind] = mean(L.mat[,k.ind])
    }
    nK = apply(L.mat,2,sum)

    Theta_kk_diff <- rep(0,dim(K_c)[2])
    for (l in 1:dim(K_c)[2]) {
      Theta_kk_diff[l] <- sum((Theta.old[,,K_c[1,l]] - Theta.old[,,K_c[2,l]])^2)
      if(!theta.fusion){Theta_kk_diff[l] = 0}
    }

    # update mean vectors
    for(j in 1:p){
      for(k.ind in 1:K) {
        tmp = t(t(data) - as.numeric(mu[k.ind,])) %*% Theta.old[,j,k.ind] + mu[k.ind,j] * Theta.old[j,j,k.ind]
        hj = t(L.mat[,k.ind])%*%tmp
        tau_k = sqrt(apply((t(mu[k.ind,] - t(mu[-k.ind,])))^2,1,sum) + Theta_kk_diff[ceiling(which(K_c == k.ind)/2)])
        if(penalty=="MCP"){
          v_k = sum(mcp_d(tau_k, lambda3, a) / (tau_k+0.00001) * mu[-k.ind,j])
          mcp_lambda1 = mcp_d(mu[k.ind,j], lambda1, a)
          v_k_hat = sum(mcp_d(tau_k, lambda3, a) / (tau_k+0.00001))
        }
        if(penalty=="SCAD"){
          v_k = sum(SCAD_d(tau_k, lambda3, a=3.7) / (tau_k+0.00001) * mu[-k.ind,j])
          mcp_lambda1 = SCAD_d(mu[k.ind,j], lambda1, a=3.7)
          v_k_hat = sum(SCAD_d(tau_k, lambda3, a=3.7) / (tau_k+0.00001))
        }
        if(penalty=="lasso"){
          v_k = sum(lasso_d(tau_k, lambda3, a) / (tau_k+0.00001) * mu[-k.ind,j])
          mcp_lambda1 = lasso_d(mu[k.ind,j], lambda1, a)
          v_k_hat = sum(lasso_d(tau_k, lambda3, a) / (tau_k+0.00001))
        }

        if(local_appro){
          mu[k.ind,j] = (hj + n*v_k) / (nK[k.ind] * Theta.old[j,j,k.ind] + n*v_k_hat + n*mcp_lambda1/(abs(mu[k.ind,j])+0.00001))
        } else {
          if(n*mcp_lambda1 >= abs(hj + n*v_k)){
            mu[k.ind,j] = 0
          }else{
            mu[k.ind,j] = (hj + n*v_k - n*mcp_lambda1*sign(mu[k.ind,j])) / (nK[k.ind] * Theta.old[j,j,k.ind] + n*v_k_hat)
          }
        }
      }
    }
    mu[abs(mu) < 1e-3] <- 0

    # update precision matrices
    mu_kk_diff <- rep(0,dim(K_c)[2])
    for (l in 1:dim(K_c)[2]) {mu_kk_diff[l] <- sum((mu[K_c[1,l],] - mu[K_c[2,l],])^2)}
    # define the pseudo sample covariance matrices \tilde{S}
    S = array(0, dim = c(p, p, K))
    for (k.ind in 1:K) {
      nk = nK[k.ind]
      L_ikx = sqrt(L.mat[,k.ind])*t(t(data) - mu[k.ind,])
      S[,,k.ind] = t(L_ikx) %*% L_ikx / nK[k.ind]
    }

    Theta_out = Update_Theta(S,nK,lambda2,lambda3, mu_kk_diff, K_c, tol=eps, maxiter=maxiter, maxiter.AMA=maxiter.AMA, penalty=penalty, theta.fusion=theta.fusion)
    Theta = Theta_out$Theta
    Xi = Theta_out$Xi
    V_kk = Theta_out$V_kk

    t = t + 1
    diff_mu = norm(mu.old-mu,type="2")/(norm(mu,type="2")+0.001)
    diff_theta = norm(Theta.old-Theta,type="2")/(norm(Theta,type="2")+0.001)
  }

  group_final = cut_diff_ama(V_kk,K_c,K,cutoff=0.01)
  K_0 = length(group_final)
  mu_final = matrix(0, K_0, p)
  Theta_final = array(0, dim = c(p, p, K_0))
  Xi_final = array(0, dim = c(p, p, K_0))
  prob0 = rep(0,K_0)
  L.mat0 = matrix(0,n,K_0)
  for (l in 1:K_0) {
    gg = group_final[[l]]
    prob0[l] = sum(prob[gg])
    L.mat0[,l] = apply(as.matrix(L.mat[,gg]),1,sum)
    if(length(gg) > 1){
      mu_final[l,] = apply(mu[gg,],2,mean)

      if(!average){
        Theta_final[,,l] = Theta[,,gg[which.max(nK[gg])]]
        Xi_final[,,l] = Xi[,,gg[which.max(nK[gg])]]
      } else {
        Theta_final[,,l] = Theta[,,gg[1]]/length(gg)
        Xi_final[,,l] = Xi[,,gg[1]]/length(gg)
        for (gi in gg[-1]) {
          Theta_final[,,l] = Theta_final[,,l] + Theta[,,gi]/length(gg)
          Xi_final[,,l] = Xi_final[,,l] + Xi[,,gi]/length(gg)
        }
      }

    }else{ mu_final[l,] = mu[gg,]; Theta_final[,,l] = Theta[,,gg]; Xi_final[,,l] = Xi[,,gg] }
  }

  if(asymmetric){
    for(k in 1:K_0) {
      Theta_final[,,k] = Symmetrize(Theta_final[,,k])
      Xi_final[,,k] = Symmetrize(Xi_final[,,k])
    }
  }

  Theta_final[abs(Theta_final) < 1e-3] <- 0
  member = apply(L.mat0,1,function(a){which(a == max(a))[1]})
  BIC.res = BIC(data, mu_final, Xi_final, L.mat0)

  FGGM_res <- list();FGGM_res$mu <-  mu_final;FGGM_res$Theta <- Theta_final
  FGGM_res$Xi <- Xi_final; FGGM_res$niter <- t; FGGM_res$diff_Xi <- V_kk
  FGGM_res$prob0 <- prob0; FGGM_res$L.mat0 <- L.mat0;
  FGGM_res$Theta0 <- Theta; FGGM_res$member <- member;
  FGGM_res$Xi0 <- Xi;FGGM_res$mu0 <- mu;FGGM_res$group <- group_final
  FGGM_res$bic <- BIC.res$bic;FGGM_res$fit.error <- BIC.res$fit.error;FGGM_res$df <- BIC.res$df
  return(FGGM_res)
}
