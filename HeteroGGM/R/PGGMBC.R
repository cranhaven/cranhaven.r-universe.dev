#' Penalized GGM-based clustering.
#'
#' @author Mingyang Ren, Sanguo Zhang, Qingzhao Zhang, Shuangge Ma. Maintainer: Mingyang Ren <renmingyang17@mails.ucas.ac.cn>.
#' @references Ren, M., Zhang S., Zhang Q. and Ma S. (2020). Gaussian Graphical Model-based Heterogeneity Analysis via Penalized Fusion. Biometrics, Published Online, https://doi.org/10.1111/biom.13426.
#'             Zhou, H., Pan, W., & Shen, X. (2009). Penalized model-based clustering with unconstrained covariance matrices. Electronic journal of statistics, 3, 1473.
#' @usage PGGMBC(lambda, data, K, initial.selection="K-means", initialize, average=F,
#'               asymmetric=T, eps = 5e-2, maxiter=10,
#'               maxiter.AMA=5, local_appro=T, trace = F, penalty = "MCP")
#'
#' @description The main function of penalized Gaussian graphical model-based clustering with unconstrained covariance matrices.
#' @param lambda A list, the sequences of the tuning parameters (lambda1 and lambda2).
#' @param data n * p matrix, the design matrix.
#' @param K Int, a given number of subgroups.
#' @param initial.selection The different initial values from two clustering methods, which can be selected from c("K-means","dbscan").
#' @param initialize A given initial values, which should be given when initial.selection is not in c("K-means","dbscan").
#' @param average The logical variable, whether to use averaging when integrating parameters that are identified as identical subgroups, the default setting is F, which means the estimated parameters for the subgroup with the largest sample size among the subgroups identified as identical subgroups is used as the final parameter for this subgroup.
#' @param asymmetric The logical variable, symmetry of the precision matrices or not, the default setting is T.
#' @param eps A float value, algorithm termination threshold.
#' @param maxiter Int, maximum number of cycles of the ADMM algorithm.
#' @param maxiter.AMA Int, maximum number of cycles of the AMA algorithm.
#' @param local_appro The logical variable, whether to use local approximations when updating mean parameters, the default setting is T.
#' @param trace The logical variable, whether or not to output the number of identified subgroups during the search for parameters.
#' @param penalty The type of the penalty, which can be selected from c("MCP", "SCAD", "lasso").
#'
#' @return A list including all estimated parameters and the BIC values with all choices of given tuning parameters, and the selected optional parameters.
#' @export
#' @import huge
#' @importFrom stats cov cutree dist hclust kmeans median
#' @importFrom utils combn
#'
#' @examples
#' \donttest{
#' ######## Example 1: Generate simulation data and apply this method to analysis #######
#' n <- 200              # The sample size of each subgroup
#' p <- 20               # The dimension of the precision matrix
#' K <- 3               # The true number of subgroups
#' N <- rep(n,K)        # The sample sizes of K subgroups
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
#' ################ The implementation and evaluation of competitors ################
#' lambda <- genelambda.obo(nlambda1=5,lambda1_max=0.5,lambda1_min=0.1,
#'                          nlambda2=15,lambda2_max=1.5,lambda2_min=0.1)
#' res <- PGGMBC(lambda, whole.data$data, K, initial.selection="K-means")
#' Theta_hat.list <- res$Theta_hat.list
#' Mu_hat.list <- res$Mu_hat.list
#' prob.list <- res$prob.list
#' L.mat.list <- res$L.mat.list
#' opt_num <- res$Opt_num
#' opt_Theta_hat <- Theta_hat.list[[opt_num]]
#' opt_Mu_hat <- Mu_hat.list[[opt_num]]
#' opt_L.mat <- L.mat.list[[opt_num]]
#' opt_prob <- prob.list[[opt_num]]
#'
#' ######## Example 2: Call the built-in simulation data set and analyze #######
#' data(example.data)
#' K <- 3
#' lambda <- genelambda.obo(nlambda1=5,lambda1_max=0.5,lambda1_min=0.1,
#'                          nlambda2=15,lambda2_max=1.5,lambda2_min=0.1)
#' res <- PGGMBC(lambda, example.data$data, K, initial.selection="K-means")
#' Theta_hat.list <- res$Theta_hat.list
#' opt_num <- res$Opt_num
#' opt_Theta_hat <- Theta_hat.list[[opt_num]]
#' }
#'
PGGMBC <- function(lambda, data, K, initial.selection="K-means",
                   initialize, average=F, asymmetric=T, eps = 5e-2, maxiter=10, maxiter.AMA=5,
                   local_appro=T, trace = F, penalty = "MCP"){

  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## The name of the function: GGMBC
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Searching and selecting the optional tuning parameters under the adaptive BIC-type criterion
  ##            using the proposed method.
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages:
  ##            R functions: FGGM.refit()
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Input:
  ## @ lambda: a list, the sequences of the tuning parameters (lambda1 and lambda2).
  ## @ data: n * p matrix, the design matrix.
  ## @ K: int, a selected upper bound of K_0.
  ## @ initial.selection: the different initial values from two clustering methods, which can be selected from c("K-means","dbscan").
  ## @ initialize: A given initial values, which should be given when initial.selection is not in c("K-means","dbscan").
  ## @ average: the logical variable, whether to use averaging when integrating parameters that are identified as identical subgroups,
  ## @          the default setting is F, which means the estimated parameters for the subgroup with the largest sample size among
  ## @          the subgroups identified as identical subgroups is used as the final parameter for this subgroup.
  ## @ asymmetric: the logical variable, symmetry of the precision matrices or not, the default setting is T.
  ## @ eps: a float value, algorithm termination threshold.
  ## @ maxiter: int, Maximum number of cycles of the ADMM algorithm.
  ## @ maxiter.AMA: int, Maximum number of cycles of the AMA algorithm.
  ## @ local_appro: the logical variable, whether to use local approximations when updating mean parameters, the default setting is T.
  ## @ trace: the logical variable, whether or not to output the number of identified subgroups during the search for parameters.
  ## ------------------------------------------------------------------------------------------------------------------------------------------
  ## Output:
  ## A list "result" including:
  ## @ Opt_lambda: the selected optional tuning parameters.
  ## @ Mu_hat.list: the estimated mean vectors of K0_hat subgroups corresponding all choices of given tuning parameters.
  ## @ Theta_hat.list: the estimated precision matrices of K0_hat subgroups corresponding all choices of given tuning parameters.
  ## @ prob.list: the estimated mixture probabilities of subgroups corresponding all choices of given tuning parameters.
  ## @ member.list: subgroup labels to which each sample belongs corresponding all choices of given tuning parameters.
  ## @ L.mat.list: the estimated probability that each sample belongs to each subgroup corresponding all choices of given tuning parameters.
  ## @ Opt_aBIC: the optional BIC value.
  ## @ Opt_num: the position of the optimal parameter for all given parameters.
  ## ------------------------------------------------------------------------------------------------------------------------------------------

  lambda1 = lambda$lambda1
  lambda2 = lambda$lambda2
  lambda3 = 0
  L1 = length(lambda1)
  L2 = length(lambda2)
  L3 = 1
  L = L1+L2+L3

  aBIC = rep(0,L)
  n_all = dim(data)[1]
  # initialize
  if(initial.selection=="K-means"){
    set.seed(1)
    out.initial = initialize_fuc(data,K)
    memb = out.initial$memb
    L.mat = matrix(0,n_all,K)
    for(jj in 1:n_all) L.mat[jj, memb[jj]]=1
    out.initial$L.mat = L.mat
  } else if(initial.selection=="dbscan"){
    out.initial = initialize_fuc.dbscan(data,K)
    memb = out.initial$memb
    L.mat = matrix(0,n_all,K)
    for(jj in 1:n_all) L.mat[jj, memb[jj]]=1
    out.initial$L.mat = L.mat
  }
  else {out.initial = initialize}

  if(L == 3){
    l=1
    aBIC = rep(0,l)
    Mu_hat.list = list()
    Theta_hat.list = list()
    prob.list = list()
    L.mat.list = list()
    member.list = list()
    lam1 = lambda1;lam2 = lambda2;lam3 = lambda3;
    PP = FGGM.refit(data, K, lam1, lam2, lam3, initialization=F, initialize=out.initial, average=average, asymmetric=asymmetric, local_appro=local_appro, penalty = penalty)
    mu_hat=PP$mu;Theta_hat=PP$Xi;L.mat = PP$L.mat0;group = PP$group;prob = PP$prob0;aBIC[l] = PP$bic; member = PP$member
    Mu_hat.list[[l]]=mu_hat; Theta_hat.list[[l]]=Theta_hat; prob.list[[l]]=prob; L.mat.list[[l]] = L.mat; member.list[[l]]=member
  } else {
    Mu_hat.list = list()
    Theta_hat.list = list()
    prob.list = list()
    L.mat.list = list()
    member.list = list()
    # search lam3
    lam1 = median(lambda1);lam2 = median(lambda2)
    for (l in 1:L3) {
      lam3 = lambda3[l]
      PP = FGGM.refit(data, K, lam1, lam2, lam3, initialization=F, initialize=out.initial, average=average, asymmetric=asymmetric, local_appro=local_appro, penalty = penalty)
      mu_hat=PP$mu;Theta_hat=PP$Xi;L.mat = PP$L.mat0;group = PP$group;prob = PP$prob0;aBIC[l] = PP$bic; member = PP$member
      Mu_hat.list[[l]]=mu_hat; Theta_hat.list[[l]]=Theta_hat; prob.list[[l]]=prob; L.mat.list[[l]] = L.mat; member.list[[l]]=member
      if(trace){
        print(c(cat(paste(l,"lam1 lam2 lam3 ="),c(round(lam1,2),round(lam2,2),round(lam3,2)),":"),paste("K =",as.numeric(dim(Theta_hat)[3]))))
      }
    }
    aBIC[is.na(aBIC)] = 10^10
    aBIC[aBIC==0] = abs(aBIC[aBIC!=0][1])*10
    n_lam3 = which(aBIC[1:L3] == min(aBIC[1:L3]))[1];lam3 = lambda3[n_lam3]

    # search lam2
    for (l2 in 1:L2) {
      lam2 = lambda2[l2];l = L3+l2
      PP = FGGM.refit(data, K, lam1, lam2, lam3, initialization=F, initialize=out.initial, average=average, asymmetric=asymmetric, local_appro=local_appro, penalty = penalty)
      mu_hat=PP$mu;Theta_hat=PP$Xi;L.mat = PP$L.mat0;group = PP$group;prob = PP$prob0;aBIC[l] = PP$bic; member = PP$member
      Mu_hat.list[[l]]=mu_hat; Theta_hat.list[[l]]=Theta_hat; prob.list[[l]]=prob; L.mat.list[[l]] = L.mat; member.list[[l]]=member
      if(trace){
        print(c(cat(paste(l,"lam1 lam2 lam3 ="),c(round(lam1,2),round(lam2,2),round(lam3,2)),":"),paste("K =",as.numeric(dim(Theta_hat)[3]))))
      }
    }
    aBIC[is.na(aBIC)] = 10^10
    n_lam2 = which(aBIC[(L3+1):(L3+L2)] == min(aBIC[(L3+1):(L3+L2)]))[1];lam2 = lambda2[n_lam2]
    # search lam1
    for (l1 in 1:L1) {
      lam1 = lambda1[l1];l = L3+L2+l1
      PP = FGGM.refit(data, K, lam1, lam2, lam3, initialization=F, initialize=out.initial, average=average, asymmetric=asymmetric, local_appro=local_appro, penalty = penalty)
      mu_hat=PP$mu;Theta_hat=PP$Xi;L.mat = PP$L.mat0;group = PP$group;prob = PP$prob0;aBIC[l] = PP$bic; member = PP$member
      Mu_hat.list[[l]]=mu_hat; Theta_hat.list[[l]]=Theta_hat; prob.list[[l]]=prob; L.mat.list[[l]] = L.mat; member.list[[l]]=member
      if(trace){
        print(c(cat(paste(l,"lam1 lam2 lam3 ="),c(round(lam1,2),round(lam2,2),round(lam3,2)),":"),paste("K =",as.numeric(dim(Theta_hat)[3]))))
      }
    }
    aBIC[is.na(aBIC)] = 10^10
    aBIC[aBIC==0] = abs(aBIC[aBIC!=0][1])*10
    n_lam1 = which(aBIC[(L3+L2+1):(L3+L2+L1)] == min(aBIC[(L3+L2+1):(L3+L2+L1)]))[1];lam1 = lambda1[n_lam1]
  }
  K.list <- rep(1,length(Theta_hat.list))
  for (l in 1:length(Theta_hat.list)) {
    K.list[l] <- as.numeric(dim(Theta_hat.list[[l]])[3])
  }
  aBIC[which(K.list == 1)] <- 10^10

  n_lam = which(aBIC == min(aBIC))[1]
  Opt_aBIC = min(aBIC)
  Opt_lambda = c(lam1,lam2,lam3)
  result = list(Opt_lambda=Opt_lambda,Mu_hat.list=Mu_hat.list,Theta_hat.list=Theta_hat.list,prob.list=prob.list,member.list=member.list,L.mat.list=L.mat.list,Opt_aBIC=Opt_aBIC,BIC=aBIC,Opt_num=n_lam)
  return(result)
}
