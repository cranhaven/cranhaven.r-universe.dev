#' Graph Inference from Noisy Data by Multiple Testing
#' @description
#' The `main_noisySBM()` function is a core component of the `noisysbmGGM` package,
#' responsible for applying the greedy algorithm to estimate model parameters, perform node clustering,
#' and conduct a multiple testing procedure to infer the underlying graph. This function is versatile,
#' offering various options and providing useful outputs for further analysis
#'
#' @export
#' @param X A p-square matrix containing the data
#' @param NIG A Boolean. If FALSE (by default), the variance under the alternative hypothesis in assumed to be known. If TRUE, the variances under the alternatives are unknown and estimated with the NIG method
#' @param sigma0 standard deviation under the null hypothesis (by default sigma0=1)
#' @param sigma1  standard deviation under the alternative hypothesis in the non-NIG method (by default sigma1=1)
#' @param alpha Level of significance of the multiple testing procedure (by default alpha=0.1)
#' @param Qup  Maximal number of cluster (by default Qup =10)
#' @param nbCores Nb of cores to be used during calculations (by default nbCores=parallel::detectCores())
#' @param nbOfZ  Nb of initialization (by default nbOfZ=12)
#' @param percentageOfPerturbation perturbation during initialization  (by default percentageOfPerturbation=0.3)
#' @param threshold Threshold use when updating the latent graphs structure from l-values (by default threshold=0.5)
#' @param Nbrepet  Number of times the algorithm is repeated (by default Nbrepet=2)
#' @param n0 Hyperparameter (by default n0=1)
#' @param eta0 Hyperparameter (by default eta0=1)
#' @param zeta0 Hyperparameter (by default zeta0=1)
#' @param rho Hyperparameter of the non-NIG method (by default rho=1)
#' @param tau Hyperparameter of the non-NIG method (by default tau=1)
#' @param a Hyperparameter of the NIG method (by default a=0)
#' @param b Hyperparameter of the NIG method (by default b=1)
#' @param c Hyperparameter of the NIG method (by default c=1)
#' @param d Hyperparameter of the NIG method (by default d=1)
#' @param verbatim print information messages  

#'
#' @return \item{\code{A}}{the adjacency matrix of the inferred graph}
#'  \item{\code{Z}}{the inferred clustering}
#'  \item{\code{theta}}{the parameters of the noisySBM at the end}
#'  \item{\code{Q}}{the number of clusters at the end}
#'
#' @examples
#' main_noisySBM(NSBMtest$dataMatrix,NIG=TRUE,Qup=10,nbOfZ=1,nbCores=1)
main_noisySBM <-function(X,NIG=FALSE,threshold=0.5, Nbrepet=2, rho=NULL,tau=NULL, a=NULL,b=NULL,c=NULL,d=NULL, n0=1, eta0=1, zeta0=1, alpha=0.1, Qup=NULL, nbCores=parallel::detectCores(), nbOfZ=12, sigma0=1, sigma1=1,percentageOfPerturbation=0.3,verbatim=TRUE){
  p=length(X[1,])
  if(is.null(Qup)) Qup=10
  if(length(X[,1])!=p){
    stop("X must be a square matrix")
  }
  
  ind.all=listNodePairs(p, directed=FALSE)
  
  if(NIG){
    if(is.null(a)) a=0
    if(is.null(b)) b=1
    if(is.null(c)) c=1
    if(is.null(d)) d=1
    dataVec=X[lower.tri(X)]
    result=mainSearchOpti_NIG(X, Qup=Qup,  threshold=threshold, Nbrepet=Nbrepet, nbCores=nbCores, nbOfZ=nbOfZ, percentageOfPerturbation=percentageOfPerturbation, a=a, b=b, c=c, d=d, n0=n0, eta0=eta0, zeta0=zeta0,sigma0=sigma0, fast=TRUE,verbatim=verbatim)
    if (result$Q > 1){
      test=Merge_Nondirige_NIG(dataVec, result$Z, result$Zmatrix, result$Q, result$Rho, result$A,result$theta, threshold=threshold, a=a, b=b, c=c, d=d, n0=n0, eta0=eta0, zeta0=zeta0)
      result$A = InferGraph(test$Rhomerge, test$thetamerge, test$Zmerge, dataVec, alpha)
      result$Q=test$Qmerge
      result$Z=test$Zmerge
      result$theta=test$thetamerge
    }
    else {
      result$A = InferGraph(result$Rho, result$theta, result$Z, dataVec,  alpha)
    }
  }
  else{
    if(is.null(rho)) rho=1
    if(is.null(tau)) tau=1
    dataVec=X[lower.tri(X)]
    result=mainSearchOpti(X, Qup=Qup,  threshold=threshold, Nbrepet=Nbrepet, nbCores=nbCores, nbOfZ=nbOfZ, percentageOfPerturbation=percentageOfPerturbation, rho=rho, tau=tau, n0=n0, eta0=eta0, zeta0=zeta0, sigma0=sigma0, sigma1=sigma1, fast=TRUE,verbatim=verbatim)
    if (result$Q > 1){
      test=Merge_Nondirige(dataVec, result$Z, result$Zmatrix, result$Q, result$Rho, result$A,result$theta, threshold=threshold, rho=rho, tau=tau, n0=n0, eta0=eta0, zeta0=zeta0)
      result$A = InferGraph(test$Rhomerge, test$thetamerge, test$Zmerge, dataVec, alpha)
      result$Q=test$Qmerge
      result$Z=test$Zmerge
      result$theta=test$thetamerge
      
    }
    else {
      result$A = InferGraph(result$Rho, result$theta, result$Z, dataVec,  alpha)
    }
  }
  return(list(A=result$A, Z=result$Z, theta=result$theta, Q=result$Q))
}

#' GGM Inference from Noisy Data by Multiple Testing using SILGGM and Drton test statistics
#' @description
#' The `main_noisySBM_GGM()` function is a key feature of the `noisysbmGGM` package,
#' dedicated to Gaussian Graphical Model (GGM) inference. This function takes an $n$-sample of a Gaussian vector
#' of dimension $p$ and provides the GGM associated with the partial correlation structure of the vector.
#' GGM inference is essential in capturing the underlying relationships between the vector's coefficients,
#' helping users uncover meaningful interactions while controlling the number of false discoveries.
#'
#' @export
#' @param X A n by p matrix containing a n-sample of a p-vector
#' @param Meth Choice of test statistics between "Ren", "Jankova_NW", "Jankova_GL", "Liu_SL", "Liu_L", and "zTransform" (warning "zTransform" only work if n>p)
#' @param NIG A Boolean (automatically chosen according to the selected method : NIG=FALSE except for "Liu_SL" and "Liu_L" test statistics as input). If FALSE, the variance under the alternative hypothesis in assumed to be known. If TRUE, the variances under the alternatives are unknown and estimated with the NIG method. 
#' @param sigma0 standard deviation under the null hypothesis (by default sigma0=1)
#' @param sigma1 standard deviation under the alternative hypothesis in the non-NIG method (by default sigma1=1)
#' @param alpha Level of significance of the multiple testing procedure (by default alpha=0.1)
#' @param Qup  Maximal number of cluster (by default Qup =10)
#' @param nbCores Nb of cores to be used during calculations (by default nbCores=parallel::detectCores())
#' @param nbOfZ  Nb of initialization (by default nbOfZ=12)
#' @param percentageOfPerturbation perturbation during initialization  (by default percentageOfPerturbation=0.3)
#' @param threshold Threshold use when updating the latent graphs structure from l-values (by default threshold=0.5)
#' @param Nbrepet  Number of times the algorithm is repeated (by default Nbrepet=2)
#' @param n0 Hyperparameter (by default n0=1)
#' @param eta0 Hyperparameter (by default eta0=1)
#' @param zeta0 Hyperparameter (by default zeta0=1)
#' @param rho Hyperparameter of the non-NIG method (by default rho=1)
#' @param tau Hyperparameter of the non-NIG method (by default tau=1)
#' @param a Hyperparameter of the NIG method (by default a=0)
#' @param b Hyperparameter of the NIG method (by default b=1)
#' @param c Hyperparameter of the NIG method (by default c=1)
#' @param d Hyperparameter of the NIG method (by default d=1)
#' @param verbatim print information messages  

#'
#' @return \item{\code{A}}{the adjacency matrix of the inferred graph}
#'  \item{\code{Z}}{the inferred clustering}
#'  \item{\code{theta}}{the parameters of the noisySBM at the end}
#'  \item{\code{Q}}{the number of clusters at the end}
#'#' @examples
#' main_noisySBM_GGM(GGMtest$dataMatrix,Meth="Ren",NIG=TRUE,Qup=10,nbOfZ=1)
#'
#' @seealso main_noisySBM
main_noisySBM_GGM <-function(X, Meth="Ren", NIG=NULL, threshold=0.5, Nbrepet=2, rho=NULL,tau=NULL, a=NULL,b=NULL,c=NULL,d=NULL, n0=1, eta0=1, zeta0=1, alpha=0.1, Qup=NULL, nbCores=parallel::detectCores(), nbOfZ=12, sigma0=1, sigma1=1,percentageOfPerturbation=0.3,verbatim=TRUE){
  if(Meth=="Ren"){
    outlist_Ren <- SILGGM::SILGGM(X, method = "B_NW_SL")
    dataMatrix=outlist_Ren$z_score_precision
    if(is.null(NIG)) NIG=FALSE}
  else if(Meth=="Jankova_NW"){
    outlist_Jankova_NW <- SILGGM::SILGGM(X, method = "D-S_NW_SL")
    dataMatrix=outlist_Jankova_NW$z_score_precision
    if(is.null(NIG)) NIG=FALSE}
  else if(Meth=="Jankova_GL"){
    outlist_Jankova_GL <- SILGGM::SILGGM(X, method = "D-S_GL")
    dataMatrix=outlist_Jankova_GL$z_score_precision
    if(is.null(NIG)) NIG=FALSE}
  else if(Meth=="Liu_SL"){
    outlist_Liu_SL <- SILGGM::SILGGM(X, method = "GFC_SL")
    dataMatrix=outlist_Liu_SL$T_stat
    if(is.null(NIG)) NIG=TRUE}
  else if(Meth=="Liu_L"){
    outlist_Liu_L <- SILGGM::SILGGM(X, method = "GFC_L")
    dataMatrix=outlist_Liu_L$T_stat
    if(is.null(NIG)) NIG=TRUE}
  else if(Meth=="zTransform"){
    if(dim(X)[1]>dim(X)[2]){
      warning("To use zTransform method n should be superior to p+1")
    }
    dataMatrix=zTransform(X)
    if(is.null(NIG)) NIG=FALSE}
  else{stop("Unknown method")}
  
  return(main_noisySBM(dataMatrix,NIG=NIG,threshold=threshold, Nbrepet=Nbrepet, rho=rho,tau=tau, a=a,b=b,c=c,d=d, n0=n0, eta0=eta0, zeta0=zeta0, alpha=alpha, Qup=Qup, nbCores=nbCores, nbOfZ=nbOfZ, sigma0=sigma0, sigma1=sigma1,percentageOfPerturbation=percentageOfPerturbation,verbatim=verbatim))
}


#' plot the data matrix, the inferred graph and/or the true binary graph
#'
#' @param dataMatrix observed data matrix
#' @param inferredGraph graph inferred by the multiple testing procedure via graphInference()
#' @param binaryTruth true binary graph
#'
#' @return a list of FDR and TDR values, if possible
#' @export
plotGraphs <- function(dataMatrix=NULL, inferredGraph=NULL, binaryTruth=NULL){
  res <- NULL
  if (!is.null(dataMatrix))
    stats::heatmap(dataMatrix, Rowv=NA, Colv=NA, symm=TRUE, col = RColorBrewer::brewer.pal(9,'YlGn'),
                   xlab='data matrix')
  xlab2 <- 'inferred graph'
  xlab3 <- 'true binary graph'
  if ((!is.null(binaryTruth))&(!is.null(inferredGraph))){
    truthVec <- binaryTruth[lower.tri(binaryTruth)]
    inferredGraphVec <- inferredGraph[lower.tri(inferredGraph)]
    FDR <- sum(inferredGraphVec[truthVec==0])/ sum(inferredGraphVec)
    TDR <- sum(inferredGraphVec[truthVec==1])/ sum(truthVec==1)
    xlab2 <- paste('inferred graph, FDR=', round(FDR, digits=3), sep='')
    xlab3 <- paste('true binary graph, TDR=', round(TDR, digits=3), sep='')
    res <- list(FDR=FDR, TDR=TDR)
  }
  if (!is.null(inferredGraph))
    stats::heatmap(inferredGraph, Rowv=NA, Colv=NA, symm=TRUE, col = RColorBrewer::brewer.pal(9,'YlGn'),
                   xlab=xlab2)
  if (!is.null(binaryTruth))
    stats::heatmap(binaryTruth, Rowv=NA, Colv=NA, symm=TRUE, col = RColorBrewer::brewer.pal(9,'YlGn'),
                   xlab=xlab3)
  return(res)
}
