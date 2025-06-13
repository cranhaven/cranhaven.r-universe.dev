
#' selectK.
#' 
#' @description
#' The function selectK is used to select the best K according to BIC or Modified BIC criterion. 
#'
#' @details The function selectK is used to select the best K according to BIC or Modified BIC criterion. 
#' @param SCobject is an object generated from SC.MEB function.
#' @param K_set is a integer vector used in SC.MEB. The default is 2:10
#' @param criterion is a character specifying the criterion for selecting K. The default value is BIC. The alternative value MBIC can also be used.
#' @param c is a positive value in the modified BIC. The default is 1.
#' Here we briefly explain how to choose the parameter c in the modified BIC. In general, For the ST or Visium dataset, it often ranges from 0.4 to 1 while for the MERFISH dataset with large number of cells, it often becomes larger, for example 10,20. Most importantly, SC-MEB is fast, scaling well in terms of sample size, which allow the user to tune the c based on their prior knowledge about the tissues or cells.
#' @return a list contains two items. one is for the best K and the other is the clustering labels of n spots.
#' @examples 
#' y = matrix(rnorm(50, 0, 1), 25,2)
#' pos = cbind(rep(1:5, each=5), rep(1:5, 5))
#' Adj_sp = getneighborhood_fast(pos, 1.2)
#' beta_grid = c(0.5,1)
#' K_set = 2:3
#' out = SC.MEB(y, Adj_sp, beta_grid, K_set, TRUE, 2)
#' selectK(out, K_set)
#' @export
selectK <- function(SCobject, K_set = 2:10, criterion = "BIC",  c = 1){
  
  out = list()
  num_K = length(SCobject)/9
  p = dim(SCobject[[6]])[1]
  n = dim(SCobject[[2]])[1]
  
  if (criterion == "BIC"){
    BIC = matrix(0, num_K, 1)
    for (i in 1:num_K){
      q = K_set[i]
      dr <- q * p + p*(p + 1)/2 * q
      BIC[i] <- -2 * SCobject[[9*i-1]] - log(n) * dr
    }
    best_K_BIC = K_set[which.max(BIC)]
    out$best_K_BIC = best_K_BIC
    out$best_K_label = SCobject[[9*which.max(BIC)-8]]
    return(out)
    
  }else if (criterion == "MBIC"){
    
    MBIC = matrix(0, num_K, 1)
    for (i in 1:num_K){
      q = K_set[i]
      dr <- q * p + p*(p + 1)/2 * q
      MBIC[i] <- -2 * SCobject[[9*i-1]] - log(n) * dr *log(log(n + p))*c
    }
    best_K_MBIC = K_set[which.max(MBIC)]
    out$best_K_MBIC = best_K_MBIC
    out$best_K_label = SCobject[[9*which.max(MBIC)-8]]
    return(out)
  }
  
}


#' ClusterPlot.
#' 
#' @description
#' The function ClusterPlot is used to Visualize spatial clusters. 
#'
#' @details The function ClusterPlot is used to Visualize spatial clusters.
#' @param out is the output of function selectK.
#' @param pos is a n-by-2 matrix of position.
#' @param size is a positive value for characterizing the size of point in the plot, which is the same as size in ggplot2.
#' @param shape is a positive value for characterizing the shape of point in the plot, which is the same as shape in ggplot2.
#' @return a ggplot2 object.
#' @examples 
#' pos = cbind(rep(1:5, each=5), rep(1:5, 5))
#' out = list()
#' out[[1]] = ""
#' out[[2]] = rep(1:5, each = 5)
#' ClusterPlot(out, pos)
#' @export
ClusterPlot <- function(out, pos, size = 5, shape = 15){
  
  imagerow = pos[,1]
  imagecol = pos[,2]
  
  cluster = factor(out[[2]])
  
  dat = data.frame(imagerow,imagecol,cluster)
  names(dat)= c("imagerow","imagecol","cluster")
  
  p1 <- ggplot(dat, aes(x=imagerow, y=imagecol, color=cluster)) +
    geom_point(size = size, shape = shape) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          legend.key.size = unit(1, "cm"),
          legend.text = element_text(size=20,face="bold"),
          legend.title = element_text(size=25,face="bold"))
  return(p1)
}


#' selectKPlot.
#' 
#' @description
#' The function selectKPlot is used to demonstrate the scatter plot of BIC or Modified BIC vs K for selecting the best K.  
#'
#' @details The function selectKPlot is used to demonstrate the scatter plot of BIC or Modified BIC vs K for selecting the best K.  
#' @param SCobject is a object generated from SC.MEB function.
#' @param K_set is the corresponding K_set used in your previous function SC.MEB.
#' @param criterion is a character specifying the criterion for selecting K. The default is BIC, the alternative criterion MBIC can also be used.
#' @param c is a positive value in modified BIC. The default is 1.  
#' Here we briefly explain how to choose the parameter c in the modified BIC. In general, For the ST or Visium dataset, it often ranges from 0.4 to 1 while for the MERFISH dataset with large number of cells, it often becomes larger, for example 10,20. Most importantly, SC-MEB is fast, scaling well in terms of sample size, which allow the user to tune the c based on their prior knowledge about the tissues or cells.
#' @return a ggplot2 object.
#' @examples 
#' y = matrix(rnorm(50, 0, 1), 25,2)
#' pos = cbind(rep(1:5, each=5), rep(1:5, 5))
#' Adj_sp = getneighborhood_fast(pos, 1.2)
#' beta_grid = c(0.5,1)
#' K_set = 2:3
#' out = SC.MEB(y, Adj_sp, beta_grid, K_set, TRUE, 2)
#' selectKPlot(out, K_set)
#' @export
selectKPlot <- function(SCobject, K_set = 2:10, criterion = "BIC",  c = 1){
  num_K = length(SCobject)/9
  p = dim(SCobject[[6]])[1]
  n = dim(SCobject[[2]])[1]
  if (criterion == "BIC"){
    BIC = matrix(0, num_K, 1)
    for (i in 1:num_K){
      q = K_set[i]
      dr <- q * p + p*(p + 1)/2 * q
      BIC[i] <- -2 * SCobject[[9*i-1]] - log(n) * dr
    }
    bic = data.frame(BIC)   
    bic$K = K_set
    ggplot(bic, aes(x=K_set, y=BIC)) + geom_line() + geom_point()
    
  }else if (criterion == "MBIC"){
    
    MBIC = matrix(0, num_K, 1)
    for (i in 1:num_K){
      q = K_set[i]
      dr <- q * p + p*(p + 1)/2 * q
      MBIC[i] <- -2 * SCobject[[9*i-1]] - log(n) * dr *log(log(n + p))*c
    }
    mbic = data.frame(MBIC)   
    mbic$K = K_set
    ggplot(mbic, aes(x=K_set, y=MBIC)) + geom_line() + geom_point()
  }
  
  
}


#' SC.MEB.
#' 
#' @description
#' SC.MEB implements the model SC-MEB, spatial clustering with hidden Markov random field using empirical Bayes. 
#'
#' @details SC.MEB can implements the model SC-MEB in parallel which can improve the speed of the computation.
#' @param y is n-by-d PCs.
#' @param Adj_sp is a sparse matrix of neighborhood. It is often generated from function find_neighbors2 or getneighborhood_fast.  
#' @param K_set is an integer vector specifying the numbers of mixture components (clusters) for which the BIC is to be calculated. The default is K = 2:10. 
#' @param beta_grid is a numeric vector specifying the smoothness parameter of Random Markov Field. The default is seq(0,4,0.2). 
#' @param parallel is a logical value to decide whether the function SC.MEB run in parallel. The default is TRUE.
#' @param num_core is an integer value to decide how many cores are used to run SC.MEB in parallel.
#' @param PX is a logical value to decide whether to use parameter expansion in EM algorithm
#' @param maxIter_ICM is the maximum iteration of ICM algorithm. The default is 10. 
#' @param maxIter is the maximum iteration of EM algorithm. The default is 50.
#' @return a list, We briefly explain the output of the SC.MEB. 
#'  
#' The item 'x' contains clustering results.
#' 
#' The item 'gam' is the posterior probability matrix.
#' 
#' The item 'ell' is the opposite log-likelihood. 
#' 
#' The item 'mu' is the mean of each component.
#' 
#' The item 'sigma' is the variance of each component.
#' @examples 
#' y = matrix(rnorm(50, 0, 1), 25,2)
#' pos = cbind(rep(1:5, each=5), rep(1:5, 5))
#' Adj_sp = getneighborhood_fast(pos, 1.2)
#' beta_grid = c(0.5,1)
#' K_set = 2:3
#' out = SC.MEB(y, Adj_sp, beta_grid, K_set, TRUE, 2)
#' @references Yang Y, Shi X, Zhou Q, et al. SC-MEB: spatial clustering with hidden Markov random field using empirical Bayes[J]. bioRxiv, 2021.
#' @import mclust
#' @export
SC.MEB <- function(y, Adj_sp, beta_grid = seq(0,4,0.2), K_set= 2:10, parallel=TRUE, num_core = 5, PX = TRUE, maxIter_ICM=10, maxIter=50){
  
  if(parallel==TRUE){
      cl <- makeCluster(num_core)
      clusterExport(cl, list("Mclust", "ICMEM", "mclustBIC"))
      cat("Starting parallel computing...")
      # Run
      icMat <- parSapply(cl, X=K_set, parafun, y=y, Adj=Adj_sp, beta_grid = beta_grid, PX = TRUE, maxIter_ICM=10, maxIter=50)
      stopCluster(cl)
  }else{
    icMat = list()
    for (G in 1:length(K_set)){
     icMat_tmp <-  parafun(y, Adj_sp, K_set[G], beta_grid = beta_grid, PX = TRUE, maxIter_ICM=10, maxIter=50)
     icMat = cbind(icMat, icMat_tmp)
    }
  }
  return(icMat)
}


#' parafun.
#' 
#' @description
#' The function parafun implements the model SC-MEB for fixed number of clusters and a sequence of beta with initial value from Gaussian mixture model
#'
#' @details The function parafun implements the model SC-MEB for fixed number of clusters and a sequence of beta with initial value from Gaussian mixture model 
#' @param y is n-by-d PCs.
#' @param Adj is a sparse matrix of neighborhood. 
#' @param G is an integer specifying the numbers of clusters.
#' @param beta_grid is a numeric vector specifying the smoothness parameter of Random Markov Field. The default is seq(0,4,0.2). 
#' @param PX is a logical value specifying the parameter expansion in EM algorithm.
#' @param maxIter_ICM is the maximum iteration of ICM algorithm. The default is 10. 
#' @param maxIter is the maximum iteration of EM algorithm. The default is 50.
#' @return a list, We briefly explain the output of the SC.MEB. 
#'  
#' The item 'x' storing clustering results.
#' 
#' The item 'gam' is the posterior probability matrix.
#' 
#' The item 'ell' is the opposite log-likelihood. 
#' 
#' The item 'mu' is the mean of each component.
#' 
#' The item 'sigma' is the variance of each component.
#' 
#' @examples 
#' y = matrix(rnorm(50, 0, 1), 25,2)
#' pos = cbind(rep(1:5, each=5), rep(1:5, 5))
#' Adj_sp = getneighborhood_fast(pos, 1.2)
#' beta_grid = c(0.5,1)
#' G = 2
#' out = parafun(y, Adj_sp, G, beta_grid)
#' @import mclust
#' @export
parafun <- function(y, Adj, G, beta_grid = seq(0,4,0.2), PX = TRUE, maxIter_ICM=10, maxIter=50){
  fit_int = Mclust(y, G = G)
  x_gmm <- fit_int$classification
  mu_int <- unname(fit_int$parameter$mean)
  sigma_int <- unname(fit_int$parameter$variance$sigma) 
  alpha <- -log(fit_int$parameter$pro)*0
  reslist <- ICMEM(y = y, x_int = x_gmm, Adj = Adj, mu_int = mu_int, sigma_int = sigma_int, alpha = alpha, beta_grid = beta_grid, PX = TRUE, maxIter_ICM = 10, maxIter = 50) 
  return(reslist)
}

#' ICMEM.
#' 
#' @description
#' The function ICMEM was used to conduct spatial clustering with hidden Markov random field for a sequence of beta and fixed number of clusters
#'
#' @details The function ICMEM was used to conduct spatial clustering with hidden Markov random field for fixed beta and fixed number of clusters
#' @param y is a matrix of PCs containing gene expression.
#' @param x_int is a vector of initial cluster label.
#' @param Adj is a matrix containing neighborhood information generated by find_neighbors2.
#' @param mu_int is a initial mean vector. we often generated it by Gaussian mixture model. 
#' @param sigma_int is a initial co-variance matrix. we often generated it by Gaussian mixture model. 
#' @param alpha is a intercept.
#' @param beta_grid is a sequence of smoothing parameter that can be specified by user.
#' @param PX is a logical value specifying the parameter expansion in EM algorithm.
#' @param maxIter_ICM is the maximum iteration of ICM algorithm.
#' @param maxIter is the maximum iteration of EM algorithm.
#' @return a list.
#' 
#' The item 'x' is the clustering result. 
#' 
#' The item 'gam' is the posterior probability matrix.
#'  
#' The item 'ell' is the opposite log-likelihood. 
#' 
#' The item 'mu' is the mean of each component.
#' 
#' The item 'sigma' is the variance of each component.
#' @examples 
#' y = matrix(rnorm(50, 0, 1), 25,2)
#' pos = cbind(rep(1:5, each=5), rep(1:5, 5))
#' Adj = getneighborhood_fast(pos, 1.2)
#' beta_grid = c(0.5,1)
#' G = 2
#' fit_int = Mclust(y, G = G)
#' x_gmm <- fit_int$classification
#' mu_int <- unname(fit_int$parameter$mean)
#' sigma_int <- unname(fit_int$parameter$variance$sigma) 
#' alpha <- -log(fit_int$parameter$pro)*0
#' reslist <- ICMEM(y = y, x_int = x_gmm, Adj = Adj, mu_int = mu_int, sigma_int = sigma_int,
#' alpha = alpha, beta_grid = beta_grid, 
#' PX = TRUE, maxIter_ICM = 10, maxIter = 50) 
#' @export
ICMEM <- function(y, x_int, Adj, mu_int, sigma_int, alpha, beta_grid, PX, maxIter_ICM, maxIter) {
  .Call(`_SC_MEB_ICMEM`, y, x_int, Adj, mu_int, sigma_int, alpha, beta_grid, PX, maxIter_ICM, maxIter)
}

