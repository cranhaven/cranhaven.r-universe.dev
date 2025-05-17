# discretize.jointly.R
#
# Created by Sajal Kumar
# Modified by Jiandong Wang
# Copyright (c) NMSU Song lab

#' Discretize Multivariate Continuous Data by a Cluster-Preserving Grid
#'
#' Discretize multivariate continuous data using a grid that captures the joint distribution via
#' preserving clusters in the original data
#'
#' @importFrom cluster silhouette
#' @importFrom stats kmeans
#' @importFrom fossil adj.rand.index
#' @importFrom stats dist
#' @importFrom dqrng dqsample
#' @importFrom Rdpack reprompt
#' @importFrom mclust Mclust mclustBIC
#' @importFrom Ckmeans.1d.dp MultiChannel.WUC ahist
#' @import Rcpp
#' @useDynLib GridOnClusters
#'
#' @param data a matrix containing two or more continuous variables.
#' Columns are variables, rows are observations.
#'
#' @param k either an integer, a vector of integers, or \code{Inf}, specifying 
#' different ways to find clusters in \code{data}. The default is a vector 
#' containing integers from 2 to 10. If 'k' is a single number, \code{data} will 
#' be grouped into into exactly 'k' clusters. If 'k' is an integer vector, an 
#' optimal 'k' is chosen from among the integers, that maximizes the average 
#' silhouette width. If 'k' is set to \code{Inf}, an optimal k is chosen among
#' 2 to \code{nrow(data)}. If \code{cluster_label} is specified, \code{k} is 
#' ignored.
#'
#' @param cluster_label a vector of user-specified cluster labels for each observation
#' in \code{data}. The user is free to choose any clustering.
#' If unspecified, k-means clustering is used by default.
#'
#' @param min_level integer or vector, signifying the minimum number of levels 
#' along each dimension. If a vector of size \code{ncol(data)}, then each element
#' will be mapped 1:1 to each dimension in order. If an integer, then all dimensions
#' will have the same minimum number of levels.
#'
#' @param grid_method the discretization method to be used. 
#' "Sort+split" will sort the cluster by cluster mean in each dimension and then
#'  split consecutive pairs only if the sum of the error rate of each cluster is
#'  less than or equal to 50%. It is possible that no grid line will be added
#'  in a certain dimension. The maximum number of lines is the number of 
#'  clusters minus one.
#'  "MultiChannel.WUC" will split each dimension by weighted with-in cluster
#'  sum of squared distances by "Ckmeans.1d.dp::MultiChannel.WUC". Applied in 
#'  each projection on each dimension. The channel of each point is defined by 
#'  its multivariate cluster label. 
#'  
#'  
#' @param cluster_method the clustering method to be used. Ignored if cluster labels 
#' are given
#' "kmeans+silhouette" will use k-means to cluster \code{data} and the average 
#' Silhouette score to select the number of clusters k.
#' "Ball+BIC" will use Mclust (modelNames = "VII") to cluster \code{data} and 
#' BIC score to select the number of cluster k.   
#' 
#'
#' @details 
#' 
#' The function implements algorithms described in \insertCite{Jwang2020BCB}{GridOnClusters}.
#' 
#'
#' @return
#'
#' A list that contains four items:
#' \item{\code{D}}{a matrix that contains the discretized version of the original \code{data}.
#' Discretized values are one(1)-based.}
#'
#' \item{\code{grid}}{a list of vectors containing decision boundaries for each variable/dimension.}
#'
#' \item{\code{clabels}}{a vector containing cluster labels for each observation in \code{data}.}
#'
#' \item{\code{csimilarity}}{a similarity score between clusters from joint discretization
#' \code{D} and cluster labels \code{clabels}. The score is the adjusted Rand index.}
#' 
#' @examples
#' # using a specified k
#' x = rnorm(100)
#' y = sin(x)
#' z = cos(x)
#' data = cbind(x, y, z)
#' discretized_data = discretize.jointly(data, k=5)$D
#'
#' # using a range of k
#' x = rnorm(100)
#' y = log1p(abs(x))
#' z = tan(x)
#' data = cbind(x, y, z)
#' discretized_data = discretize.jointly(data, k=c(3:10))$D
#' 
#' # using k = Inf
#' x = c()
#' y = c()
#' mns = seq(0,1200,100)
#' for(i in 1:12){
#'   x = c(x,runif(n=20, min=mns[i], max=mns[i]+20))
#'   y = c(y,runif(n=20, min=mns[i], max=mns[i]+20))
#' }
#' data = cbind(x, y)
#' discretized_data = discretize.jointly(data, k=Inf)$D
#'
#' # using an alternate clustering method to k-means
#' library(cluster)
#' x = rnorm(100)
#' y = log1p(abs(x))
#' z = sin(x)
#' data = cbind(x, y, z)
#'
#' # pre-cluster the data using partition around medoids (PAM)
#' cluster_label = pam(x=data, diss = FALSE, metric = "euclidean", k = 5)$clustering
#' discretized_data = discretize.jointly(data, cluster_label = cluster_label)$D
#'
#' @references
#' \insertAllCited{}
#' 
#' @author 
#' 
#' Jiandong Wang, Sajal Kumar and Mingzhou Song
#' 
#' @seealso
#'
#' See \link[Ckmeans.1d.dp]{Ckmeans.1d.dp} for discretizing univariate continuous data.
#'
#' @export
discretize.jointly = function(data, k=c(2:10), min_level = 1, 
                              cluster_method = c("Ball+BIC", "kmeans+silhouette", "PAM"), 
                              grid_method = c("Sort+split", "MultiChannel.WUC"), cluster_label=NULL){

  # check if data provided is a matrix
  if( !("matrix" %in% class(data)) && !("data.frame" %in% class(data))){
    stop("'data' must be a matrix or data.frame.")
  }

  # check if all columns in 'data' are numeric or integer
  dim_class = apply(data, 2, class)
  if(!all(dim_class == "numeric" | dim_class == "integer")){
    stop("All columns in 'data' should be numeric or integer.")
  }

  # 'data' should have at least 10 observations
  dim_data = dim(data)
  #if(dim_data[1] < 10){
  #  stop("'data' should have at least 10 observations,")
  #}

  # 'cluster_label' should either be null or integers (or numeric) matching nrow(data)
  if(!is.null(cluster_label) && length(cluster_label) != nrow(data)){
    stop("'cluster_label' should either be null or a vector with nrow(data) elements.")
  }

  if(!is.null(cluster_label) && !class(cluster_label) %in% c("numeric","integer")){
    stop("'cluster_label' should be either null or a numeric/integer vector.")
  }
  
  # if "min_level" is a list or a integer
  if(length(min_level)==1){
    min_level = rep(min_level, ncol(data))
  } else if(length(min_level) > 1 && length(min_level) != ncol(data)){
    stop("'min_level' should either be an integer or a vector of size ncol(data)")
  }
    
  # switching order, checking min_level < k after having a k
  
  # 'min_level' should smaller then the max of k
  if(max(k) < max(min_level)){
    k = c(min_level,min_level+5)
    warning("'min_level' should be in the range of k, k has been adapted")
   } 
  
  # the maximum of k range should be less then the number of point
  if(all(is.finite(k))){
    k = k[which(k<nrow(data))]  
  }
  
  #check method
  cluster_method = match.arg(cluster_method)
  grid_method = match.arg(grid_method)
  
  # if no cluster labels and methods are supplied, default to mclust(VII)
  if(is.null(cluster_label)){

    cluster_info =  cluster(data, k, cluster_method)

  } else { # cluster labels are supplied

    cluster_label = as.numeric(as.factor(cluster_label)) # making labels consecutive

    # # find medoids
    # centers = data[medoids(D = dist(data), cl = cluster_label),,drop=FALSE]

    cluster_info = list(#centers = as.matrix(centers),
                        clusters = cluster_label-1,
                        data = as.matrix(data), method = cluster_method)
  }

  if(grid_method == "MultiChannel.WUC"){  
    # prepare the weights
    # kstar = length(unique(cluster_info$clusters)) # MS 2022-01-17
    clusters.factor <- factor(cluster_info$clusters) # MS 2022-01-17
    kstar <- nlevels(clusters.factor)                # MS 2022-01-17
    weight = matrix(1/nrow(data), nrow = nrow(data), ncol = kstar)
    for (i in c(1:nrow(data))) {
      # weight[i, cluster_info$clusters[i]+1] = nrow(data)/(nrow(data)+1) # MS 2022-01-17
       weight[i, clusters.factor[i]] = nrow(data)/(nrow(data)+1) # MS 2022-01-17
    }
    grid_lines = vector("list", length = ncol(data))
    # for n dim
    for(dim in c(1:ncol(data))){
      result = Ckmeans.1d.dp::MultiChannel.WUC(x = data[,dim], y = weight, k = c(min_level[dim]:kstar))
      result$size = as.vector(table(result$cluster))
      res <- structure(result,class = "Ckmeans.1d.dp")
      
      res.hist = Ckmeans.1d.dp::ahist(res, data = data[,dim], style = "midpoints", plot = FALSE)
      
      lines = unique(res.hist$breaks)
      grid_lines[[dim]] = lines[-c(1,length(lines))]
    }
  }

  else if(grid_method == "Sort+split"){
    # get grid lines
    grid_lines = findgrid(cluster_info, length(unique(cluster_info$clusters)), nrow(data), ncol(data), min_level)
    
    # filter grid lines
    grid_lines = lapply(grid_lines, function(i){ 
      mx = max(i) 
      return(i[-which(i == mx)])
      })
  }

  # discretize data
  discr_data = discretize_data(data, grid_lines)

  # compute adjusted random index
  ndim_cluster_dist = discr_data[,1]
  for(i in 2:ncol(discr_data)){
    ndim_cluster_dist = paste0(ndim_cluster_dist,",",discr_data[,2])
  }
  cluster_similarity = adj.rand.index(as.numeric(as.factor(ndim_cluster_dist))-1, cluster_info$clusters)

  class = "GridOnClusters"
  
  res = structure(
    list(D=discr_data, grid=grid_lines, clabels=cluster_info$clusters+1, 
         csimilarity=cluster_similarity, method = cluster_info$method, 
         data = data),
    class = class)
  return(res)
}

#' Cluster Multivariate Data
#' 
#' The function obtains clusters from data using the given
#'   number of clusters, which may be a range.
#'   
#' @param data input continuous multivariate data
#' @param k the number(s) of clusters 
#' @param method the method for clustering
#'
#' @export
cluster = function(data, k, method){
  if(method == "Ball+BIC"){
    if(all(is.finite(k))){
      
      # use noisy Mclust
      mclust.res = Call_Noisy_Mclust(data, k)
      
      cluster_info = list(
        clusters = as.vector(as.numeric(as.factor(mclust.res$classification))-1),
        data = as.matrix(data), method = "Ball+BIC")
        
    } else {
      
      range_k = c(2:8)
      
      # use noisy Mclust
      mclust.res = Call_Noisy_Mclust(data, range_k)
      
      # find the number of best clusters
      BIC_collect = mclust.res$BIC[,1]
      bestk = as.numeric(names(which.max(BIC_collect)))
      ogk = range_k
      while(bestk == max(range_k)){
        
        range_k = c((max(range_k)+1):min((max(range_k)*2), nrow(data)))
        ogk = c(ogk, range_k)
        
        mclust.res = Call_Noisy_Mclust(data, range_k)
        
        BIC_collect = c(BIC_collect, mclust.res$BIC[,1])  
        bestk = as.numeric(names(which.max(BIC_collect)))
        
      }
      
      # # let the user know that the user has reached the best k
      # message(paste0("Reached best k=",bestk,
      #                ". Re-clustering data using the best k."))
      
      # re-cluster using best k
      mclust.res = Call_Noisy_Mclust(data, bestk)
      
      cluster_info = list(
        clusters = as.vector(as.numeric(as.factor(mclust.res$classification))-1),
        data = as.matrix(data), method = method)
      
    }
    
  } else if(method == "kmeans+silhouette"){
    # is k a single number or a range
    if(length(k) == 1 && is.finite(k)){
      
      # randomly generate centers
      centers = dqsample(which(!duplicated(data)), k)
      
      # get cluster information for 'k'
      kmeans.res = kmeans(data, centers = as.matrix(data[centers,]), 
                            iter.max = 100)
      
      # only keep cluster centers and labels
      cluster_info = list(#centers = as.matrix(kmeans.res$centers),
        clusters = as.vector(kmeans.res$cluster)-1,
        data = as.matrix(data), method = method)
      
    } else if(length(k) == 1 && !is.finite(k)) { # else if k is set to Inf
      
      data_dist = dist(data) # distance matrix for data
      
      # compute cluster info and silhouette score for the cluster range k, 
      # adaptively increasing k until optimal or each sample is placed in their 
      # own cluster
      
      # find unique number of samples
      unq = sum(!duplicated(data))
      if(unq > 8){
        range_k = c(2:8)  
      } else {
        range_k = c(2:unq)
      }
      
      # compute cluster info for original k range
      cluster_info = lapply(range_k, function(i){
        centers = dqsample(which(!duplicated(data)), i)
        data_clust = kmeans(data, centers = as.matrix(data[centers,]),
                            iter.max = 100)
        return(list(data_clust$cluster, data_clust$centers, 
                    mean(silhouette(data_clust$cluster, dist=data_dist)[,3])))
      })
      
      # find the max silhouette
      silhouette_scr = unlist(lapply(cluster_info, function(x){
        return(x[[3]])
      }), use.names = FALSE)
      max_silhouette = max(which.max(silhouette_scr)) # take the bigger k out of those with equal silhouette scores
      
      # while best k is equal to max k, keep computing scores
      bestk = range_k[max_silhouette]
      ogk = range_k
      while(bestk == max(range_k) && unq > 8){
        
        range_k = c((max(range_k)+1):min((max(range_k)*2), nrow(data)))
        ogk = c(ogk, range_k)
        
        # compute cluster info for original k range
        n_cluster_info = lapply(range_k, function(i){
          centers = dqsample(which(!duplicated(data)), i)
          data_clust = kmeans(data, centers = as.matrix(data[centers,]),
                              iter.max = 100)
          return(list(data_clust$cluster, data_clust$centers, 
                      mean(silhouette(data_clust$cluster, dist=data_dist)[,3])))
        })
        
        # append new cluster info to original
        cluster_info = c(cluster_info, n_cluster_info)
        
        # find the max silhouette
        silhouette_scr = unlist(lapply(cluster_info, function(x){
          return(x[[3]])
        }), use.names = FALSE)
        max_silhouette = max(which.max(silhouette_scr)) # take the bigger k out of those with equal silhouette scores
        
        # find best k
        bestk = ogk[max_silhouette]
      }
      
      # only keep cluster centers and labels for the 'k' with max silhouette
      cluster_info = list(#centers = as.matrix(cluster_info[[max_silhouette]][[2]]),
        clusters = as.vector(cluster_info[[max_silhouette]][[1]]-1),
        data = as.matrix(data), method = method)
      
    } else { # else k is a vector
      
      data_dist = dist(data) # distance matrix for data
      
      # compute cluster info and silhouette score for the cluster range k
      cluster_info = lapply(k, function(i){
        centers = dqsample(which(!duplicated(data)), i)
        data_clust = kmeans(data, centers = as.matrix(data[centers,]),
                            iter.max = 100)
        return(list(data_clust$cluster, data_clust$centers, 
                    mean(silhouette(data_clust$cluster, dist=data_dist)[,3])))
      })
      
      # find the max silhouette
      silhouette_scr = unlist(lapply(cluster_info, function(x){
        return(x[[3]])
      }), use.names = FALSE)
      max_silhouette = max(which.max(silhouette_scr)) # take the bigger k out of those with equal silhouette scores
      
      # only keep cluster centers and labels for the 'k' with max silhouette
      cluster_info = list(#centers = as.matrix(cluster_info[[max_silhouette]][[2]]),
        clusters = as.vector(cluster_info[[max_silhouette]][[1]]-1),
        data = as.matrix(data), method = method)
      
    }
  }
  return(cluster_info)
}

# for internal use
# takes n-dimensional 'data' and clusters it into 'k' groups using Mclust
# adding noise whenever necessary
Call_Noisy_Mclust = function(data, k){
  
  mclust.res = mclust::Mclust(data, G = k, modelNames = "VII", 
                              verbose = FALSE)
  
  # add additional information to Mclust (non noisy data)
  if(!is.null(mclust.res)){
    mclust.res$isNoisy = FALSE  
  }
  
  # Mclust can return NULL result for sparse discrete data
  amt = 0.5
  noisy_data = data # don't change the original input
  while(is.null(mclust.res)){
    
    # add a little bit of noise to enable clustering
    for(j in 1:ncol(noisy_data)){
      noisy_data[,j] = jitter(as.numeric(noisy_data[,j]), amount = amt)
    }
    
    mclust.res = mclust::Mclust(noisy_data, G = k, modelNames = "VII", 
                                verbose = FALSE)
    
    # add additional information to Mclust (noisy data)
    if(!is.null(mclust.res)){
      mclust.res$isNoisy = TRUE
    }
    
    # double the amount of noise for the next iteration (if any)
    amt = amt*2
    
  }
  
  if(mclust.res$isNoisy){
    warning(paste0("Mclust could not cluster 'data', jitter noise 
            (factor=1, amount=",(amt/2),") was added."))
  }
  
  return(mclust.res)
  
}

# for internal use
# takes n-dimesional 'data' and gridlines to quantify each dimension
discretize_data = function(data, gridlines){
  
  # use gridlines for each dimension
  for(i in 1:ncol(data)){
    
    if(length(unique(gridlines))==0){
      discr = rep(1, nrow(data))
    }else{
      discr = rep(length(gridlines[[i]])+1, nrow(data))
      gridlines[[i]] = sort(gridlines[[i]])
      for(j in 1:length(gridlines[[i]])){ # determine discretization levels
        if(j == 1) {
          discr[data[,i] < gridlines[[i]][j]] = 1
        } else {
          discr[data[,i] < gridlines[[i]][j] & data[,i] >= gridlines[[i]][j-1]] = j
        }
      }
    }
    data[,i] = discr
  }
  
  return(data)
}
