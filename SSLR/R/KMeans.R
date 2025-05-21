
#Kmeans


init_clusters_kmeans <- function(X, n_clusters, y){
  centers<-  X[sample(1:nrow(X), n_clusters, replace = FALSE),]
  rownames(centers) <- NULL
  centers
}



init_clusters_seeded_kmeans <- function(X, n_clusters, y){
  centers <- mean_centers(X,y, n_clusters)
  rownames(centers) <- NULL
  centers
}


assing_clusters_kmeans <- function(X, y, clusters_centers, distance = "euclidean"){

  labels <- rep(-1, length(y))

  for(idx_label in 1:length(labels)){

    result <- c()

    for(idx_cluster in 1:nrow(clusters_centers)){

      result <- c(result, dist(rbind(X[idx_label,],clusters_centers[idx_cluster,]), method = distance)[1])

    }

    labels[idx_label] <- which.min(result)

  }

  return(labels)

}


assing_clusters_constrained_kmeans <- function(X, y, clusters_centers, distance = "euclidean"){

  labels <- rep(-1, length(y))

  for(idx_label in 1:length(labels)){

    result <- c()

    if(!is.na(y[idx_label])){
      labels[idx_label] <- y[idx_label]
    }

    else{


      for(idx_cluster in 1:nrow(clusters_centers)){

        result <- c(result, dist(rbind(X[idx_label,],clusters_centers[idx_cluster,]), method = distance)[1])

      }

      labels[idx_label] <- which.min(result)

    }
  }

  return(labels)

}




mean_centers <- function(X,y, n_centers){

  start_centroids <- list()

  for(cluster in 1:n_centers){

    #Get data for index cluster
    data_cluster <- X[y == cluster & !is.na(y),]

    start_centroids[[cluster]] <- as.numeric(colMeans(data_cluster))

  }

  start_centroids <- (do.call(rbind, start_centroids))
  start_centroids

}


fit_kmeans_total <- function(X, Y, n_clusters = NULL, max_iter = 10, method = "euclidean",
                      init_clusters = init_clusters_kmeans, assing_clusters = assing_clusters_kmeans,
                      estimate_centers = mean_centers){


  levels_y <- levels(Y)
  if(is.null(n_clusters)){
    n_clusters <- length(levels_y)
  }

  y <- as.numeric(Y)

  clusterHistory <- vector(max_iter, mode="list")
  centerHistory <- vector(max_iter, mode="list")




  X <- as.matrix(X)

  #Init clusters
  clusters_centers <- init_clusters(X,n_clusters, y)

  for(iter in 1:max_iter){

    #Previous centers
    prev_cluster_centers <- clusters_centers


    #assign clusters
    labels <- assing_clusters(X, y, prev_cluster_centers, distance = method)


    #Apply mean
    clusters_centers <- estimate_centers(X, labels, n_clusters)

    # Saving history
    clusterHistory[[iter]] <- labels
    centerHistory[[iter]] <- clusters_centers

    converged <- FALSE
    if(isTRUE(all.equal(prev_cluster_centers, clusters_centers, tolerance = 1.5e-8)))
      converged <- TRUE

    if(converged) break

  }

  totss <- sum(scale(X, scale = FALSE)^2)
  structure(list(cluster = labels, centers = clusters_centers, totss = totss),
            class = "kmeans")

  #return(clusters_centers)


}




fit_kmeans <- function(X, y,  n_clusters = NULL, max_iter = 10, method = "euclidean"){


  train_function <- function(x, y) {

    x <- as.data.frame(x)
    y <- as.factor(y)


    result <- fit_kmeans_total(X = x, Y = y, n_clusters = n_clusters, max_iter = max_iter, method = method)

    ### Result ###
    result$classes = levels(y)
    #result$pred.params = c("class","raw", "prob")
    result$mode = "clustering"
    #class(result) <- "kmeans"

    return(result)

  }


  args <- list(
    n_clusters = n_clusters,
    max_iter = max_iter,
    method = method
  )

  new_model_sslr(train_function, "sslr_kmeans", args)

}

#' @title  General Interface Seeded KMeans
#' @description  The difference with traditional Kmeans is that in this method implemented,
#' at initialization, there are as many clusters as the number of classes that exist of the labelled data,
#' the average of the labelled data of a given class
#' @param max_iter maximum iterations in KMeans. Default is 10
#' @param method distance method in KMeans: "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
#' @references
#' Sugato Basu, Arindam Banerjee, Raymond Mooney\cr
#' \emph{Semi-supervised clustering by seeding}\cr
#' July 2002
#' In Proceedings of 19th International Conference on Machine Learning
#' @example demo/seeded_kmeans.R
#' @export
seeded_kmeans <- function(max_iter = 10,  method = "euclidean"){


  train_function <- function(x, y) {

    x <- as.data.frame(x)
    y <- as.factor(y)

    n_clusters <- length(levels(y))

    result <- fit_kmeans_total(X = x, Y = y, n_clusters = n_clusters, max_iter = max_iter, method = method,
                        init_clusters = init_clusters_seeded_kmeans)

    ### Result ###
    result$classes = levels(y)
    #result$pred.params = c("class","raw", "prob")
    result$mode = "clustering"
    #class(result) <- "kmeans"

    return(result)

  }

  args <- list(
    max_iter = max_iter,
    method = method
  )

  new_model_sslr(train_function, "seeded_kmeans", args)

}


#' @title  General Interface Constrained KMeans
#' @description  The initialization is the same as seeded kmeans,
#' the difference is that in the following steps the allocation of the clusters in
#' the labelled data does not change
#' @param max_iter maximum iterations in KMeans. Default is 10
#' @param method distance method in KMeans: "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
#' @references
#' Sugato Basu, Arindam Banerjee, Raymond Mooney\cr
#' \emph{Semi-supervised clustering by seeding}\cr
#' July 2002
#' In Proceedings of 19th International Conference on Machine Learning
#' @example demo/constrained_kmeans.R
#' @export
constrained_kmeans <- function(max_iter = 10 , method = "euclidean"){


  train_function <- function(x, y) {

    x <- as.data.frame(x)
    y <- as.factor(y)

    n_clusters <- length(levels(y))


    result <- fit_kmeans_total(X = x, Y = y, n_clusters = n_clusters, max_iter = max_iter,
                               method = method,init_clusters = init_clusters_seeded_kmeans,
                               assing_clusters = assing_clusters_constrained_kmeans)

    ### Result ###
    result$classes = levels(y)
    #result$pred.params = c("class","raw", "prob")
    result$mode = "clustering"
    #class(result) <- "kmeans"

    return(result)


  }


  args <- list(
    max_iter = max_iter,
    method = method
  )

  new_model_sslr(train_function, "constrained_kmeans", args)


}

