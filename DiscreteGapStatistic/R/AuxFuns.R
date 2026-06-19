#' Adapted k-modes algorithm
#'
#' K-modes function to accept any categorical distance based on
#' the function found in `klaR:kmodes`.
#'
#' @param data A matrix or data frame of categorical data. Objects have to be in rows, variables in columns.
#' @param modes The number of modes
#' @param distFun Pairwise categorical distance function. A function accepting two categorical vectors.
#' @param iter.max The maximum number of iterations allowed.
#' @return An object of class kmodes as found in `klaR` packages.
#' An additional component specifies the categorical distance function found in `distFun`.
#' @export

kmodesD <- function (data,
                    modes,
                    distFun,
                    iter.max = 10) {
   weighted = FALSE
   fast = TRUE

   if (!is.data.frame(data))
      data <- as.data.frame(data)
   isnumeric <- sapply(data, is.numeric)
   isfactor <- sapply(data, is.factor)
   if (any(isfactor)) {
      levs <- vector("list", ncol(data))
      for (j in which(isfactor)) {
         levsj <- levels(data[, j])
         data[, j] <- levsj[data[, j]]
         levs[[j]] <- levsj
      }
   }
   if (any(isnumeric)) {
      lengths <- sapply(data[, isnumeric], function(z) return(length(unique(z))))
      if (any(lengths > 30))
         warning("data has numeric coloumns with more than 30 different levels!")
   }
   update_mode <- function(num, num_var, data, cluster) {
      clust <- data[which(cluster == num), ]
      apply(clust, 2, function(cat) {
         cat <- table(cat)
         names(cat)[which.max(cat)]
      })
   }
   distance <- function(mode, obj, weights) {
      if (is.null(weights))
         return(distFun(mode, obj))
         ## return(sum(mode != obj))
      obj <- as.character(obj)
      mode <- as.character(mode)
      different <- which(mode != obj)
      n_mode <- n_obj <- numeric(length(different))
      for (i in seq(along = different)) {
         weight <- weights[[different[i]]]
         names <- names(weight)
         n_mode[i] <- weight[which(names == mode[different[i]])]
         n_obj[i] <- weight[which(names == obj[different[i]])]
      }
      dist <- sum((n_mode + n_obj)/(n_mode * n_obj))
      return(dist)
   }
   n <- nrow(data)
   num_var <- ncol(data)
   data <- as.data.frame(data)
   cluster <- numeric(n)
   names(cluster) <- 1:n
   if (missing(modes))
      stop("'modes' must be a number or a data frame.")
   if (iter.max < 1)
      stop("'iter.max' must be positive.")
   if (length(modes) == 1) {
      k <- modes
      modes <- unique(data)[sample(nrow(unique(data)))[1:k],
      ]
      for (i in 1:k) cluster[which(rownames(data) == rownames(modes)[i])] <- i
   }
   else {
      if (any(duplicated(modes)))
         stop("Initial modes are not distinct.")
      if (ncol(data) != ncol(modes))
         stop("'data' and 'modes' must have same number of columns")
      modes <- as.data.frame(modes)
      if (any(isfactor)) {
         if (!all(sapply(modes[, isfactor], is.factor)))
            stop("Types of modes do not match data!")
         for (j in which(isfactor)) modes[, j] <- levels(modes[,
                                                               j])[modes[, j]]
      }
      for (j in 1:num_var) if (weighted)
         if (!all(modes[, j] %in% unique(data[, j])))
            stop("For weighted call values of modes must exist in data!")
      k <- nrow(modes)
   }
   if (k > nrow(unique(data)))
      stop("More cluster modes than distinct data points.")
   if (weighted) {
      weights <- vector("list", num_var)
      for (i in 1:num_var) weights[[i]] <- table(data[, i])
   }
   else {
      weights <- NULL
   }
   if (!fast) {
      for (j in which(cluster == 0)) {
         dist <- apply(modes, 1, function(x) distance(x, data[j,
         ], weights))
         cluster[j] <- which.min(dist)
         modes[cluster[j], ] <- update_mode(cluster[j], num_var,
                                            data, cluster)
      }
      for (i in 1:iter.max) {
         continue <- FALSE
         for (j in 1:n) {
            dist <- apply(modes, 1, function(x) distance(x,
                                                         data[j, ], weights))
            clust_new <- which.min(dist)
            clust_old <- cluster[j]
            if (clust_new != clust_old) {
               cluster[j] <- clust_new
               modes[clust_new, ] <- update_mode(clust_new,
                                                 num_var, data, cluster)
               modes[clust_old, ] <- update_mode(clust_old,
                                                 num_var, data, cluster)
               continue <- TRUE
            }
         }
         if (!continue)
            break
      }
   }
   if (fast) {
      dists <- matrix(NA, nrow = n, ncol = k)
      if (!weighted) {
         for (i in 1:k) {
            di <- sapply(1:ncol(data), function(j) return(data[,
                                                               j] != rep(modes[i, j], n)))
            di <- rowSums(di)
            dists[, i] <- di
         }
      }
      if (weighted) {
         n_obj <- matrix(NA, nrow = n, ncol = ncol(data))
         for (j in 1:ncol(data)) n_obj[, j] <- weights[[j]][sapply(as.character(data[,
                                                                                     j]), function(z) return(which(names(weights[[j]]) ==
                                                                                                                      z)))]
         n_mode <- matrix(NA, nrow = nrow(modes), ncol = ncol(data))
         for (j in 1:ncol(data)) n_mode[, j] <- weights[[j]][sapply(as.character(modes[,
                                                                                       j]), function(z) return(which(names(weights[[j]]) ==
                                                                                                                        z)))]
         for (i in 1:k) {
            di <- sapply(1:ncol(data), function(j) return(data[,
                                                               j] != rep(modes[i, j], n)))
            wts <- (n_mode[rep(i, n), ] + n_obj)/(n_mode[rep(i,
                                                             n), ] * n_obj)
            di <- rowSums(di * wts)
            dists[, i] <- di
         }
      }
      cluster <- apply(dists, 1, function(z) {
         a <- which(z == min(z, na.rm = TRUE))
         if (length(a) > 1)
            a <- sample(a, 1)
         return(a)
      })
      for (j in 1:nrow(modes)) modes[j, ] <- update_mode(j,
                                                         num_var, data, cluster)
      for (i in 1:iter.max) {
         continue <- FALSE
         dists <- matrix(NA, nrow = n, ncol = k)
         if (!weighted) {
            for (i in 1:k) {
               di <- sapply(1:ncol(data), function(j) return(data[,
                                                                  j] != rep(modes[i, j], n)))
               di <- rowSums(di)
               dists[, i] <- di
            }
         }
         if (weighted) {
            n_mode <- matrix(NA, nrow = nrow(modes), ncol = ncol(data))
            for (j in 1:ncol(data)) n_mode[, j] <- weights[[j]][sapply(as.character(modes[,
                                                                                          j]), function(z) return(which(names(weights[[j]]) ==
                                                                                                                           z)))]
            for (i in 1:k) {
               di <- sapply(1:ncol(data), function(j) return(data[,
                                                                  j] != rep(modes[i, j], n)))
               wts <- (n_mode[rep(i, n), ] + n_obj)/(n_mode[rep(i,
                                                                n), ] * n_obj)
               di <- rowSums(di * wts)
               dists[, i] <- di
            }
         }
         old.cluster <- cluster
         cluster <- apply(dists, 1, function(z) {
            a <- which(z == min(z, na.rm = TRUE))
            if (length(a) > 1)
               a <- sample(a, 1)
            return(a)
         })
         for (j in 1:nrow(modes)) modes[j, ] <- update_mode(j,
                                                            num_var, data, cluster)
         if (any(old.cluster != cluster))
            continue <- TRUE
         if (!continue)
            break
      }
   }
   cluster.size <- table(cluster)
   if (length(cluster.size) < k)
      warning("One or more clusters are empty.")
   dists <- matrix(NA, nrow = n, ncol = k)
   if (weighted) {
      n_mode <- matrix(NA, nrow = nrow(modes), ncol = ncol(data))
      for (j in 1:ncol(data)) n_mode[, j] <- weights[[j]][sapply(as.character(modes[,
                                                                                    j]), function(z) return(which(names(weights[[j]]) ==
                                                                                                                     z)))]
   }
   for (i in 1:k) {
      di <- sapply(1:ncol(data), function(j) return(data[,
                                                         j] != rep(modes[i, j], n)))
      if (!weighted)
         di <- rowSums(di)
      if (weighted) {
         if (!fast) {
            n_obj <- matrix(NA, nrow = n, ncol = ncol(data))
            for (j in 1:ncol(data)) n_obj[, j] <- weights[[j]][sapply(as.character(data[,
                                                                                        j]), function(z) return(which(names(weights[[j]]) ==
                                                                                                                         z)))]
         }
         wts <- (n_mode[rep(i, n), ] + n_obj)/(n_mode[rep(i,
                                                          n), ] * n_obj)
         di <- rowSums(di * wts)
      }
      dists[, i] <- di
   }
   diffs <- numeric(k)
   for (i in seq_along(cluster.size)) diffs[i] <- sum(dists[cluster ==
                                                               i, i])
   rownames(modes) <- 1:k
   colnames(modes) <- colnames(data)
   if (any(isfactor))
      for (j in which(isfactor)) modes[, j] <- factor(modes[,
                                                            j], levels = levs[[j]])
   if (any(isnumeric))
      for (j in which(isnumeric)) modes[, j] <- as.numeric(modes[,
                                                                 j])

   result <- list(cluster = cluster,
                  size = cluster.size,
                  modes = modes,
                  withindiff = diffs,
                  iterations = i,
                  weighted = FALSE,
                  distFun = distFun)
   class(result) <- "kmodes"
   return(result)
}

#' Clustering generating function
#'
#' A function that generates formatted algorithmic functions that can be plugged
#' to enable run a wide variety of clustering algorithm for `clusGapDiscr` function.
#'
#' @param clustFun A character string with the following possible options:
#' 'pam' (default) from `cluster::pam`, 'diana' from `cluster::diana`, 'fanny' from `cluster::fanny`,
#' 'agnes-\{average, single, complete, ward, weighted\}' from `cluster::agnes`,
#' 'hclust-\{ward.D, ward.D2, single, complete, average, mcquitty, median, centroid\}' from `base::hclust`,
#' 'kmodes' from `klar::kmodes` (`iter.max = 10`, `weighted = FALSE` and `fast= TRUE`).
#' 'kmodes-N' enables to run the `kmodes` algorithm with a given number N of iterations where `iter.max = N`.
#' @return An object of class kmodes as found in `klaR` packages.
#' An additional component specifies the categorical distance function found in `distFun`.
#' @export

clusterFunSel <- function(clustFun ){
   if(clustFun  == 'pam'){
         return(cluster::pam)

   }else if(clustFun  == 'fanny'){
         return(cluster::fanny)

   }else if(clustFun  == 'diana'){
      dianaK <- function(x, k){
         out <- cluster::diana(x = x) %>%
            stats::cutree(k = k)
         list(cluster = out)
      }
      return(dianaK)

   }else if(grepl(pattern = '^agnes-.+', x = clustFun  )){
      cMeth <- sub('agnes-', '', clustFun  )
      agnesK <- function(x, k){
         out <- cluster::agnes(x = x,
                               diss = TRUE,
                               method = cMeth) %>%
            stats::cutree(k = k)
         list(cluster = out)
      }
      return(agnesK)

   }else if(grepl(pattern = '^hclust-.+', x = clustFun )){
      cMeth <- sub('hclust-', '', clustFun )
      hClustK <- function(x, k){
         out <- stats::hclust(d = x, method = cMeth) %>%
            stats::cutree(k = k)
         list(cluster = out)
      }
      return(hClustK)

   }else if(grepl(pattern = '^kmodes.*', x = clustFun )){

      cIter <- ifelse(grepl(pattern = 'kmodes-.+',  x = clustFun ),
                      as.numeric(sub('kmodes-', '', clustFun )),
                      10)
      kmodesD_ <- function(x, k, distFun){
         kmodesD(data = x, modes = k, distFun = distFun, iter.max = cIter)
      }
      return(kmodesD_)

   }
}
