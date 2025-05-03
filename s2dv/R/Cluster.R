#'K-means Clustering
#'
#'Compute cluster centers and their time series of occurrences, with the 
#'K-means clustering method using Euclidean distance, of an array of input data
#'with any number of dimensions that at least contain time_dim. 
#'Specifically, it partitions the array along time axis in K groups or clusters
#'in which each space vector/array belongs to (i.e., is a member of) the 
#'cluster with the nearest center or centroid. This function is a wrapper of 
#'kmeans() and relies on the NbClust package (Charrad et al., 2014 JSS) to 
#'determine the optimal number of clusters used for K-means clustering if it is
#'not provided by users. 
#'
#'@param data A numeric array with named dimensions that at least have 
#'  'time_dim' corresponding to time and 'space_dim' (optional) corresponding 
#'  to either area-averages over a series of domains or the grid points for any
#'  sptial grid structure.
#'@param weights A numeric array with named dimension of multiplicative weights
#'  based on the areas covering each domain/region or grid-cell of 'data'. The 
#'  dimensions must be equal to the 'space_dim' in 'data'. The default value is
#'  NULL which means no weighting is applied.
#'@param time_dim A character string indicating the name of time dimension in 
#'  'data'. The default value is 'sdate'.
#'@param space_dim A character vector indicating the names of spatial dimensions
#'  in 'data'. The default value is NULL.
#'@param nclusters A positive integer K that must be bigger than 1 indicating
#'  the number of clusters to be computed, or K initial cluster centers to be 
#'  used in the method. The default value is NULL, which means that the number
#'  of clusters will be determined by NbClust(). The parameter 'index' 
#'  therefore needs to be specified for NbClust() to find the optimal number of 
#'  clusters to be used for K-means clustering calculation.
#'@param index A character string of the validity index from NbClust package 
#'  that can be used to determine optimal K if K is not specified with 
#'  'nclusters'. The default value is 'sdindex' (Halkidi et al. 2001, JIIS). 
#'  Other indices available in NBClust are "kl", "ch", "hartigan", "ccc", 
#'  "scott", "marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db",
#'  "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ball", 
#'  "ptbiserial", "gap", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", 
#'  "hubert", "sdindex", and "sdbw".
#'  One can also use all of them with the option 'alllong' or almost all indices
#'   except gap, gamma, gplus and tau with 'all', when the optimal number of
#'  clusters K is detremined by the majority rule (the maximum of histogram of 
#'  the results of all indices with finite solutions). Use of some indices on 
#'  a big and/or unstructured dataset can be computationally intense and/or 
#'  could lead to numerical singularity. 
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return
#'A list containing:
#'\item{$cluster}{
#'  An integer array of the occurrence of a cluster along time, i.e., when
#'  certain data member in time is allocated to a specific cluster. The dimensions
#'  are same as 'data' without 'space_dim'.
#'}
#'\item{$centers}{
#'  A numeric array of cluster centres or centroids (e.g. [1:K, 1:spatial degrees 
#'  of freedom]). The rest dimensions are same as 'data' except 'time_dim' 
#'  and 'space_dim'.
#'}
#'\item{$totss}{
#'  A numeric array of the total sum of squares. The dimensions are same as 'data'
#'  except 'time_dim' and 'space_dim'.
#'}
#'\item{$withinss}{
#'  A numeric array of within-cluster sum of squares, one component per cluster. 
#'  The first dimenion is the number of cluster, and the rest dimensions are 
#'  same as 'data' except 'time_dim' and 'space_dim'.
#'}
#'\item{$tot.withinss}{
#'  A numeric array of the total within-cluster sum of squares, i.e., 
#'  sum(withinss). The dimensions are same as 'data' except 'time_dim' and 
#'  'space_dim'.
#'}
#'\item{$betweenss}{
#'  A numeric array of the between-cluster sum of squares, i.e. totss-tot.withinss.
#'  The dimensions are same as 'data' except 'time_dim' and 'space_dim'.
#'}
#'\item{$size}{
#'  A numeric array of the number of points in each cluster. The first dimenion 
#'  is the number of cluster, and the rest dimensions are same as 'data' except
#'  'time_dim' and 'space_dim'.
#'}
#'\item{$iter}{
#'  A numeric array of the number of (outer) iterations. The dimensions are 
#'  same as 'data' except 'time_dim' and 'space_dim'.
#'}
#'\item{$ifault}{
#'  A numeric array of an indicator of a possible algorithm problem. The 
#'  dimensions are same as 'data' except 'time_dim' and 'space_dim'.
#'}
#'
#'@references
#'Wilks, 2011, Statistical Methods in the Atmospheric Sciences, 3rd ed., Elsevire, pp 676.
#'
#'@examples
#'# Generating synthetic data
#'a1 <- array(dim = c(200, 4))
#'mean1 <- 0
#'sd1 <- 0.3 
#'
#'c0 <- seq(1, 200)
#'c1 <- sort(sample(x = 1:200, size = sample(x = 50:150, size = 1), replace = FALSE))
#'x1 <- c(1, 1, 1, 1)
#'for (i1 in c1) {
#'  a1[i1, ] <- x1 + rnorm(4, mean = mean1, sd = sd1)
#'}
#'
#'c1p5 <- c0[!(c0 %in% c1)]
#'c2 <- c1p5[seq(1, length(c1p5), 2)] 
#'x2 <- c(2, 2, 4, 4)
#'for (i2 in c2) {
#'  a1[i2, ] <- x2 + rnorm(4, mean = mean1, sd = sd1)
#'}
#'
#'c3 <- c1p5[seq(2, length(c1p5), 2)]
#'x3 <- c(3, 3, 1, 1)
#'for (i3 in c3) {
#'  a1[i3, ] <- x3 + rnorm(4, mean = mean1, sd = sd1)
#'}
#'
#'# Computing the clusters
#'names(dim(a1)) <- c('sdate', 'space')
#'res1 <- Cluster(data = a1, weights = array(1, dim = dim(a1)[2]), nclusters = 3)
#'res2 <- Cluster(data = a1, weights = array(1, dim = dim(a1)[2]))
#'
#'@import NbClust multiApply
#'@importFrom abind abind
#'@importFrom stats kmeans
#'@importFrom grDevices pdf dev.off 
#'@export
Cluster <- function(data, weights = NULL, time_dim = 'sdate', space_dim = NULL,
                    nclusters = NULL, index = 'sdindex', ncores = NULL) {
  # Check inputs 
  ## data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  if (is.null(dim(data))) {  #is vector
    dim(data) <- c(length(data))
    names(dim(data)) <- time_dim
  }
  if (any(is.null(names(dim(data)))) | any(nchar(names(dim(data))) == 0)) {
    stop("Parameter 'data' must have dimension names.")
  }

  ## weights
  if (!is.null(weights)) {
    if (!is.numeric(weights)) {
      stop("Parameter 'weights' must be a numeric array.")
    }
    if (is.null(dim(weights))) {  #is vector
      dim(weights) <- c(length(weights))
    }
    if (any(is.null(names(dim(weights)))) | any(nchar(names(dim(weights))) == 0)) {
      stop("Parameter 'weights' must have dimension names.")
    }
    if (any(!names(dim(weights)) %in% names(dim(data)) |
            !dim(weights) %in% dim(data))) {
      stop("Parameter 'weights' must have dimensions that can be found in 'data' dimensions.")
    }
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(data))) {
    stop("Parameter 'time_dim' is not found in 'data' dimensions.")
  }
  ## space_dim
  if (!is.null(space_dim)) {
    if (!is.character(space_dim)) {
      stop("Parameter 'space_dim' must be a character vector.")
    }
    if (!all(space_dim %in% names(dim(data)))) {
      stop("Parameter 'space_dim' is not found in 'data' dimensions.")
    }
    if (!is.null(weights)) {
      if (!(length(space_dim) == length(dim(weights)) & all(space_dim %in% names(dim(weights))))) {
        stop("Parameter 'weights' must have dimension names the same as 'space_dim'.")
      }
      if (any(space_dim != names(dim(weights)))) {
        space_dim <- names(dim(weights))
      }
    }
  }
  if (is.null(space_dim) & !is.null(weights)) {
    space_dim <- names(dim(weights))
    .warning(paste0("Parameter 'weights' is assigned but not 'space_dim'. Define 'space_dim' ",
                    "by the dimensions of 'weights'."))
  }
  ## nclusters
  if (!is.null(nclusters)) {
    if (!is.numeric(nclusters) | length(nclusters) != 1) {
      stop("Parameter 'nclusters' must be an integer bigger than 1.") 
    } else if (nclusters <= 1) {
      stop("Parameter 'nclusters' must be an integer bigger than 1.")      
    }
  }

  ## index
  if (!is.character(index) | length(index) > 1) {
    stop("Parameter 'index' should be a character strings accepted as 'index' ",
         "by the function NbClust::NbClust.")
  }

  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }

  ###############################
  # Compute nclusters
  if (is.null(nclusters)) {
    pdf(file = NULL)
     nbclust.results <- NbClust::NbClust(data, distance = 'euclidean', 
                                         min.nc = 2, max.nc = 20, 
                                         method = 'kmeans', index = index)
    dev.off()
    if (index == 'all' || index == 'alllong') {
      kmc  <- hist(nbclust.results$Best.nc[1, ], breaks = seq(0, 20), 
                   plot = FALSE)$counts
      nclusters <- which(kmc == max(kmc))
    } else {
      nclusters <- nbclust.results$Best.nc[1]
    }
  }

  # Calculate Cluster
  output <- Apply(list(data),
                  target_dims = c(time_dim, space_dim),
                  fun = .Cluster,
                  weights = weights, nclusters = nclusters, index = index,
                  ncores = ncores)

  return(output)
}

.Cluster <- function(data, nclusters, weights = NULL, index = 'sdindex') {
  # data: [time, (lat, lon)]
  dat_dim <- dim(data)

  if (length(dim(data)) != 1) {
    # Reshape data into two dims
    dim(data) <- c(dat_dim[1], prod(dat_dim[-1]))
  
    # weights
    if (!is.null(weights)) {
      dim(weights) <- prod(dim(weights))  # a vector 
      data_list <- lapply(1:dat_dim[1], 
                          function(x) {
                            data[x, ] * weights
                            })
      data <- do.call(abind::abind, c(data_list, along = 0))
    }
  }
  
  kmeans.results <- kmeans(data, centers = nclusters, iter.max = 300, 
                           nstart = 30) 

#---------------NEW---------------
  # Add dimension names and shape space_dim back
  kmeans.results$cluster <- as.array(kmeans.results$cluster)
  names(dim(kmeans.results$cluster)) <- names(dat_dim)[1]
  kmeans.results$centers <- array(kmeans.results$centers, 
                                  dim = c(nclusters, dat_dim[-1]))
  names(dim(kmeans.results$centers)) <- c('K', names(dat_dim)[-1])
  kmeans.results$withinss <- as.array(kmeans.results$withinss)
  names(dim(kmeans.results$withinss)) <- 'K'
  kmeans.results$size <- as.array(kmeans.results$size)
  names(dim(kmeans.results$size)) <- 'K'

#----------NEW_END----------------
  invisible(kmeans.results)
}
