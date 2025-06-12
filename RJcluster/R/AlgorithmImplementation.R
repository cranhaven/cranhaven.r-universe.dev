cleanFindCC = function(temp)
{
  for (i in 1:length(temp))
  {
    temp[[i]] = temp[[i]][, 1]
  }

  return(temp)
}


### STEP 1
# initial clustering
initialClustering = function(X, n_bins, seed = 1)
{
  # get dimensions
  p = ncol(X)
  N = nrow(X)

  # sample data
  set.seed(seed)
  fold_ids = sample(rep(seq_len(n_bins), length.out = N))

  # if not doing parallel: d is the number of clusters, CC is an empty list
  CC = list() # if not doing parallel

  # get clusters in each cut
  for (i in 1:n_bins) 
  {
    # get relevent indices
    temp_index = which(fold_ids == i) # get the index
    Z1 = X[temp_index, ]

    # get scaled cross product
    GG1 = tcrossprod(Z1) / p

    # remove the diagonal and add as a new column at the end
    gg_wodiag = GG1 - diag(diag(GG1))
    cut = length(temp_index) - 1
    GG_new = cbind(gg_wodiag + diag(colSums(gg_wodiag) / (cut - 1)), diag(GG1))

    # calling actual clustering
    MclustGG1 = Mclust(GG_new, modelNames = "VVI", verbose = F)

    # if not doing parallel
    CC_i = getCCmatrix_c(MclustGG1$classification, temp_index, MclustGG1$G)
    CC_i = cleanFindCC(CC_i)
    CC = c(CC, CC_i)
  }

  return(CC = CC)
}
# 
# # initial clustering
# initialClusteringparallel = function(X, n_bins, seed = 1)
# {
#   # get dimensions
#   p = ncol(X)
#   N = nrow(X)
#   
#   # sample data
#   set.seed(seed)
#   fold_ids = sample(rep(seq_len(n_bins), length.out = N))
#   
#   # register parallel session with total number of cores - 2
#   # doParallel::registerDoParallel(cores = parallel::detectCores() - 2)
#   
#   chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
#   
#   if (nzchar(chk) && chk == "TRUE") {
#     # use 2 cores in CRAN/Travis/AppVeyor
#     num_workers <- 2L
#   } else {
#     # use all cores in devtools::test()
#     num_workers <- parallel::detectCores()
#   }
#   
#   cl = parallel::makeCluster(num_workers)
#   doParallel::registerDoParallel(cl)
#   
#   CC_loop <- foreach(i = 1:n_bins, .combine='c', .multicombine=TRUE, .packages = c("mclust")) %dopar% 
#   {
#     # get relevent indices
#     temp_index = which(fold_ids == i) # get the index
#     Z1 = X[temp_index, ]
#     
#     # get scaled cross product
#     GG1 = tcrossprod(Z1) / p
#     
#     # remove the diagonal and add as a new column at the end
#     gg_wodiag = GG1 - diag(diag(GG1))
#     cut = length(temp_index) - 1
#     GG_new = cbind(gg_wodiag + diag(colSums(gg_wodiag) / (cut-1)), diag(GG1))
#     
#     # calling actual clustering
#     MclustGG1 = Mclust(GG_new, modelNames = "VVI", verbose = F)
#     return_information = list(MclustGG1$classification, temp_index, MclustGG1$G)
#   }
#   
#   parallel::stopCluster(cl)
#   
#   CC = list()
#   for (i in seq(1, length(CC_loop), 3))
#   {
#     CC_i = getCCmatrix_c(CC_loop[[ i ]], CC_loop[[ i + 1 ]], CC_loop[[ i + 2 ]])
#     CC_i = cleanFindCC(CC_i)
# 
#     CC = c(CC, CC_i)
#   }
#   
#   return(CC = CC)
# }

### step 2
# get the matrix of means and return them
getMeansMatrix = function(X, CC)
{
  d = length(CC)
  p = ncol(X)

  Cmeans = matrix(0, nrow = d, ncol = p)
  # get matrix of means
  Cmeans = getMatrixMeans_c(CC, X, d)

  return(Cmeans)
}


### step 3
secondClustering = function(Cmeans)
{
  # get dimensions
  n = nrow(Cmeans)
  p = ncol(Cmeans)

  # reset for notational match with original code
  ZZ = Cmeans

  # getting ZZ'
  GG = tcrossprod(ZZ) / p

  # set up new matrix for RJ algorithm
  gg = GG
  gg_wodiag = gg - diag(diag(gg))
  # gg_wdiag = cbind(gg_wodiag, diag(gg))
  GG_new = cbind(gg_wodiag + diag(colSums(gg_wodiag) / (n - 1)), diag(gg))

  # do second clustering
  Clust.J = Mclust(GG_new, modelNames = "VVI", verbose = F)

  return(Clust.J)
}

### step 4 - assign each data point a group
assignGroups = function(N, G, classification, CC)
{
  Group = assignGroups_c(N, G, classification, CC)
  return(Group[,1])
}

### step 5 - final clustering
finalClustering = function(G, Group, X)
{
  # N = nrow(X)
  p = ncol(X)

  Lmark = getFinalMeans_c(G, Group, X)

  LL = tcrossprod(X, Lmark) / p

  newRJ = Mclust(LL, modelNames = "VVI", G, verbose = F)

  return(newRJ)
}


## Actual function called by user if RJ scale
RJclust_backend = function(X, n_bins, seed = seed)
{
  # 1- initia lclustering of cuts of matrix
  # CC = initialClusteringparallel(X, n_bins, seed = seed)
  CC = initialClustering(X, n_bins, seed = seed)
  
  # 2- get first dxp matrix of means
  Cmeans = getMeansMatrix(X, CC)

  # 3- get second clustering based on means
  clustering = secondClustering(Cmeans)

  # 4 - assign each data point a group
  G = as.numeric(clustering[6])
  # classification = clustering[14] # from original code, but should be 15 - classification index
  classification = clustering[15] # this is the classification
  classification = as.vector(unlist(classification[1]))
  
  Group = assignGroups(nrow(X), G, classification, CC)

  # 5 - final clustering
  RJ  = finalClustering(G, Group, X)

  return(RJ)
}


# RJ function with no scaling
RJclust_noscale_mnist = function(Z, G_mclust = 10, seed = 1)
{
  set.seed(seed)
  p_temp = ncol(Z)
  n_temp = nrow(Z)
  
  # RJ steps
  gg = tcrossprod(Z)/p_temp
  gg_wodiag = gg - diag(diag(gg))
  # gg_wdiag = cbind(gg_wodiag, diag(gg))
  GG_new = cbind(gg_wodiag + diag(colSums(gg_wodiag) / (n_temp - 1)), diag(gg))
  
  # par(mfrow = c(1,1))
  # image(rotate(GG_new), col = gray.colors(33))
  
  step_one_clust = Mclust(GG_new, G = G_mclust, verbose = FALSE)
  
  return(step_one_clust)
}

# For large \eqn{n}, the scale method can be used, which uses the approximation method of RJclust. For the scale method,a parmater n_bins (usually \eqn{\sqrt(p)}) is required that splits the data into different buckets. 

#' RJclust
#'
#' This is a high dimensional clustering algorithm for data in matrix form. There are are two different types of penalty methods that can be used, 
#' depending on the size of the data and the desired accuracy. The first is the default method: the hokey stick penalty. There is also the BIC penalty.
#' For large \eqn{n}, the scale method can be used,  which uses the approximation method of RJclust. For the scaleRJ method,
#' a parmater n_bins (usually \eqn{\sqrt(p)}) is required that splits the data into different buckets. 
#' For all  methods, a C_max variable is needed that is an upper limit on the possible 
#' number of clusters. 
#' 
#' All implementations use backend C++ to increase runtime. 
#' 
#' model_names controls the type of covariance structure. See \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5096736/}{Mclust Documenttion} 
#' for more information. Note criterion "kmeans" is the same as "EEI". It is not suggested to use "kmeans" if it is suspected the classes
#' are imbalanced
#' 
#' @param data Data input, must be in matrix form. Currently no support for missing values 
#' @param penalty A string of possible vectors. Options include: "bic" an "hockey_stock" (default = "hockey_stick")
#' @param scaleRJ Should the scaled version of RJ be used, suggested for data where n > 1000 (default = FALSE)
#' @param C_max Maximum number of clusters to look for (default is 10)
#' @param criterion Model of covariance structure (default = "VVI")
#' @param n_bins Number of cuts if penalty = "scale" for the scaled RJ algorithm (default = sqrt(p))
#' @param seed Seed (defalt = 1)
#' @param verbose Should progress be printed? (default = FALSE)
#'
#' @return Returns RJ algorithm result for "aic", "bic" ("mclust" and "scale" will return an mclust object: \tabular{ll}{
#'    \code{K} \tab number of clusters found \cr
#'    \tab \cr
#'    \code{class} \tab Class labels \cr
#'    \tab \cr
#'    \code{penalty} \tab Penalty values at each iteraiton \cr
#'    \tab \cr
#'    \code{mean} \tab Mean matrix \cr
#'    \tab \cr
#'    \code{prob} \tab Probability values \cr
#'    \tab \cr
#'    \code{z} \tab Z values from mclust (NULL penalty = "full_covariance") \cr
#' }
#' @export
#'
#' @examples
#' X = simulate_HD_data()
#' X = X$X
#' clust = RJclust(X, penalty = "hockey_stick", C_max = 10)
RJclust = function(data, penalty = "hockey_stick", scaleRJ = FALSE, C_max = 10, criterion = "VVI", n_bins = NULL, seed = 1, verbose = FALSE)
{
  
  print("NOTE: RJclust assumes that data is centered and scaled. Use the scale() function if your data is not already normalized")
  
  possible_model_names = c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EVE", "VEE", "VVE", "EEV", "VEV", "EVV", "VVV", "kmeans")
  model_names = criterion
  
  if (!(model_names %in% possible_model_names))
  {
    stop("Criterion must be one of the following: \"EII\", \"VII\", \"EEI\", \"VEI\", \"EVI\", \"VVI\", \"EEE\", 
          \"EVE\", \"VEE\", \"VVE\", \"EEV\", \"VEV\", \"EVV\", \"VVV\", \"kmeans\")")
  }

  if (model_names == "kmeans")
  {
    model_names = "EII"
  }
  
  X = data
  # first check method: Options are bic (default), aic, full covariance, mclust, and scale
  penalty = tolower(penalty)
  possibilities = c("bic", "hockey_stick")
  if (!(penalty %in% possibilities))
  {
    warning("No valid value for method given. Defaulting to hockey_stick Possiblities are: (\"bic\", \"hockey_stick\")")
  }
  
  # check that data is a matrix
  if (!is.matrix(X))
  {
    stop("Data must be in a matrix form")
  }

  # check that n_bins is not too big, assuming it was not null
  if (scaleRJ)
  {
    
    if (is.null(n_bins))
    {
      n_bins = floor(sqrt(ncol(X)))
      warning("No n_bins value provided, using sqrt(p)")
    } else {
      if (n_bins >= nrow(X))
      {
        stop("n_bins must be < n")
      }
      
      if (n_bins >= nrow(X) / 4)
      {
        warning("RJclust will preform better with a n_bins that divides the data into larger chunks")
      }
    }
  }

  if (!scaleRJ & nrow(X) > 1000)
  {
    warning("RJclust will preform better with the scaled version, try passing in a n_bins value and setting scale = TRUE")
  }
  
  # if (scale)
  # {
  #   # if the data has all positive data, take the log and center/scale
  #   if (min(X) > 0)
  #   {
  #     temp = log(X)
  #     X = scale(temp)
  #   } else {
  #     X = scale(X)
  #   }
  # }
  
  # if there is a n_bins indicated, run RJ_scale, otherwise run with no scale
  
  if (penalty == "bic") 
  {
    if (!scaleRJ)
    {
      to_return = RJ_bic(X, C_max = C_max,modelNames = model_names, verbose = verbose)
    } else {
      to_return = RJ_bic_scale(X, num_cut = n_bins, C_max = C_max)
    }
  } else if (penalty == "hockey_stick") {
    if (!scaleRJ)
    {
      to_return = RJ_hockey_stick(X, C_max = C_max,modelNames = model_names, verbose = verbose)
    } else {
      to_return = RJ_hockey_stick_scale(X, num_cut = n_bins, C_max = C_max)
    }
  } else
  {
    to_return = paste("No valid value for penalty provided. Please check the documentation for possibilities")
  }
  
  # run RJ algorithm
  return(to_return)
}


# default RJclust - bic 
# backup - RJclust_BIC full model (this is what I have been working on)
# another way: use mclust


