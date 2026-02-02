## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

## -----------------------------------------------------------------------------
## Load packages
library(sarp.snowprofile.alignment)

## Sample profiles
plot(SPgroup2, SortMethod = 'hs')

## -----------------------------------------------------------------------------
## Distance matrix using default settings
distmat1 <- distanceSP(SPgroup2)
print(distmat1)

## -----------------------------------------------------------------------------
## Check for same result when computing distant in parallel on multiple cores
# library(parallel)
# n_cores <- detectCores() - 1
# distmat1b <- distanceSP(SPgroup2, n_cores = n_cores)
# identical(distmat1, distmat1b)

## Fast version of pairwise distances based on summary stats
distmat2 <- distanceSP(SPgroup2, fast_summary = TRUE)
print(distmat1)
print(distmat2)

## -----------------------------------------------------------------------------
config <- clusterSPconfig(simType = 'layerwise', ddate = T)
str(config)
distmat3 <- do.call('distanceSP', c(list(SPgroup2), config$args_distance))

## -----------------------------------------------------------------------------
cl_hclust <- clusterSP(SPx = SPgroup2, k = 2, type = 'hclust', distmat = distmat1)
plot(cl_hclust)

## -----------------------------------------------------------------------------
cl_pam <- clusterSP(SPx = SPgroup2, k = 2, type = 'pam', distmat = distmat1)
plot(cl_pam)

## -----------------------------------------------------------------------------
cl_kdba <- clusterSP(SPx = SPgroup2, k = 2, type = 'kdba')
plot(cl_kdba, centers = 'n')

## ----fig.height = 4-----------------------------------------------------------
plot(cl_pam, centers = 'medoids')
plot(cl_kdba, centers = 'centroids')

## -----------------------------------------------------------------------------
## A binary vector identifying which profiles have SH on the surface
sh <- sapply(SPgroup2, function(x) x$layers$gtype[nrow(x$layers)] == 'SH')

## Construct a distance matrix
distmat_sh <- dist(sh)

## Create weighted average
distmat <- 0.25 *  distmat1 + 0.75 * distmat_sh
cl_sh <- clusterSP(SPx = SPgroup2, k = 2, type = 'hclust', distmat = distmat)
plot(cl_sh)

