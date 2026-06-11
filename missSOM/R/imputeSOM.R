# Johannes Kruisselbrink and Ron Wehrens are the authors of supersom function from the 'kohonen' package. 
# Our method, named as imputeSOM, uses the online mode of this function. 
#'
#' The Self-Organizing Maps with Built-in Missing Data Imputation. 
#'
#' @param data a \code{matrix} or \code{data.frame} with continuous variables containing the observations to be mapped on the grid by the kohonen algorithm, even if there are incomplete. 
#' @param grid a grid for the codebook vectors: see \code{somgrid}.
#' @param rlen the number of times the complete data set will be presented to the network.
#' @param alpha learning rate, a vector of two numbers indicating the amount of change. Default is to decline linearly from 0.05 to 0.01 over \code{rlen} updates. 
#' @param radius the radius of the neighbourhood, either given as a single number or a vector (start, stop). If it is given as a single 
#' number the radius will change linearly from \code{radius} to zero; as soon as the neighbourhood gets smaller than one only the winning unit 
#' will be updated. Note that the default before version 3.0 was to run from \code{radius} to \code{-radius}. If nothing is supplied, the 
#' default is to start with a value that covers 2/3 of all unit-to-unit distances.
#' @param maxNA.fraction the maximal fraction of values that may be NA to prevent the column to be removed.
#' @param keep.data if TRUE, return original data and mapping information. If FALSE, only return the trained map (in essence the 
#' codebook vectors).
#' @param dist.fcts  distance function to be used for the data. Admissable values 
#' currently are "sumofsquares", "euclidean" and "manhattan. Default is to use "sumofsquares".
#' @param init a \code{matrix} or \code{data.frame} corresponding to the initial values for the codebook vectors. 
#' It should have the same number of variables (columns) as the data. 
#' The number of rows corresponding to the number of units in the map.
#'
#' @return An object of class "missSOM" with components
#' \item{data}{Data matrix, only returned if \code{keep.data == TRUE}.}
#' \item{ximp}{Imputed data matrix.}
#' \item{unit.classif}{Winning units for data objects, only returned if \code{keep.data == TRUE}.} 
#' \item{distances}{Distances of objects to their corresponding winning unit, only returned if \code{keep.data == TRUE}.}
#' \item{grid}{The grid, an object of class \code{somgrid}.} 
#' \item{codes}{A list of matrices containing codebook vectors.} 
#' \item{alpha, radius}{Input arguments presented to the function.}
#' \item{maxNA.fraction}{The maximal fraction of values that may be NA to prevent the column to be removed.}
#' \item{dist.fcts}{The distance function used for the data.}
#' @export
#' @description \code{imputeSOM} is an extension of the online algorithm of the 'kohonen' package where missing data are imputed during the algorithm. 
#' All missing values are first imputed with initial values such as the mean of the observed variables. 
#' @seealso somgrid, \code{\link{plot.missSOM}}, \code{\link{map.missSOM}}
#' @importFrom Rcpp evalCpp sourceCpp
#' @importFrom stats quantile dist
#' @importFrom kpodclustr findMissing 
#' @examples 
#' data(wines)
#' 
#' ## Data with no missing values 
#' som.wines <- imputeSOM(scale(wines), grid = somgrid(5, 5, "hexagonal"))
#' summary(som.wines)
#' print(dim(som.wines$data))
#' 
#' ## Data with missing values 
#' X <- scale(wines)
#' missing_obs <- sample(1:nrow(wines), 10, replace = FALSE)
#' X[missing_obs, 1:2] <- NaN
#' som.wines <- imputeSOM(X, grid = somgrid(5, 5, "hexagonal"))
#' summary(som.wines)
#' print(dim(som.wines$ximp))
#' print(sum(is.na(som.wines$ximp)))
#' 
imputeSOM <- function(data,
                     grid = somgrid(),
                     rlen = 100,
                     alpha = c(0.05, 0.01),
                     radius = quantile(nhbrdist, 2/3),
                     maxNA.fraction = 1,
                     keep.data = TRUE,
                     dist.fcts = NULL,
                     init)      
{
  ## ##########################################################################
  ## Check data
  data <- check.data(data)
  
  nacolumns <- check.empty.columns(data, maxNA.fraction = maxNA.fraction)
  
  ## data_matrix is the complete list, but with columns removed that
  ## contain too many NAs
  data_matrix <- remove.data.na(data, nacolumns)
  
  ## impute is FALSE if data does not contain missing data
  impute <- FALSE
  
  nobjects <- nrow(data_matrix)
  nvar <- ncol(data_matrix)
  
  missingCol <- c()
  missingRow <- c()
  
  ## Check if data contain missing values and if TRUE missing data are imputed by the mean of the observed data
  if (any(c(is.na(data_matrix)))){
    ## Index of missing data in data_matrix
    missingInd <- findMissing(data_matrix)
    ## Index of columns of missing data 
    missingCol <- ((missingInd-1) %/% nobjects)+1
    ## Index of rows of missing data 
    missingRow <- missingInd %% nobjects
    missingRow[missingRow==0] <- nobjects
    Xmeans <- colMeans(data_matrix, na.rm=TRUE)     
    data_matrix[missingInd] <- Xmeans[missingCol]
    ## Missing data will be imputed during algorithm
    impute <- TRUE
  }
  
  
  ## ##########################################################################
  ## Check radius update parameters
  grid <- check.somgrid(grid)
  nhbrdist <- unit.distances(grid)
  if (length(radius) == 1)
    if (is.na(radius))
      radius <- quantile(nhbrdist, 2/3)
    radius <- c(radius, 0)
  
  ## ##########################################################################
  ## Distances.
  ## Situations:
  ## - no distance defined. Then we take the default squared euclidean distance 
  if (length(dist.fcts) == 1) {
    orig.dist.fcts <- dist.fcts
  } else {
      defaultDist <- "sumofsquares"
      default.dist.fcts <- defaultDist
      if (length(dist.fcts) == 0) {
        orig.dist.fcts <- default.dist.fcts
        dist.fcts <- orig.dist.fcts
      } else {
          stop("Wrong number of distances defined")
      }
  }

  dist.ptrs <- getDistancePointers(c(dist.fcts))
  
  ## ##########################################################################
  ## Get or create initial codebooks
  ncodes <- nrow(grid$pts)
  if (missing(init)) {
    starters <- sample(1:nobjects, ncodes, replace = FALSE)
    init <- data_matrix[starters,,drop=FALSE]
  } else {
    ## Check length and dimensions
    if (is.data.frame(init) | is.matrix(init))
      init <- as.matrix(init)
    ## Check whether init is numeric
    if (!all(sapply(init, is.numeric)))
      stop("Argument init should be numeric")
    if (!(nrow(init) == ncodes))
      stop("Incorrect number of objects in initalization matrices")
    if (!(ncol(init) == nvar)) {     
      stop("Incorrect number of variables in initialization matrices, ",
             "maybe due to the removal of columns because of NAs")
      }
  }

  init.matrix <- t(init)
  
  data.matrix <- t(data_matrix)
  data <- t(data)
  ## ##########################################################################
  ## Go!
  res <- RcppImputeSOM(data = data.matrix,
                       missData = data,
                       codes = init.matrix,
                       distanceFunctions = dist.ptrs,
                       neighbourhoodDistances = nhbrdist,
                       neighbourhoodFct =
                         as.integer(grid$neighbourhood.fct),
                       alphas = alpha,
                       radii = radius,
                       numEpochs = rlen, 
                       bool_impute = impute,                                   
                       missingCol = as.integer(missingRow),
                       missingRow = as.integer(missingCol))                                   
  changes <- matrix(res$changes, ncol = 1, byrow = TRUE)
  mycodes <- res$codes
  ximp <- res$ximp
  
  ## ##########################################################################
  ## Format the codes and data
  mycodes <- t(mycodes)
  if (impute) {
    ximp <- t(ximp)
  } else {
    ximp <- NULL
  }
  colnames(mycodes) <- colnames(data_matrix)
  colnames(ximp) <- colnames(ximp)
  
  ## ##########################################################################
  ## Prepare results
  if (keep.data) {
    data.full <- t(data)
    if (impute){
      data.full <- ximp
    }
    mapping <-
      map.missSOM(structure(list(codes = mycodes,
                                 dist.fcts = c(orig.dist.fcts),
                                 data = data.full),
                            class = "missSOM"),
                  maxNA.fraction = maxNA.fraction)
    structure(list(data = data.full,
                   ximp = ximp,
                   unit.classif = mapping$unit.classif,
                   distances = mapping$distances,
                   grid = grid,
                   codes = mycodes,
                   changes = changes,
                   alpha = alpha,
                   radius = radius,
                   maxNA.fraction = maxNA.fraction,
                   dist.fcts = orig.dist.fcts),
              class = "missSOM")
  } else {
    structure(list(grid = grid,
                   ximp = ximp,
                   codes = mycodes,
                   changes = changes,
                   alpha = alpha,
                   radius = radius,
                   maxNA.fraction = maxNA.fraction,
                   dist.fcts = orig.dist.fcts),
              class = "missSOM")
  }
}
