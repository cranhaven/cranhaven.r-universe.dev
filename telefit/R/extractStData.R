#' Basic extraction of SpatialGridDataFrame data for teleconnection analysis
#'
#' @export
#' 
#' @import foreach
#' 
#' @importFrom sp coordinates
#' @importFrom raster extent
#' @importFrom stats complete.cases model.matrix
#' 
#' @param X SpatialGridDataFrame with local covariates.  If X is a list, each
#'  SpatialGridDataFrame will be included as one covariate.
#' @param Y SpatialGridDataFrame with response data
#' @param Z SpatialGridDataFrame with remote covariates. If Z is a list, this 
#'  function assumes each element of the list contains observations for the same
#'  covariate, but from different spatial regions.  If Z is a list, D.r and 
#'  mask.r must
#'  also be lists so that this function can know which regions to extract from
#'  each SpatialGridDataFrame
#' @param t Timepoint from which to extract data from X, Y, and Z.  If NULL,
#'  then all timepoints will be used.
#' @param D.s c(xmin, xmax, ymin, ymax) region from which to extract data from 
#'  X and Y, or a SpatialPolygonsXXX object containing boundaries of regions to
#'  extract areal data from.
#' @param D.r c(xmin, xmax, ymin, ymax) region from which to extract data from Z
#' @param intercept If TRUE, an intercept will be added to the design matrix
#' @param mask.s SpatialGridDataFrame to be used as a mask when extracting data
#'  from X and Y.  Locations in mask.s with NA values will be ignored when 
#'  extracting data from X and Y.
#' @param mask.r SpatialGridDataFrame to be used as a mask when extracting data
#'  from Z.  Locations in mask.s with NA values will be ignored when 
#'  extracting data from Z.
#' @param type.s 'response' 'anomaly' or 'std.anomaly' or a vector of these
#'  options depending on whether
#'  data extracted from X should be the observed data, anomalies, or
#'  standardized anomalies (where the climatology is computed from the 
#'  observations as the pointwise temporal average)
#' @param type.s.y 'response' 'anomaly' or 'std.anomaly' depending on whether
#'  data extracted from Y should be the observed data, anomalies, or
#'  standardized anomalies (where the climatology is computed from the 
#'  observations as the pointwise temporal average)
#' @param type.r 'response' 'anomaly' or 'std.anomaly' or a vector of these
#'  options depending on whether
#'  data extracted from Z should be the observed data, anomalies, or
#'  standardized anomalies (where the climatology is computed from the 
#'  observations as the pointwise temporal average)
#' @param aggfact.s If provided, will spatially average Y and X data
#' @param aggfact.r If provided, will spatially average Z data
#' @param X.lab name for X data (optional)
#' @param Y.lab name for Y data (optional)
#' @param Z.lab name for Z data (optional)
#' @param aspect TRUE or vector of logicals (one for each X object)
#'   to return the aspect of the surface at each location 
#'   instead of the value of the surface itself
#' @param aspect.categories if aspect==TRUE, this specifies the number of 
#'   discrete categories to divide aspect numbers (0-360) into.  NULL if the
#'   original scale (0-360) should be kept. By design, the aspect categories
#'   will be centered on north in the first category.
#' @param slope TRUE or vector of logicals (one for each X object)
#'   to return the slope of the surface at each location 
#'   instead of the value of the surface itself
#' @param colnames.X names of columns of X
#' @param formula formula object to specify how to create the design matrix
#'  
#' @example examples/extract.R

extractStData = function( X, Y, Z, t=NULL, D.s, D.r, mask.s = NULL, mask.r = NULL,
                          aggfact.s = NULL, aggfact.r = NULL, intercept = T,
                          type.s = 'response', type.r = 'response',
                          type.s.y = 'response',
                          X.lab = NULL, Y.lab = NULL, Z.lab = NULL,
                          aspect = F, aspect.categories=4, slope=F,
                          colnames.X=NULL, formula=NULL) {
              
  if(!inherits(X, 'list'))
    X = list(X)
  
  if(is.null(X.lab))
    X.lab = 'X'
  
  if(is.null(Y.lab))
    Y.lab = 'Y'
  
  if(is.null(Z.lab))
    Z.lab = 'Z'
  
  if(!inherits(Z,'list')) {
    Z = list(Z)
    D.r = list(D.r)
    mask.r = list(mask.r)
  }
  
  if(length(type.s)!=length(X)) {
    type.s = rep(type.s, length(X))
  }
  
  if(length(type.r)!=length(Z)) {
    type.r = rep(type.r, length(Z))
  }
  
  if(length(aspect)!=length(X)) {
    aspect = rep(aspect, length(X))
  }
  
  if(length(slope)!=length(X)) {
    slope = rep(slope, length(X))
  }
  
  # convert local bounds to extent object if not extracting areal data
  if(is.numeric(D.s)) {
    D.s = extent(D.s)
  }
  
  # convert remote bounds to extent object if not extracting areal data
  for(i in 1:length(D.r)) {
    if(is.numeric(D.r[[i]])) {
      D.r[[i]] = extent(D.r[[i]])
    }
  }
  
  # save time labels before they are converted to column indices
  if(is.null(t)) {
    t = names(Y)
  }
  tLabs = t
  
  # filter out undesired timepoints
  Y = Y[,,match(t, names(Y@data)), drop =FALSE]
  for(i in 1:length(X)) {
    X[[i]] = X[[i]][,,match(t, names(X[[i]]@data)), drop =FALSE]
  }
  for(i in 1:length(Z)) {
    Z[[i]] = Z[[i]][,,match(t, names(Z[[i]]@data)), drop =FALSE]
  }
  for(i in 1:length(mask.r)) {
    if(!is.null(mask.r[[i]])) {
      mask.r[[i]] = mask.r[[i]][,,match(t, names(mask.r[[i]]@data)), drop =FALSE]
    }
  }
  if(!is.null(mask.s)) {
    mask.s = mask.s[,,match(t, names(mask.s@data)), drop =FALSE]
  }
  
  # convert time labels to column indices
  t = match(t, names(Y))
  
  
  # extract local data
  
  Y = extractRegion(Y, D.s, type.s.y, aggfact.s, mask.s)
  
  # extract regions and aggregate local covariates
  for(i in 1:length(X))
    X[[i]] = extractRegion(X[[i]], D.s, type.s[i], aggfact.s, mask.s, aspect[i],
                           aspect.categories, slope[i])
  
  # build local design matrices for each timepoint
  o = options('na.action')
  options(na.action = 'na.pass')
  X.mat = foreach(tt = t, .combine = 'abind3') %do% {
    
    # extract data from each predictor
    x = foreach(x = X, .combine='cbind') %do% { 
      if(inherits(x, 'RasterBrick')) { 
        x@data@values[, tt, drop =FALSE] 
      } else if(startsWith(class(x), 'SpatialPolygons')) {
        x@data[, tt, drop =FALSE] 
      }
    }
    
    # if a formula is not specified, add intercept if requested; return data
    if(!is.null(formula)) {
      x = data.frame(x)
      colnames(x) = colnames.X
      x = model.matrix(formula, x)
    } else if(intercept) {
      x = cbind(1, x)
    }
    
    x
  }
  options(na.action = o)
  
  # correct for single-year extractions
  if(length(t)==1) { 
    X.mat = array(data = X.mat, dim = c(nrow(X.mat), 1, ncol(X.mat)))
  }
  
  
  # extract remote data
  
  
  # extract regions and aggregate remote covariates
  for(i in 1:length(Z))
    Z[[i]] = extractRegion(Z[[i]], D.r[[i]], type.r[i], aggfact.r, mask.r[[i]])
  
  # combine remote covariate data from each region
  Z.mat = foreach(z = Z, .combine = 'rbind') %do% { 
    matrix(z@data@values[, t, drop =FALSE], ncol = length(t)) 
  }

  # extract response data and coordinates
  if(inherits(Y,'RasterBrick')) { 
    Y.mat = Y@data@values[,t, drop =FALSE]
    coords.s = coordinates(Y)
  } else if(startsWith(class(Y), 'SpatialPolygons')) {
    Y.mat = as.matrix(Y@data[,t, drop =FALSE])
  }
  
  # extract remote coordinates
  coords.r = foreach(z = Z, .combine = 'rbind') %do% { coordinates(z) }
  
  # remove remote covariates that have NA data
  complete.data = complete.cases(Z.mat)
  Z.mat = matrix(Z.mat[complete.data,], ncol=length(t))
  coords.r = coords.r[complete.data,]
  
  # remove local coordinates that have NA responses
  complete.data = complete.cases(Y.mat)
  Y.mat = matrix(Y.mat[complete.data,], ncol=length(t))
  X.mat = X.mat[complete.data,,, drop = FALSE]
  if(inherits(Y,'RasterBrick')) {
    coords.s = coords.s[complete.data,]
  }
  
  # remove local coordinates that have NA covariates
  complete.data = complete.cases(matrix(X.mat[,,1], nrow = nrow(Y.mat)))
  for(i in 2:dim(X.mat)[3]) { 
    complete.data = complete.data & 
      complete.cases(matrix(X.mat[,,i], nrow = nrow(Y.mat)))
  }
  Y.mat = matrix(Y.mat[complete.data,], ncol=length(t))
  X.mat = X.mat[complete.data,,, drop = FALSE]
  if(inherits(Y,'RasterBrick')) {
    coords.s = coords.s[complete.data,]
  }
  
  # build return object
    
  res = list(
    tLabs = tLabs,
    coords.r = coords.r,
    X = X.mat,
    Y = Y.mat,
    Z = Z.mat
  )
  
  if(inherits(Y,'RasterBrick')) {
    res$coords.s = coords.s
  }
  
  res$X.lab = X.lab
  res$Y.lab = Y.lab
  res$Z.lab = Z.lab
  
  class(res) = 'stData'
  
  res
}
