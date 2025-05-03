#'Area-weighted empirical orthogonal function analysis using SVD
#'
#'Perform an area-weighted EOF analysis using single value decomposition (SVD) 
#'based on a covariance matrix or a correlation matrix if parameter 'corr' is 
#'set to TRUE.
#'
#'@param ano A numerical array of anomalies with named dimensions to calculate
#'  EOF. The dimensions must have at least 'time_dim' and 'space_dim'. NAs 
#'  could exist but it should be consistent along time_dim. That is, if one grid
#'  point has NAs, all the time steps at this point should be NAs. 
#'@param lat A vector of the latitudes of 'ano'.
#'@param lon A vector of the longitudes of 'ano'.
#'@param time_dim A character string indicating the name of the time dimension
#' of 'ano'. The default value is 'sdate'. 
#'@param space_dim A vector of two character strings. The first is the dimension
#'  name of latitude of 'ano' and the second is the dimension name of longitude
#'  of 'ano'. The default value is c('lat', 'lon').
#'@param neofs A positive integer of the modes to be kept. The default value is
#'  15. If time length or the product of the length of space_dim is smaller than
#'  neofs, neofs will be changed to the minimum of the three values.
#'@param corr A logical value indicating whether to base on a correlation (TRUE)
#'  or on a covariance matrix (FALSE). The default value is FALSE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return 
#'A list containing:
#'\item{EOFs}{
#'  An array of EOF patterns normalized to 1 (unitless) with dimensions 
#'  (number of modes, rest of the dimensions of 'ano' except 'time_dim'). 
#'  Multiplying \code{EOFs} by \code{PCs} gives the original reconstructed 
#'  field.
#'}
#'\item{PCs}{
#'  An array of principal components with the units of the original field to 
#'  the power of 2, with dimensions (time_dim, number of modes, rest of the
#'  dimensions of 'ano' except 'space_dim'). 
#'  'PCs' contains already the percentage of explained variance so, 
#'  to reconstruct the original field it's only needed to multiply 'EOFs'
#'  by 'PCs'.
#'} 
#'\item{var}{
#'  An array of the percentage (%) of variance fraction of total variance 
#'  explained by each mode (number of modes). The dimensions are (number of 
#'  modes, rest of the dimensions of 'ano' except 'time_dim' and 'space_dim').
#'}
#'\item{mask}{
#'  An array of the mask with dimensions (space_dim, rest of the dimensions of 
#'  'ano' except 'time_dim'). It is made from 'ano', 1 for the positions that 
#'  'ano' has value and NA for the positions that 'ano' has NA. It is used to 
#'  replace NAs with 0s for EOF calculation and mask the result with NAs again
#'  after the calculation.
#'}
#'\item{wght}{
#'  An array of the area weighting with dimensions 'space_dim'. It is calculated
#'  by cosine of 'lat' and used to compute the fraction of variance explained by
#'  each EOFs.
#'}
#'\item{tot_var}{
#'  A number or a numeric array of the total variance explained by all the modes.
#'  The dimensions are same as 'ano' except 'time_dim' and 'space_dim'.
#'}
#'
#'@seealso ProjectField, NAO, PlotBoxWhisker
#'@examples
#'# This example computes the EOFs along forecast horizons and plots the one 
#'# that explains the greatest amount of variability. The example data has low  
#'# resolution so the result may not be explanatory, but it displays how to 
#'# use this function.
#'\dontshow{
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- s2dv:::.LoadSampleData('tos', c('experiment'),
#'                                     c('observation'), startDates,
#'                                     leadtimemin = 1,
#'                                     leadtimemax = 4,
#'                                     output = 'lonlat',
#'                                     latmin = 27, latmax = 48,
#'                                     lonmin = -12, lonmax = 40)
#'}
#'ano <- Ano_CrossValid(sampleData$mod, sampleData$obs)
#'tmp <- MeanDims(ano$exp, c('dataset', 'member'))
#'ano <- tmp[1, , ,]
#'names(dim(ano)) <- names(dim(tmp))[-2]
#'eof <- EOF(ano, sampleData$lat, sampleData$lon)
#'\dontrun{
#'PlotEquiMap(eof$EOFs[1, , ], sampleData$lon, sampleData$lat)
#'}
#'
#'@import multiApply
#'@importFrom stats sd
#'@export
EOF <- function(ano, lat, lon, time_dim = 'sdate', space_dim = c('lat', 'lon'), 
                neofs = 15, corr = FALSE, ncores = NULL) {

  # Check inputs 
  ## ano
  if (is.null(ano)) {
    stop("Parameter 'ano' cannot be NULL.")
  }
  if (!is.numeric(ano)) {
    stop("Parameter 'ano' must be a numeric array.")
  }
  if (any(is.null(names(dim(ano)))) | any(nchar(names(dim(ano))) == 0)) {
    stop("Parameter 'ano' must have dimension names.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(ano))) {
    stop("Parameter 'time_dim' is not found in 'ano' dimension.")
  }
  ## space_dim
  if (!is.character(space_dim) | length(space_dim) != 2) {
    stop("Parameter 'space_dim' must be a character vector of 2.")
  }
  if (!all(space_dim %in% names(dim(ano)))) {
    stop("Parameter 'space_dim' is not found in 'ano' dimension.")
  }
  ## lat
  if (!is.numeric(lat) | length(lat) != dim(ano)[space_dim[1]]) {
    stop("Parameter 'lat' must be a numeric vector with the same ",
         "length as the latitude dimension of 'ano'.")
  }
  if (any(lat > 90 | lat < -90)) {
    stop("Parameter 'lat' must contain values within the range [-90, 90].")
  }
  ## lon
  if (!is.numeric(lon) | length(lon) != dim(ano)[space_dim[2]]) {
    stop("Parameter 'lon' must be a numeric vector with the same ",
         "length as the longitude dimension of 'ano'.")
  }
  if (any(lon > 360 | lon < -360)) {
    .warning("Some 'lon' is out of the range [-360, 360].")
  }
  ## neofs
  if (!is.numeric(neofs) | neofs %% 1 != 0 | neofs <= 0 | length(neofs) > 1) {
    stop("Parameter 'neofs' must be a positive integer.")
  }
  ## corr
  if (!is.logical(corr) | length(corr) > 1) {
    stop("Parameter 'corr' must be one logical value.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }

  ###############################
  # Calculate EOF

#  # Replace mask of NAs with 0s for EOF analysis.
#  ano[!is.finite(ano)] <- 0

  # Area weighting. Weights for EOF; needed to compute the
  # fraction of variance explained by each EOFs
  space_ind <- sapply(space_dim, function(a) which(names(dim(ano)) == a))
  wght <- array(cos(lat * pi / 180), dim = dim(ano)[space_ind])
  
  # We want the covariance matrix to be weigthed by the grid
  # cell area so the anomaly field is weighted by its square
  # root since the covariance matrix equals transpose(ano)
  # times ano.
  wght <- sqrt(wght)

  # neofs is bounded
  if (neofs != min(dim(ano)[time_dim], prod(dim(ano)[space_dim]), neofs)) {
    neofs <- min(dim(ano)[time_dim], prod(dim(ano)[space_dim]), neofs)
    .warning(paste0("Parameter 'neofs' is changed to ", neofs, ", the minimum among ",
                    "the length of time_dim, the production of the length of space_dim, ",
                    "and neofs."))
  }

  res <- Apply(ano, 
               target_dims = c(time_dim, space_dim), 
               output_dims = list(EOFs = c('mode', space_dim),
                                  PCs = c(time_dim, 'mode'),
                                  var = 'mode',
                                  tot_var = NULL,
                                  mask = space_dim),
               fun = .EOF, 
               corr = corr, neofs = neofs,
               wght = wght,
               ncores = ncores)

  return(c(res, wght = list(wght)))

}

.EOF <- function(ano, neofs = 15, corr = FALSE, wght = wght) {
  # ano: [time, lat, lon]

  # Dimensions
  nt <- dim(ano)[1]
  ny <- dim(ano)[2]
  nx <- dim(ano)[3]

  # Check if all the time steps at one grid point are NA-consistent.
  # The grid point should have all NAs or no NA along time dim.
  if (anyNA(ano)) {
    ano_latlon <- array(ano, dim = c(nt, ny * nx))  # [time, lat*lon]
    na_ind <- which(is.na(ano_latlon), arr.ind = T)
    if (dim(na_ind)[1] != nt * length(unique(na_ind[, 2]))) {
      stop("Detect certain grid points have NAs but not consistent across time ",
           "dimension. If the grid point is NA, it should have NA at all time step.")
    }
  }

  # Build the mask
  mask <- ano[1, , ]
  mask[!is.finite(mask)] <- NA
  mask[is.finite(mask)] <- 1
  dim(mask) <- c(ny, nx) 

  # Replace mask of NAs with 0s for EOF analysis.
  ano[!is.finite(ano)] <- 0

  ano <- ano * InsertDim(wght, 1, nt)

  # The use of the correlation matrix is done under the option corr.
  if (corr) {
    stdv <- apply(ano, c(2, 3), sd, na.rm = T)
    ano <- ano / InsertDim(stdv, 1, nt)
  }
  
  # Time/space matrix for SVD
  dim(ano) <- c(nt, ny * nx)
  dim.dat <- dim(ano)
  
  # 'transpose' means the array needs to be transposed before
  # calling La.svd for computational efficiency because the
  # spatial dimension is larger than the time dimension. This
  # goes with transposing the outputs of LA.svd also.
  if (dim.dat[2] > dim.dat[1]) {
    transpose <- TRUE
  } else {
    transpose <- FALSE
  }
  if (transpose) {
    pca <- La.svd(t(ano))
  } else {
    pca <- La.svd(ano)
  }
  
  # La.svd conventions: decomposition X = U D t(V) La.svd$u
  # returns U La.svd$d returns diagonal values of D La.svd$v
  # returns t(V) !!  The usual convention is PC=U and EOF=V.
  # If La.svd is called for ano (transpose=FALSE case): EOFs:
  # $v PCs: $u If La.svd is called for t(ano) (transposed=TRUE
  # case): EOFs: t($u) PCs: t($v)

  if (transpose) {
    pca.EOFs <- t(pca$u)
    pca.PCs <- t(pca$v)
  } else {
    pca.EOFs <- pca$v
    pca.PCs <- pca$u
  }
  
  # The numbers of transposition is limited to neofs
  PC <- pca.PCs[, 1:neofs]
  EOF <- pca.EOFs[1:neofs, ]
  dim(EOF) <- c(neofs, ny, nx)

  # To sort out crash when neofs=1.
  if (neofs == 1) {
    PC <- InsertDim(PC, 2, 1, name = 'new')
  }
  
  # Computation of the % of variance associated with each mode
  W <- pca$d[1:neofs]
  tot.var <- sum(pca$d^2)
  var.eof <- 100 * pca$d[1:neofs]^2 / tot.var

  for (e in 1:neofs) {
    # Set all masked grid points to NA in the EOFs
    # Divide patterns by area weights so that EOF * PC gives unweigthed (original) data
    EOF[e, , ] <- EOF[e, , ] * mask / wght    
    # PC is multiplied by the explained variance,
    # so that the reconstruction is only EOF * PC 
    PC[, e] <- PC[, e] * W[e]
  }

  if (neofs == 1) {
    var.eof <- as.array(var.eof)
  }

  return(invisible(list(EOFs = EOF, PCs = PC, var = var.eof, tot_var = tot.var, mask = mask)))
}
