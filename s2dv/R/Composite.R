#'Compute composites
#'
#'Composite a multi-dimensional array which contains two spatial and one 
#'temporal dimensions, e.g., (lon, lat, time), according to the indices of 
#'mode/cluster occurrences in time. The p-value by t-test is also computed. 
#'
#'@param data A numeric array containing two spatial and one temporal 
#'  dimensions.
#'@param occ A vector of the occurrence time series of mode(s)/cluster(s).
#'  The length should be the same as the temporal dimension in 'data'.
#'  (*1) When one wants to composite all modes, e.g., all K = 3 clusters then 
#'    for example occurrences could look like: 1 1 2 3 2 3 1 3 3 2 3 2 2 3 2.
#'  (*2) Otherwise for compositing only the 2nd mode or cluster of the above 
#'    example occurrences should look like 0 0 1 0 1 0 0 0 0 1 0 1 1 0 1.
#'@param time_dim A character string indicating the name of the temporal 
#'  dimension in 'data'. The default value is 'time'.
#'@param space_dim A character vector indicating the names of the spatial 
#'  dimensions in 'data'. The default value is c('lon', 'lat').
#'@param lag An integer indicating the lag time step. E.g., for lag = 2, 
#'  +2 occurrences will be used (i.e., shifted 2 time steps forward). 
#'  The default value is 0.
#'@param eno A logical value indicating whether to use the effective sample 
#'  size (TRUE) or the total sample size (FALSE) for the number of degrees of 
#'  freedom. The default value is FALSE.
#'@param K A numeric value indicating the maximum number of composites. The 
#'  default value is NULL, which means the maximum value provided in 'occ' is 
#'  used.
#'@param fileout A character string indicating the name of the .sav output file
#'  The default value is NULL, which means not to save the output.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return 
#'A list containing:
#'\item{$composite}{ 
#'  A numeric array of the spatial dimensions and new dimension 'K' first, 
#'  followed by the same dimensions as parameter 'data'. The length of 
#'  dimension 'K' is parameter 'K'.
#'}
#'\item{$p.val}{
#'  A numeric array with the same dimension as $composite. It is the p-value of
#'  the composites obtained through a t-test that accounts for the serial
#'  dependence of the data.
#'}
#'
#'@examples
#'blank <- array(0, dim = c(20, 10, 30))
#'x1 <- blank
#'t1 <- blank
#'f1 <- blank
#'
#'for (i in 1:20) {
#'  x1[i, , ] <- i
#'}
#'
#'for (i in 1:30) {
#'  t1[, , i] <- i
#'}
#'
#'# This is 2D propagating sin wave example, where we use f1(lon, lat, time) 
#'# wave field. Compositing (like using stroboscopicc light) at different
#'# time steps can lead to modification or cancelation of wave pattern.
#'
#'for (i in 1:20) {
#'  for (j in 1:30) {
#'    f1[i, , j] <- 3 * sin(2 * pi * x1[i, , j] / 5. - 2 * pi * t1[i, , j] / 6.)
#'  }
#'}
#'names(dim(f1)) <- c('lon', 'lat', 'time')
#'occ <- rep(0, 30)
#'occ[c(2, 5, 8, 11, 14, 17, 20, 23)] <- 1
#'res <- Composite(data = f1, occ = occ)
#'filled.contour(res$composite[, , 1])
#'
#'occ <- rep(0, 30)
#'occ[c(3, 9, 15, 21)] <- 1
#'res <- Composite(data = f1, occ = occ)
#'filled.contour(res$composite[, , 1])
#'
#'# Example with one missing composite in occ:
#'data <- 1:(4 * 5 * 6)
#'dim(data) <- c(lon = 4, lat = 5, case = 6)
#'occ <- c(1, 1, 2, 2, 3, 3) 
#'res <- Composite(data, occ, time_dim = 'case',  K = 4)
#'
#'@importFrom stats sd pt
#'@import multiApply
#'@export
Composite <- function(data, occ, time_dim = 'time', space_dim = c('lon', 'lat'),
                      lag = 0, eno = FALSE, K = NULL, fileout = NULL, ncores = NULL) {

  # Check inputs 
  ## data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  if (length(dim(data)) < 3) {
    stop("Parameter 'data' must have at least three dimensions.")
  }
  if (any(is.null(names(dim(data)))) | any(nchar(names(dim(data))) == 0)) {
    stop("Parameter 'data' must have dimension names.")
  }
  ## occ
  if (is.null(occ)) {
    stop("Parameter 'occ' cannot be NULL.")
  }
  if (!is.numeric(occ) | length(dim(occ)) > 1) {
    stop("Parameter 'occ' must be a numeric vector.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(data))) {
    stop("Parameter 'time_dim' is not found in 'data' dimension.")
  }
  if (dim(data)[time_dim] != length(occ)) {
     stop("The length of time_dim dimension in parameter 'data' is not ",
          "equal to length of parameter 'occ'.")
  }
  ## space_dim
  if (!is.character(space_dim) | length(space_dim) != 2) {
    stop("Parameter 'space_dim' must be a character vector with two elements.")
  }
  if (!all(space_dim %in% names(dim(data)))) {
    # See if 'longitude' and 'latitude' exist
    if (all(c('longitude', 'latitude') %in% names(dim(data)))) {
      space_dim <- c('longitude', 'latitude')
    } else {
      stop("Parameter 'space_dim' is not found in 'data' dimension.")
    }
  }
  ## lag
  if (!is.null(lag)) {
    if (!is.numeric(lag) | lag %% 1 != 0 | lag < 0 | length(lag) > 1) {
      stop("Parameter 'lag' must be a non-negative integer.")
    }
  }
  ## eno
  if (!is.logical(eno) | length(eno) > 1) {
    stop("Parameter 'eno' must be one logical value.")
  }
  ## K
  if (!is.null(K)) {
    if (!is.numeric(K) | K %% 1 != 0 | K < 1 | length(K) > 1) {
      stop("Parameter 'K' must be a positive integer.")
    }
  }
  ## fileout
  if (!is.null(fileout)) {
    if (!is.character(fileout) | length(fileout) > 1) {
      stop("Parameter 'fileout' must be a character string.")
    }
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }

  ###############################
  # Calculate Composite

  ## Check if any occ is only 1 case
  count_k <- plyr::count(occ)
  if (any(count_k$freq == 1)) {
    tmp <- count_k$x[which(count_k$freq == 1)]
    .warning(paste0("Composite K = ", tmp, " has length 1. The p-value is NA."))
  }

  output_dims <- list(composite = c(space_dim, 'K'), 
                      p.val = c(space_dim, 'K'))

  output <- Apply(list(data),
                  target_dims = list(c(space_dim, time_dim)),
                  fun = .Composite,
                  output_dims = output_dims,
                  occ = occ, time_dim = time_dim, space_dim = space_dim,
                  K = K, lag = lag, eno = eno, ncores_input = ncores,
                  ncores = ncores)

  if (!is.null(fileout)) {
    save(output, file = paste0(fileout, '.sav'))
  }

  return(output)
}

.Composite <- function(data, occ, time_dim = 'time', space_dim = c('lon', 'lat'), 
                       K = NULL, lag = 0, eno = FALSE, ncores_input = NULL) {
# data: [lon, lat, time]
# occ: [time]
  if (is.null(K)) {
     K <- max(occ)
  }
  composite <- array(dim = c(dim(data)[1:2], composite = K))
  tvalue <- array(dim = dim(data)[1:2])
  dof <- array(dim = dim(data)[1:2])
  pval <- array(dim = c(dim(data)[1:2], composite = K))

  if (eno) { 
    n_tot <- Eno(data, time_dim = time_dim, ncores = ncores_input)
  } else {
    n_tot <- length(occ)
  }

  mean_tot <- MeanDims(data, dims = 3, na.rm = TRUE)
  stdv_tot <- apply(data, c(1, 2), sd, na.rm = TRUE) 

  for (k in 1:K) {

    if (any(occ == k)) {
      indices <- which(occ == k) + lag
      toberemoved <-  which(0 > indices | indices > dim(data)[3])

    if (length(toberemoved) > 0) {
        indices <- indices[-toberemoved]
    }
    if (eno) {
        data_tmp <- data[, , indices]
        names(dim(data_tmp)) <- names(dim(data))
        n_k <- Eno(data_tmp, time_dim = time_dim, ncores = ncores_input)
    } else {
        n_k <- length(indices)
    }
    if (length(indices) == 1) {
        composite[, , k] <- data[, , indices] 
    }  else {
        composite[, , k] <- MeanDims(data[, , indices], dims = 3, na.rm = TRUE)
    }
    stdv_k <- apply(data[, , indices], c(1, 2), sd, na.rm = TRUE)
    
    tvalue <- (mean_tot - composite[, , k]) / 
               sqrt(stdv_tot^2 / n_tot + stdv_k^2 / n_k)
    dof <- (stdv_tot^2 / n_tot + stdv_k^2 / n_k)^2 / 
           ((stdv_tot^2 / n_tot)^2 / (n_tot - 1) +
           (stdv_k^2 / n_k)^2 / (n_k - 1))
    pval[, , k] <- 2 * pt(-abs(tvalue), df = dof)
    }
  }  

  invisible(list(K = composite, p.val = pval)) 
}
