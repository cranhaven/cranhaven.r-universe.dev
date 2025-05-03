#'@rdname CST_RegimesAssign
#'@title Function for matching a field of anomalies with 
#'a set of maps used as a reference (e.g. clusters obtained from the WeatherRegime function)
#'
#'@author Verónica Torralba - BSC, \email{veronica.torralba@bsc.es}
#'
#'@description This function performs the matching between a field of anomalies 
#'and a set of maps which will be used as a reference. The anomalies will be 
#'assigned to the reference map for which the minimum Eucledian distance 
#'(method =’distance’) or highest spatial correlation (method = 'ACC') is 
#'obtained. 
#'
#'@references Torralba, V. (2019) Seasonal climate prediction for the wind 
#'energy sector: methods and tools for the development of a climate service. 
#'Thesis. Available online: \url{https://eprints.ucm.es/56841/}
#'
#'@param data An 's2dv_cube' object.
#'@param ref_maps An 's2dv_cube' object as the output of CST_WeatherRegimes.  
#'@param method Whether the matching will be performed in terms of minimum 
#'  distance (default = 'distance') or the maximum spatial correlation 
#'  (method = 'ACC') between the maps.
#'@param composite A logical parameter indicating if the composite maps are 
#'  computed or not (default = FALSE).
#'@param memb A logical value indicating whether to compute composites for 
#'  separate members (default FALSE) or as unique ensemble (TRUE). This option 
#'  is only available for when parameter 'composite' is set to TRUE and the data 
#'  object has a dimension named 'member'.
#'@param ncores The number of multicore threads to use for parallel computation.
#'@return A list with two elements \code{$data} (a 's2dv_cube' object containing 
#'the composites cluster=1,..,K for case (*1) or only k=1 for any specific 
#'cluster, i.e., case (*2)) (only when composite = 'TRUE') and \code{$statistics} 
#'that includes \code{$pvalue} (array with the same structure as \code{$data} 
#'containing the pvalue of the composites obtained through a t-test that 
#'accounts for the serial dependence of the data with the same structure as 
#'Composite.)(only when composite = 'TRUE'), \code{$cluster} (array with the 
#'same dimensions as data (except latitude and longitude which are removed) 
#'indicating the ref_maps to which each point is allocated.), \code{$frequency} 
#'(A vector of integers (from k=1,...k n reference maps) indicating the 
#'percentage of assignations corresponding to each map.).
#'@examples
#'data <- array(abs(rnorm(1280, 282.7, 6.4)), dim = c(dataset = 2, member = 2, 
#'                                                    sdate = 3, ftime = 3, 
#'                                                    lat = 4, lon = 4))
#'coords <- list(lon = seq(0, 3), lat = seq(47, 44))
#'exp <- list(data = data, coords = coords)
#'class(exp) <- 's2dv_cube'
#'regimes <- CST_WeatherRegimes(data = exp, EOFs = FALSE, 
#'                              ncenters = 4)
#'res1 <- CST_RegimesAssign(data = exp, ref_maps = regimes, 
#'                          composite = FALSE)
#'@importFrom s2dv ACC MeanDims InsertDim
#'@import multiApply
#'@export
CST_RegimesAssign <- function(data, ref_maps, 
                              method = "distance", 
                              composite = FALSE,
                              memb = FALSE, ncores = NULL) {
  # Check 's2dv_cube'
  if (!inherits(data, 's2dv_cube')) {
    stop("Parameter 'data' must be of the class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  if (!inherits(ref_maps, 's2dv_cube')) {
    stop("Parameter 'ref_maps' must be of the class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  # Check 'exp' object structure
  if (!all(c('data', 'coords') %in% names(data))) {
    stop("Parameter 'data' must have 'data' and 'coords' elements ",
         "within the 's2dv_cube' structure.")
  }
  # Check coordinates
  if (!any(names(data$coords) %in% .KnownLatNames())) {
    stop("Spatial coordinate names do not match any of the names accepted ",
         "the package.")
  } else {
    lat_name <- names(data$coords)[[which(names(data$coords) %in% .KnownLatNames())]]
    lat <- as.vector(data$coords[[lat_name]])
  }

  result <- RegimesAssign(data = data$data, ref_maps = ref_maps$data, lat = lat, 
                          method = method, composite = composite, 
                          memb = memb, ncores = ncores)
  
  if (composite) {
    data$data <- result$composite
    data$statistics <- result[-1]
  } else {
    data <- NULL
    data$statistics <- result
  }

  return(data)
}

#'@rdname RegimesAssign
#'@title Function for matching a field of anomalies with 
#'a set of maps used as a reference (e.g. clusters obtained from the WeatherRegime function).
#'
#'@author Verónica Torralba - BSC, \email{veronica.torralba@bsc.es}
#'
#'@description This function performs the matching between a field of anomalies 
#'and a set of maps which will be used as a reference. The anomalies will be 
#'assigned to the reference map for which the minimum Eucledian distance 
#'(method = 'distance') or highest spatial correlation (method = 'ACC') is 
#'obtained. 
#'
#'@references Torralba, V. (2019) Seasonal climate prediction for the wind 
#'energy sector: methods and tools for the development of a climate service. 
#'Thesis. Available online: \url{https://eprints.ucm.es/56841/}
#'
#'@param data An array containing anomalies with named dimensions: dataset, 
#'  member, sdate, ftime, lat and lon.
#'@param ref_maps Array with 3-dimensions ('lon', 'lat', 'cluster') containing 
#'  the maps/clusters that will be used as a reference for the matching. 
#'@param method Whether the matching will be performed in terms of minimum 
#'  distance (default = 'distance') or the maximum spatial correlation 
#'  (method = 'ACC') between the maps.
#'@param lat A vector of latitudes corresponding to the positions provided in 
#'  data and ref_maps.
#'@param composite A logical parameter indicating if the composite maps are 
#'  computed or not (default = FALSE).
#'@param memb A logical value indicating whether to compute composites for 
#'  separate members (default FALSE) or as unique ensemble (TRUE). This option 
#'  is only available for when parameter 'composite' is set to TRUE and the data 
#'  object has a dimension named 'member'.
#'@param ncores The number of multicore threads to use for parallel computation.
#'@return A list with elements \code{$composite} (3-d array (lon, lat, k) 
#'containing the composites k = 1,..,K for case (*1) or only k = 1 for any specific 
#'cluster, i.e., case (*2)) (only if composite = 'TRUE'), \code{$pvalue} (array 
#'with the same structure as \code{$composite} containing the pvalue of the 
#'composites obtained through a t-test that accounts for the serial dependence 
#'of the data with the same structure as Composite.) (only if composite='TRUE'),
#'\code{$cluster} (array with the same dimensions as data (except latitude and 
#'longitude which are removed) indicating the ref_maps to which each point is 
#'allocated.), \code{$frequency} (A vector of integers (from k = 1, ... k n 
#'reference maps) indicating the percentage of assignations corresponding to 
#'each map.),
#' 
#'@examples 
#'data <- array(abs(rnorm(1280, 282.7, 6.4)), dim = c(dataset = 2, member = 2, 
#'                                                    sdate = 3, ftime = 3, 
#'                                                    lat = 4, lon = 4))
#'regimes <- WeatherRegime(data = data, lat = seq(47, 44),
#'                         EOFs = FALSE, ncenters = 4)$composite
#'res1 <- RegimesAssign(data = data, ref_maps = drop(regimes), 
#'                      lat = seq(47, 44), composite = FALSE)
#'@importFrom s2dv ACC MeanDims Eno InsertDim
#'@import multiApply   
#'@export
RegimesAssign <- function(data, ref_maps, lat, method = "distance", composite = FALSE, 
                          memb = FALSE, ncores = NULL) {
  ## Initial checks
  # data
  if (is.null(names(dim(data)))) {
    stop("Parameter 'data' must be an array with named dimensions.")
  }
  # ref_maps
  if (is.null(ref_maps)) {
    stop("Parameter 'ref_maps' must be specified.")
  }
  if (is.null(names(dim(ref_maps)))) {
    stop("Parameter 'ref_maps' must be an array with named dimensions.")
  }
  # lat
  if (is.null(lat)) {
    stop("Parameter 'lat' must be specified.")
  }
  # memb
  if (!is.logical(memb)) {
      stop("Parameter 'memb' must be logical.")
  }
  # composite
  if (!is.logical(composite)) {
      stop("Parameter 'memb' must be logical.")
  }
  dimData <- names(dim(data))
  # Know spatial coordinates names
  if (!any(dimData %in% .KnownLonNames()) | 
      !any(dimData %in% .KnownLatNames())) {
    stop("Spatial coordinate dimension names do not match any of the names ",
         "accepted by the package.")
  }
  lon_name <- dimData[[which(dimData %in% .KnownLonNames())]]
  lat_name <- dimData[[which(dimData %in% .KnownLatNames())]]
  dimRef <- names(dim(ref_maps))
  if (!any(dimRef %in% .KnownLonNames()) | 
      !any(dimRef %in% .KnownLatNames())) {
    stop("Spatial coordinate dimension names do not match any of the names ",
         "accepted by the package.")
  }
  lon_name_ref <- dimRef[[which(dimRef %in% .KnownLonNames())]]
  lat_name_ref <- dimRef[[which(dimRef %in% .KnownLatNames())]]
  if (!all( c('cluster', lat_name_ref, lon_name_ref) %in% dimRef)) {
   stop("Parameter 'ref_maps' must contain the named dimensions 
        'cluster', and the spatial coordinates accepted names.")
  }
  if (length(lat) != dim(data)[lat_name] | 
      (length(lat) != dim(ref_maps)[lat_name_ref])) {
    stop("Parameter 'lat' does not match with the latitudinal dimension",
         " in the parameter 'data' or in the parameter 'ref_maps'.")
  }
  # Temporal dimensions
  if ('sdate' %in% dimData && 'ftime' %in% dimData) {
    nsdates <- dim(data)['sdate']
    nftimes <- dim(data)['ftime']
    data <- MergeDims(data, 
                      merge_dims = c('ftime','sdate'), 
                      rename_dim = 'time')
  } else if ('sdate' %in% dimData | 'ftime' %in% dimData) {
    names(dim(data))[which(dimData == 'sdate' | dimData == 'ftime') ] = 'time' 
  } else {
    if (!('time' %in% dimData)) {
      stop("Parameter 'data' must have temporal dimensions.")
    }
  }
  ref_maps <- drop(ref_maps) 
  index <- Apply(data = list(ref = ref_maps, target = data),
                 target_dims = list(c(lat_name_ref, lon_name_ref, 'cluster'), 
                                    c(lat_name, lon_name)),
                 fun = .RegimesAssign,
                 lat = lat,  method = method, 
                 lon_name = lon_name, lat_name = lat_name,
                 lon_name_ref = lon_name_ref, lat_name_ref = lat_name_ref, 
                 ncores = ncores)[[1]]

  nclust <- dim(ref_maps)['cluster']
  freqs <- rep(NA, nclust)
  for (n in 1:nclust) {
    freqs[n] <- (length(which(index == n)) / length(index)) * 100
  }
  
  if (composite) {
    poslon <- which(names(dim(data)) == lon_name)
    poslat <- which(names(dim(data)) == lat_name)
    postime <- which(names(dim(data)) == 'time')
    posdim <- setdiff(1:length(dim(data)), c(postime, poslat, poslon))
    dataComp <- aperm(data, c(poslon, poslat, postime, posdim))
    
    if (any(is.na(index))) {
      recon <-list(
          composite = InsertDim(array(NA, dim = c(dim(dataComp)[-postime])), 
                                postime, dim(ref_maps)['composite.cluster'], name = ''),
          pvalue = InsertDim(array(NA, dim = c(dim(dataComp)[-postime])), 
                             postime, dim(ref_maps)['composite.cluster'], name = ''))
    } else {
      if (memb) {
        dataComp <- MergeDims(dataComp, merge_dims = c('time', 'member'), rename_dim = 'time')
        index <- MergeDims(index, merge_dims = c('time', 'member'), rename_dim = 'time') 
      }
      recon <- Apply(data = list(var = dataComp, occ = index),
                     target_dims = list(c(lon_name, lat_name, 'time'), c('time')),
                     fun = Composite,
                     K = dim(ref_maps)['cluster'])
    }
    output <- list(composite = recon$composite,
                   pvalue = recon$pvalue,
                   cluster = index,
                   frequency = freqs)
  } else {
    output <- list(cluster = index,
                   frequency = freqs)
  }
  
  return(output)
}

.RegimesAssign <- function(ref, target, method = 'distance', lat, 
                           composite = FALSE, 
                           lon_name = 'lon', lat_name = 'lat', 
                           lon_name_ref = 'lon', lat_name_ref = 'lat') {

  # ref: [lat_name_ref, lon_name_ref, 'cluster']
  # target: [lat_name, lon_name]

  posdim <- which(names(dim(ref)) == 'cluster')
  poslat <- which(names(dim(ref)) == lat_name_ref)
  poslon <- which(names(dim(ref)) == lon_name_ref)
  
  nclust <- dim(ref)[posdim]
  if (all(dim(ref)[-posdim] != dim(target))) {
    stop('The target should have the same dimensions [lat_name, lon_name] that',
         'the reference ')
  }
  if (is.null(names(dim(ref))) | is.null(names(dim(target)))) {
    stop('The arrays should include dimensions names ref[cluster, lat_name, ',
         'lon_name] and target [lat_name, lon_name]'
    )
  }
  if (length(lat) != dim(ref)[poslat]) {
    stop('latitudes do not match with the maps')
  }
 if (is.na(max(target))){
    assign <- NA
  } else {
    # This dimensions are reorganized
    ref <- aperm(ref, c(posdim, poslat, poslon))
    target <- aperm(target, 
                    c(which(names(dim(target)) == lat_name), 
                      which(names(dim(target)) == lon_name)))
    
    # weights are defined
    latWeights <- InsertDim(sqrt(cos(lat * pi / 180)), 2, dim(ref)[3])
    
    rmsdiff <- function(x, y) {
      dims <- dim(x)
      ndims <- length(dims)
      if (ndims != 2 | ndims != length(dim(y))) {
        stop('x and y should be maps')
      }
      map_diff <- NA * x
      for (i in 1:dims[1]) {
        for (j in 1:dims[2]) {
          map_diff[i, j] <- (x[i, j] - y[i, j]) ^ 2
        }
      }
      rmsdiff <- sqrt(mean(map_diff))
      return(rmsdiff)
    }
    
    if (method == 'ACC') {
      corr <- rep(NA, nclust)
      for (i in 1:nclust) {
        #NOTE: s2dv::ACC returns centralized and weighted result.
        corr[i] <-
          ACC(ref[i, , ], target, lat = lat, dat_dim = NULL, avg_dim = NULL, 
              memb_dim = NULL)$acc
      }
      assign <- which(corr == max(corr))
    }
    
    if (method == 'distance') {
      rms <- rep(NA, nclust)
      for (i in 1:nclust) {
        rms[i] <- rmsdiff(ref[i, , ] * latWeights, target * latWeights)
      }
      assign <- which(rms == min(rms))
    }
  }
  
  return(assign)
}


Composite <- function(var, occ, lag = 0, eno = FALSE, K = NULL, fileout = NULL) {

  if ( dim(var)[3] != length(occ) ) {
     stop("Temporal dimension of var is not equal to length of occ.")
  }
  if (is.null(K)) {
     K <- max(occ)
  }
  composite <- array(dim = c(dim(var)[1:2], composite = K))
  tvalue    <- array(dim = dim(var)[1:2])
  dof       <- array(dim = dim(var)[1:2])
  pvalue    <- array(dim = c(dim(var)[1:2], composite = K))

  if (eno == TRUE) { 
    n_tot <- Eno(var, time_dim = 'time')
  } else {
    n_tot <- length(occ)
  }
  mean_tot <- MeanDims(var, dims = 3, na.rm = TRUE)
  stdv_tot <- apply(var, c(1, 2), sd, na.rm = TRUE) 

  for (k in 1 : K) {
    if (length(which(occ == k)) >= 1) {
      indices <- which(occ == k) + lag
      toberemoved <-  which(0 > indices | indices > dim(var)[3])

      if (length(toberemoved) > 0) {
        indices <- indices[-toberemoved]
      }
      if (eno == TRUE) {
        n_k <- Eno(var[, , indices], time_dim = 'time')
      } else {
        n_k <- length(indices)
      }
      if (length(indices) == 1) {
        composite[, , k] <- var[, , indices] 
        warning(paste("Composite", k, "has length 1 and pvalue is NA."))
      }  else {
        composite[, , k] <- MeanDims(var[, , indices], dims = 3, na.rm = TRUE)
      }
      stdv_k <- apply(var[, , indices], c(1, 2), sd, na.rm = TRUE)
    
      tvalue <- (mean_tot - composite[, , k]) / 
                 sqrt(stdv_tot ^ 2 / n_tot + stdv_k ^ 2 / n_k)
      dof <- (stdv_tot ^ 2 / n_tot + stdv_k ^ 2 / n_k) ^ 2 / 
             ((stdv_tot ^ 2 / n_tot) ^ 2 / (n_tot - 1) +
             (stdv_k ^ 2 / n_k) ^ 2 / (n_k - 1))
      pvalue[, , k] <- 2 * pt(-abs(tvalue), df = dof)
    }  
  }
  if (is.null(fileout) == FALSE) { 
    output <- list(composite = composite, pvalue = pvalue)   
    save(output, file = paste(fileout, '.sav', sep = ''))
  }
  
  invisible(list(composite = composite, pvalue = pvalue)) 
}


