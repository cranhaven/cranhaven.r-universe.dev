#'@rdname CST_EnsClustering
#'@title Ensemble clustering
#'
#'@author Federico Fabiano - ISAC-CNR, \email{f.fabiano@isac.cnr.it}
#'@author Ignazio Giuntoli - ISAC-CNR, \email{i.giuntoli@isac.cnr.it}
#'@author Danila Volpi - ISAC-CNR, \email{d.volpi@isac.cnr.it}
#'@author Paolo Davini - ISAC-CNR, \email{p.davini@isac.cnr.it}
#'@author Jost von Hardenberg - ISAC-CNR, \email{j.vonhardenberg@isac.cnr.it}
#'
#'@description This function performs a clustering on members/starting dates
#'and returns a number of scenarios, with representative members for each of 
#'them. The clustering is performed in a reduced EOF space.
#' 
#'Motivation:
#'Ensemble forecasts give a probabilistic insight of average weather conditions
#'on extended timescales, i.e. from sub-seasonal to seasonal and beyond.
#'With large ensembles, it is often an advantage to be able to group members
#'according to similar characteristics and to select the most representative 
#'member for each cluster. This can be useful to characterize the most probable 
#'forecast scenarios in a multi-model (or single model) ensemble prediction.  
#'This approach, applied at a regional level, can also be used to identify the 
#'subset of ensemble members that best represent the full range of possible 
#'solutions for downscaling applications. The choice of the ensemble members is 
#'made flexible in order to meet the requirements of specific (regional) climate 
#'information products, to be tailored for different regions and user needs. 
#'
#'Description of the tool:
#'EnsClustering is a cluster analysis tool, based on the k-means algorithm, for 
#'ensemble predictions. The aim is to group ensemble members according to 
#'similar characteristics and to select the most representative member for each 
#'cluster. The user chooses which feature of the data is used to group the 
#'ensemble members by clustering: time mean, maximum, a certain percentile 
#'(e.g., 75% as in the examples below), standard deviation and trend over the 
#'time period. For each ensemble member this value is computed at each grid 
#'point, obtaining N lat-lon maps, where N is the number of ensemble members.
#'The anomaly is computed subtracting the ensemble mean of these maps to each of 
#'the single maps. The anomaly is therefore computed with respect to the 
#'ensemble members (and not with respect to the time) and the Empirical 
#'Orthogonal Function (EOF) analysis is applied to these anomaly maps. Regarding 
#'the EOF analysis, the user can choose either how many Principal Components 
#'(PCs) to retain or the percentage of explained variance to keep. After 
#'reducing dimensionality via EOF analysis, k-means analysis is applied using 
#'the desired subset of PCs. 
#'
#'The major final outputs are the classification in clusters, i.e. which member 
#'belongs to which cluster (in k-means analysis the number k of clusters needs 
#'to be defined prior to the analysis) and the most representative member for 
#'each cluster, which is the closest member to the cluster centroid. Other 
#'outputs refer to the statistics of clustering: in the PC space, the minimum 
#'and the maximum distance between a member in a cluster and the cluster 
#'centroid (i.e. the closest and the furthest member), the intra-cluster 
#'standard deviation for each cluster (i.e. how much the cluster is compact).
#'
#'@param exp An object of the class 's2dv_cube', containing the variables to be 
#'  analysed. The element 'data' in the 's2dv_cube' object must have, at
#'  least, spatial and temporal dimensions. Latitudinal dimension accepted 
#'  names: 'lat', 'lats', 'latitude', 'y', 'j', 'nav_lat'. Longitudinal 
#'  dimension accepted names: 'lon', 'lons','longitude', 'x', 'i', 'nav_lon'.
#'@param time_moment Decides the moment to be applied to the time dimension. Can 
#'  be either 'mean' (time mean), 'sd' (standard deviation along time) or 'perc' 
#'  (a selected percentile on time). If 'perc' the keyword 'time_percentile' is 
#'  also used.
#'@param time_percentile Set the percentile in time you want to analyse (used 
#'  for `time_moment = "perc").
#'@param numclus Number of clusters (scenarios) to be calculated. If set to NULL 
#'  the number of ensemble members divided by 10 is used, with a minimum of 2 
#'  and a maximum of 8.
#'@param lon_lim List with the two longitude margins in `c(-180,180)` format.
#'@param lat_lim List with the two latitude margins.
#'@param variance_explained variance (percentage) to be explained by the set of 
#'  EOFs. Defaults to 80. Not used if numpcs is specified.
#'@param numpcs Number of EOFs retained in the analysis (optional).
#'@param cluster_dim Dimension along which to cluster. Typically "member" or 
#'  "sdate". This can also be a list like c("member", "sdate").
#'@param time_dim String or character array with name(s) of dimension(s) over 
#'  which to compute statistics. If omitted c("ftime", "sdate", "time") are 
#'  searched in this order.
#'@param verbose Logical for verbose output
#'@return A list with elements \code{$cluster} (cluster assigned for each 
#'member), \code{$freq} (relative frequency of each cluster), 
#'\code{$closest_member} (representative member for each cluster), 
#'\code{$repr_field} (list of fields for each representative member), 
#'\code{composites} (list of mean fields for each cluster), \code{$lon} 
#'(selected longitudes of output fields), \code{$lat} (selected longitudes of 
#'output fields).
#'@examples
#'dat_exp <- array(abs(rnorm(1152))*275, dim = c(dataset = 1, member = 4, 
#'                                               sdate = 6, ftime = 3, 
#'                                               lat = 4, lon = 4))
#'lon <- seq(0, 3)
#'lat <- seq(48, 45)
#'coords <- list(lon = lon, lat = lat)
#'exp <- list(data = dat_exp, coords = coords)
#'attr(exp, 'class') <- 's2dv_cube'
#'res <- CST_EnsClustering(exp = exp, numclus = 3,
#'                         cluster_dim = c("sdate"))
#'
#'@export
CST_EnsClustering <- function(exp, time_moment = "mean", numclus = NULL,
                              lon_lim = NULL, lat_lim = NULL,
                              variance_explained = 80, numpcs = NULL, 
                              time_dim = NULL, time_percentile = 90, 
                              cluster_dim = "member", verbose = F) {

  # Check 's2dv_cube'
  if (!inherits(exp, "s2dv_cube")) {
    stop("Parameter 'exp' must be of the class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  # Check 'exp' object structure
  if (!all(c('data', 'coords') %in% names(exp))) {
    stop("Parameter 'exp' must have 'data' and 'coords' elements ",
         "within the 's2dv_cube' structure.")
  }
  # Check coordinates
  if (!any(names(exp$coords) %in% .KnownLonNames()) | 
      !any(names(exp$coords) %in% .KnownLatNames())) {
    stop("Spatial coordinate names do not match any of the names accepted by ",
         "the package. Latitudes accepted names: 'lat', 'lats', 'latitude',", 
         " 'y', 'j', 'nav_lat'. Longitudes accepted names: 'lon', 'lons',", 
         " 'longitude', 'x', 'i', 'nav_lon'.")
  }

  lon_name <- names(exp$coords)[[which(names(exp$coords) %in% .KnownLonNames())]]
  lat_name <- names(exp$coords)[[which(names(exp$coords) %in% .KnownLatNames())]]

  result <- EnsClustering(exp$data, 
                          lat = as.vector(exp$coords[[lat_name]]), 
                          lon = as.vector(exp$coords[[lon_name]]), 
                          time_moment = time_moment, numclus = numclus,
                          lon_lim = lon_lim, lat_lim = lat_lim,
                          variance_explained = variance_explained, 
                          numpcs = numpcs, time_percentile = time_percentile, 
                          time_dim = time_dim, cluster_dim = cluster_dim, 
                          verbose = verbose)

  return(result)
}
#'@rdname EnsClustering
#'@title Ensemble clustering
#'
#'@author Federico Fabiano - ISAC-CNR, \email{f.fabiano@isac.cnr.it}
#'@author Ignazio Giuntoli - ISAC-CNR, \email{i.giuntoli@isac.cnr.it}
#'@author Danila Volpi - ISAC-CNR, \email{d.volpi@isac.cnr.it}
#'@author Paolo Davini - ISAC-CNR, \email{p.davini@isac.cnr.it}
#'@author Jost von Hardenberg - ISAC-CNR, \email{j.vonhardenberg@isac.cnr.it}
#'
#'@description This function performs a clustering on members/starting dates
#'and returns a number of scenarios, with representative members for each of 
#'them. The clustering is performed in a reduced EOF space.
#'
#'@param data A matrix of dimensions 'dataset member sdate ftime lat lon' 
#'  containing the variables to be analysed. Latitudinal dimension accepted 
#'  names: 'lat', 'lats', 'latitude', 'y', 'j', 'nav_lat'. Longitudinal 
#'  dimension accepted names: 'lon', 'lons','longitude', 'x', 'i', 'nav_lon'.
#'@param lat Vector of latitudes.
#'@param lon Vector of longitudes.
#'@param time_moment Decides the moment to be applied to the time dimension. Can 
#'  be either 'mean' (time mean), 'sd' (standard deviation along time) or 'perc' 
#'  (a selected percentile on time). If 'perc' the keyword 'time_percentile' is 
#'  also used.
#'@param time_percentile Set the percentile in time you want to analyse (used 
#'  for `time_moment = "perc").
#'@param numclus Number of clusters (scenarios) to be calculated. If set to NULL
#'  the number of ensemble members divided by 10 is used, with a minimum of 2 
#'  and a maximum of 8.
#'@param lon_lim List with the two longitude margins in `c(-180,180)` format.
#'@param lat_lim List with the two latitude margins.
#'@param variance_explained variance (percentage) to be explained by the set of 
#'  EOFs. Defaults to 80. Not used if numpcs is specified.
#'@param numpcs Number of EOFs retained in the analysis (optional).
#'@param cluster_dim Dimension along which to cluster. Typically "member" or 
#'  "sdate". This can also be a list like c("member", "sdate").
#'@param time_dim String or character array with name(s) of dimension(s) over 
#'  which to compute statistics. If omitted c("ftime", "sdate", "time") are 
#'  searched in this order.
#'@param verbose Logical for verbose output
#'@return A list with elements \code{$cluster} (cluster assigned for each member),
#'\code{$freq} (relative frequency of each cluster), \code{$closest_member}
#'(representative member for each cluster), \code{$repr_field} (list of fields for
#'each representative member), \code{composites} (list of mean fields for each 
#'cluster), \code{$lon} (selected longitudes of output fields), \code{$lat} 
#'(selected longitudes of output fields).
#' 
#'@examples
#'exp <- array(abs(rnorm(1152))*275, dim = c(dataset = 1, member = 4, 
#'                                           sdate = 6, ftime = 3, 
#'                                           lat = 4, lon = 4))
#'lon <- seq(0, 3)
#'lat <- seq(48, 45)
#'res <- EnsClustering(exp, lat = lat, lon = lon, numclus = 2,
#'                     cluster_dim = c("member", "dataset", "sdate"))
#' 
#'@export
EnsClustering <- function(data, lat, lon, time_moment = "mean", numclus = NULL,
                          lon_lim = NULL, lat_lim = NULL, variance_explained = 80,
                          numpcs = NULL, time_percentile = 90, time_dim = NULL,
                          cluster_dim = "member", verbose = T) {

  # Know spatial coordinates names
  if (!any(names(dim(data)) %in% .KnownLonNames()) | 
      !any(names(dim(data)) %in% .KnownLatNames())) {
    stop("Spatial coordinate names do not match any of the names accepted by ",
         "the package.")
  }
  
  lon_name <- names(dim(data))[[which(names(dim(data)) %in% .KnownLonNames())]]
  lat_name <- names(dim(data))[[which(names(dim(data)) %in% .KnownLatNames())]]

  # Check/detect time_dim
  if (is.null(time_dim)) {
    time_dim_names <- c("ftime", "sdate", "time")
    time_dim_num <- which(time_dim_names %in% names(dim(data)))
    if (length(time_dim_num) > 0) {
      # Find time dimension with length > 1
      ilong <- which(dim(data)[time_dim_names[time_dim_num]] > 1)
      if (length(ilong) > 0) {
        time_dim <- time_dim_names[time_dim_num[ilong[1]]]
      } else {
        stop("No time dimension longer than one found.")
      }
    } else {
      stop("Could not automatically detect a target time dimension ",
           "in the provided data in 'data'.")
    }
    .printv(paste("Selected time dim:", time_dim), verbose)
  }

  # Apply time_moment
  if (time_moment == "mean") {
    .printv("Considering the time_moment: mean", verbose)
    exp <- Apply(data, target_dims = time_dim, mean)$output1
  } else if (time_moment == "sd") {
    .printv("Considering the time_moment: sd", verbose)
    exp <- Apply(data, target_dims = time_dim, sd)$output1
  } else if (time_moment == "perc") {
    .printv(paste0("Considering the time_moment: percentile ",
                 sprintf("%5f", time_percentile)), verbose)
    exp <- Apply(data, target_dims = time_dim,
                 function(x) {quantile(as.vector(x),
                                       time_percentile / 100.)})$output1
  } else {
    stop(paste0("Invalid time_moment '", time_moment, "' specified!"))
  }

  # Repeatedly apply .ensclus
  result <- Apply(exp, target_dims = c(cluster_dim, lat_name, lon_name), .ensclus,
                  lat, lon, numclus = numclus,
                  lon_lim = lon_lim, lat_lim = lat_lim,
                  variance_explained = variance_explained,
                  numpcs = numpcs, verbose = verbose)

  # Expand result$closest_member into indices in cluster_dim dimensions
  cm = result$closest_member
  cml <- vector(mode = "list", length = length(cluster_dim))
  cum <- cm * 0
  dim_cd <- dim(exp)[cluster_dim]
  for (i in rev(seq_along(cluster_dim))) {
    cml[[i]] <- floor((cm - cum - 1) / prod(dim_cd[-i])) + 1
    cum <- cum + (cml[[i]] - 1) * prod(dim_cd[-i])
    dim_cd <- dim_cd[-i]
  }
  names(cml) <- cluster_dim
  result$closest_member <- cml

  result[[lon_name]] <- lon
  result[[lat_name]] <- lat

  return(result)
}

# Atomic ensclus function
.ensclus <- function(var_ens, lat, lon, numclus = NULL, lon_lim = NULL,
                     lat_lim = NULL, variance_explained = 80, numpcs = NULL,
                     verbose = T) {
  # Check if more than one dimension has been passed for clustering
  sampledims <- NULL
  if (length(dim(var_ens)) > 3) {
    sampledims <- head(dim(var_ens), -2)
    dim(var_ens) <- c(samples = prod(sampledims),
                      tail(dim(var_ens), 2))
  }
  if (length(lat) != dim(var_ens)[2]) {
    stop("Incorrect lat length")
  }
  if (length(lon) != dim(var_ens)[3]) {
    stop("Incorrect lon length")
  }
  n_ens <- dim(var_ens)[1]
  if (is.null(numclus)) {
    numclus <- min(max(floor(n_ens / 10), 2), 8)
  }
  .printv(paste("Number of clusters:", numclus), verbose)

  .printv("Calculating ensemble anomalies...", verbose)
  ens_mean <- apply(var_ens, c(2, 3), mean)
  var_anom <- array(dim = dim(var_ens))
  for (k in seq(1, n_ens)) {
    var_anom[k, , ] <- var_ens[k, , ] - ens_mean
  }

  # reshaping to give the right input to regimes function
  var_anom <- aperm(var_anom, c(3, 2, 1))
  clusters <- .regimes(lon, lat, var_anom, ncluster = numclus, ntime = 1000,
                      neof = numpcs, lon_lim, lat_lim,
                      perc = variance_explained,
                      max_eofs = n_ens - 1, verbose = verbose)

  clus_labels <- as.array(clusters$cluster)
  names(dim(clus_labels))[1] <- names(dim(var_ens))[1]
  if (!is.null(sampledims)) {
    dim(clus_labels) <- c(sampledims, dim(clus_labels)[-1])
  }
  frequencies <- as.array(clusters$frequencies)
  names(dim(frequencies))[1] <- "cluster"

  clus_centers <- clusters$clus_centers

  closest_member <- array(dim = numclus)
  dist_closest_member <- array(dim = numclus)
  for (iclu in seq(1, numclus)) {
    this_clus_labels <- which(clus_labels == iclu)
    if (length(this_clus_labels) > 1) {
      dist_arr <- apply(clusters$pcs[clus_labels == iclu, ], 1,
                        .dist_from_center, center = clus_centers[iclu, ])
      .printv(paste0("distance from cluster ", iclu, " center:"), verbose)
      .printv(this_clus_labels, verbose)
      .printv(dist_arr, verbose)
      closest_member[iclu] <- this_clus_labels[which.min(dist_arr)]
      dist_closest_member[iclu] <- min(dist_arr)
      .printv(paste0("closest member to cluster ", iclu, " center is: ",
                   closest_member[iclu]), verbose)
    } else {
      .printv(paste0("distance from cluster ", iclu, " center:"), verbose)
      dista <- .dist_from_center(clusters$pcs[clus_labels == iclu, ],
                                center = clus_centers[iclu, ])
      .printv(this_clus_labels, verbose)
      .printv(dist_arr, verbose)
      closest_member[iclu] <- this_clus_labels
      dist_closest_member[iclu] <- dista
    }
  }
  .printv("EnsClustering completed...", verbose)

  names(dim(closest_member))[1] <- "cluster"
  repr_field <- var_ens[closest_member, , ]
  names(dim(repr_field))[2:3] <- names(dim(var_ens))[2:3]
  names(dim(repr_field))[1] <- "cluster"
  # Bring back to original order lat lon
  composites <- aperm(clusters$regimes, c(1, 3, 2))
  names(dim(composites))[2:3] <- names(dim(var_ens))[2:3]
  names(dim(composites))[1] <- "cluster"
  out <- list(cluster = clus_labels, freq = frequencies,
              closest_member = closest_member, repr_field = repr_field,
              composites = composites)
  return(out)
}

.dist_from_center <- function(y, center = NULL) {
  dist <- sqrt(sum((y - center)^2))
  return(dist)
}

.eofs <- function(lon, lat, field, neof = 4, xlim = NULL, ylim = NULL,
                  method = "SVD", do_standardize = F, do_regression = F,
                  verbose = T) {
  # R tool for computing EOFs based on
  # Singular Value Decomposition ("SVD", default)
  # or with the eigenvectors of the covariance matrix ("covariance", slower)
  # If requested, computes linear regressions and standardizes the PCs
  # If you want to use the regressions, remember to standardize the PCs
  # Take as input a 3D anomaly field.
  # Requires "personal" functions area.weight, standardize

  if (exists(".lm.fit")) {
    lin.fit <- .lm.fit
  } else {
    lin.fit <- lm.fit
  }

  # area weighting, based on the root of cosine
  .printv("Area Weighting...", verbose)
  ww <- .area.weight(lon, lat, root = T)
  wwfield <- sweep(field, c(1, 2), ww, "*")

  idx <- .selbox(lon, lat, xlim, ylim)
  slon <- lon[idx$ilon]
  slat <- lat[idx$ilat]
  wwfield <- wwfield[idx$ilon, idx$ilat, ]

  # transform 3D field in a matrix
  wwfield <- array(wwfield, dim = c(dim(wwfield)[1] * dim(wwfield)[2],
                                    dim(wwfield)[3]))

  # calling SVD
  if (method == "SVD") {
    .printv("Calling SVD...", verbose)
    SVD <- svd(wwfield, nu = neof, nv = neof)

    # extracting EOFs (loading pattern), expansions coefficient
    # and variance explained
    pattern <- array(SVD$u, dim = c(dim(box)[1], dim(box)[2], neof))
    coefficient <- SVD$v
    variance <- (SVD$d[1:neof])^2 / sum((SVD$d)^2)
    if (do_standardize) {
      coefficient <- apply(coefficient, c(2), .standardize)
    } else {
      coefficient <- sweep(coefficient, c(2), sqrt(variance), "*")
    }
  }

  # calling covariance matrix
  if (method == "covariance") {
    .printv("Calling eigenvectors of the covariance matrix...", verbose)
    covma <- cov(t(wwfield))
    eig <- eigen(covma)
    coef <- (t(wwfield) %*% eig$vector)[, 1:neof]
    pattern <- array(eig$vectors, dim = c(dim(box)[1], dim(box)[2],
                                          dim(box)[3]))[, , 1:neof]
    variance <- eig$values[1:neof] / sum(eig$values)
    if (do_standardize) {
      coefficient <- apply(coef, c(2), .standardize)
    } else {
      coefficient <- coef
    }
  }

  # linear regressions on anomalies
  regression <- NULL
  if (do_regression) {
    .printv("Linear Regressions (it can takes a while)... ", verbose)
    regression <- array(NA, dim = c(length(lon), length(lat), neof))
    for (i in 1:neof) {
      regression[, , i] <- apply(field, c(1, 2),
         function(x) lin.fit(as.matrix(coefficient[, i],
                                       ncol = 1), x)$coefficients)
    }
  }

  # preparing output
  .printv("Finalize eofs...", verbose)
  pattern <- list(x = slon, y = slat, z = pattern)
  out <- list(pattern = pattern, coeff = coefficient, variance = variance,
              regression = regression)
  return(out)
}


.regimes <- function(lon, lat, field, ncluster = 4, ntime = 1000, neof = 10,
                     xlim, ylim, alg = "Hartigan-Wong",
                     perc = NULL, max_eofs = 50, verbose = T) {
  # R tool to compute cluster analysis based on k-means.
  # Requires "personal" function eofs
  # Take as input a 3D anomaly field

  # Reduce the phase space with EOFs: use SVD and do not standardize PCs
  .printv("Launching EOFs...", verbose)
  t0 <- proc.time()
  if (is.null(neof)) {
    reducedspace <- .eofs(lon, lat, field, neof = max_eofs, xlim = xlim,
                          ylim = ylim, method = "SVD", do_regression = F,
                          do_standardize = F, verbose = verbose)
    neof <- which(cumsum(reducedspace$variance) > perc / 100.)[1]
    .printv(paste("Number of EOFs needed for var:", neof), verbose)
    PC <- reducedspace$coeff[,1:neof]
  } else {
    reducedspace <- .eofs(lon, lat, field, neof = neof, xlim = xlim,
                          ylim = ylim, method = "SVD", do_regression = F,
                          do_standardize = F, verbose = verbose)
    PC <- reducedspace$coeff
  }
  t1 <- proc.time() - t0

  # k-means computation repeat for ntime to find best solution.
  .printv("Computing k-means...", verbose)
  t0 <- proc.time()
  regimes <- kmeans(PC, as.numeric(ncluster), nstart = ntime,
                    iter.max = 1000, algorithm = alg)
  t1 <- proc.time() - t0

  # Extract regimes frequency and timeseries of occupation
  cluster <- regimes$cluster
  frequencies <- regimes$size / dim(field)[3] * 100
  .printv("Cluster frequencies:", verbose)
  .printv(frequencies[order(frequencies, decreasing = T)], verbose)

  .printv("Creating Composites...", verbose)
  compose <- apply(field, c(1, 2), by, cluster, mean)

  # sorting from the more frequent to the less frequent
  kk <- order(frequencies, decreasing = T)
  cluster <- cluster + 100
  for (ss in 1:ncluster) {
    cluster[cluster == (ss + 100)] <- which(kk == ss)
  }

  # prepare output
  .printv("Finalize regimes...", verbose)
  out <- list(cluster = cluster, frequencies = frequencies[kk],
              regimes = compose[kk, , ], pcs = PC,
              clus_centers = regimes$centers[kk, ],
              tot.withinss = regimes$tot.withinss)
  return(out)
}
