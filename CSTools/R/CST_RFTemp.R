#'@rdname CST_RFTemp
#'@title Temperature downscaling of a CSTools object using lapse rate
#'correction or a reference field
#'@author Jost von Hardenberg - ISAC-CNR, \email{j.vonhardenberg@isac.cnr.it}
#'@description This function implements a simple lapse rate correction of a
#'temperature field (an object of class 's2dv_cube' as provided by
#'`CST_Load`) as input.
#'The input lon grid must be increasing (but can be modulo 360).
#'The input lat grid can be irregularly spaced (e.g. a Gaussian grid)
#'The output grid can be irregularly spaced in lon and/or lat.
#'@references Method described in ERA4CS MEDSCOPE milestone M3.2: 
#'High-quality climate prediction data available to WP4 here: 
#'\url{https://www.medscope-project.eu/the-project/deliverables-reports/}
#'and in H2020 ECOPOTENTIAL Deliverable No. 8.1:
#'High resolution (1-10 km) climate, land use and ocean change scenarios available 
#'here: \url{https://ec.europa.eu/research/participants/documents/downloadPublic?documentIds=080166e5b6cd2324&appId=PPGMS}
#'@param data An object of the class 's2dv_cube' as returned by `CST_Load`,
#'  containing the temperature fields to downscale. The data object is expected 
#'  to have an element named \code{$data} with at least two spatial dimensions 
#'  named "lon" and "lat". (these default names can be changed with the 
#'  \code{lon_dim} and \code{lat_dim} parameters).
#'@param oro An object of the class 's2dv_cube' as returned by `CST_Load`,
#'  containing fine scale orography (in meters). The destination downscaling 
#'  area must be contained in the orography field.
#'@param xlim Vector with longitude bounds for downscaling; the full input
#'  field is downscaled if `xlim` and `ylim` are not specified.
#'@param ylim Vector with latitude bounds for downscaling
#'@param lapse Float with environmental lapse rate
#'@param lon_dim String with name of longitude dimension
#'@param lat_dim String with name of latitude dimension
#'@param time_dim A vector of character string indicating the name of temporal 
#'  dimension. By default, it is set to NULL and it considers "ftime", "sdate" 
#'  and "time" as temporal dimensions.
#'@param verbose Logical if to print diagnostic output.
#'@param nolapse Logical, if true `oro` is interpreted as a fine-scale
#'  climatology and used directly for bias correction.
#'@param compute_delta Logical if true returns only a delta to be used for
#'  out-of-sample forecasts. Returns an object of the class 's2dv_cube',
#'  containing a delta. Activates `nolapse = TRUE`.
#'@param delta An object of the class 's2dv_cube', containing a delta
#'  to be applied to the downscaled input data. Activates `nolapse = TRUE`.
#'  The grid of this object must coincide with that of the required output.
#'@param method String indicating the method used for interpolation:
#'  "nearest" (nearest neighbours followed by smoothing with a circular
#'  uniform weights kernel), "bilinear" (bilinear interpolation)
#'  The two methods provide similar results, but nearest is slightly better
#'  provided that the fine-scale grid is correctly centered as a subdivision
#'  of the large-scale grid.
#'@return CST_RFTemp() returns a downscaled CSTools object (i.e., of the class 
#''s2dv_cube').
#'@examples
#'# Generate simple synthetic data and downscale by factor 4
#'t <- rnorm(7 * 6 * 2 * 3 * 4)*10 + 273.15 + 10
#'dim(t) <- c(dataset = 1, member = 2, sdate = 3, ftime = 4, lat = 6, lon = 7)
#'lon <- seq(3, 9, 1)
#'lat <- seq(42, 47, 1)
#'coords <- list(lat = lat, lon = lon)
#'exp <- list(data = t, coords = coords)
#'attr(exp, 'class') <- 's2dv_cube'
#'o <- runif(29*29)*3000
#'dim(o) <- c(lats = 29, lons = 29)
#'lon <- seq(3, 10, 0.25)
#'lat <- seq(41, 48, 0.25)
#'coords <- list(lat = lat, lon = lon)
#'oro <- list(data = o, coords = coords)
#'attr(oro, 'class') <- 's2dv_cube'
#'res <- CST_RFTemp(data = exp, oro = oro, xlim = c(4,8), ylim = c(43, 46), 
#'                  lapse = 6.5, time_dim = 'ftime',
#'                  lon_dim = 'lon', lat_dim = 'lat')
#'@import multiApply
#'@export
CST_RFTemp <- function(data, oro, xlim = NULL, ylim = NULL, lapse = 6.5,
                       lon_dim = "lon", lat_dim = "lat", time_dim = NULL,
                       nolapse = FALSE, verbose = FALSE, compute_delta = FALSE,
                       method = "bilinear", delta = NULL) {
  # Check 's2dv_cube'
  if (!inherits(data, "s2dv_cube")) {
    stop("Parameter 'data' must be of the class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  if (!inherits(oro, "s2dv_cube")) {
    stop("Parameter 'oro' must be of the class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  if (!is.null(delta)) {
    if (!inherits(delta, "s2dv_cube")) {
      stop("Parameter 'delta' must be of the class 's2dv_cube', ",
           "as output by CSTools::CST_Load.")
    }
  }
  # Check 's2dv_cube' structure
  if (!all(c('data', 'coords') %in% names(data))) {
    stop("Parameter 'data' must have 'data' and 'coords' elements ",
         "within the 's2dv_cube' structure.")
  }
  if (!all(c('data', 'coords') %in% names(oro))) {
    stop("Parameter 'oro' must have 'data' and 'coords' elements ",
         "within the 's2dv_cube' structure.")
  }
  # Check coordinates
  if (!any(names(data$coords) %in% .KnownLonNames()) | 
      !any(names(data$coords) %in% .KnownLatNames())) {
    stop("Spatial coordinate names of 'data' do not match any of the names ",
         "accepted by the package.")
  }
  if (!any(names(oro$coords) %in% .KnownLonNames()) | 
      !any(names(oro$coords) %in% .KnownLatNames())) {
    stop("Spatial coordinate names of 'oro' do not match any of the names ",
         "accepted by the package.")
  }

  lon_data <- names(data$coords)[[which(names(data$coords) %in% .KnownLonNames())]]
  lat_data <- names(data$coords)[[which(names(data$coords) %in% .KnownLatNames())]]

  lon_oro <- names(oro$coords)[[which(names(oro$coords) %in% .KnownLonNames())]]
  lat_oro <- names(oro$coords)[[which(names(oro$coords) %in% .KnownLatNames())]]

  res <- RFTemp(data = data$data, 
                lon = as.vector(data$coords[[lon_data]]),
                lat = as.vector(data$coords[[lat_data]]),
                oro = oro$data, 
                lonoro = as.vector(oro$coords[[lon_oro]]), 
                latoro = as.vector(oro$coords[[lat_oro]]), 
                xlim = xlim, ylim = ylim, lapse = lapse,
                lon_dim = lon_dim, lat_dim = lat_dim, time_dim = time_dim,
                nolapse = nolapse, verbose = verbose, method = method,
                compute_delta = compute_delta, delta = delta$data)

  data$data <- res$data
  data$coords[[lon_data]] <- res$coords[[lon_dim]]
  data$coords[[lat_data]] <- res$coords[[lat_dim]]

  return(data)
}

#'@rdname RFTemp
#'@title Temperature downscaling of a CSTools object using lapse rate
#'correction (reduced version)
#'@author Jost von Hardenberg - ISAC-CNR, \email{j.vonhardenberg@isac.cnr.it}
#'@description This function implements a simple lapse rate correction of a
#'temperature field (a multidimensional array) as input.
#'The input lon grid must be increasing (but can be modulo 360).
#'The input lat grid can be irregularly spaced (e.g. a Gaussian grid)
#'The output grid can be irregularly spaced in lon and/or lat.
#'@references Method described in ERA4CS MEDSCOPE milestone M3.2: 
#'High-quality climate prediction data available to WP4 here:
#'\url{https://www.medscope-project.eu/the-project/deliverables-reports/}
#'and in H2020 ECOPOTENTIAL Deliverable No. 8.1:
#'High resolution (1-10 km) climate, land use and ocean change scenarios here: 
#'\url{https://ec.europa.eu/research/participants/documents/downloadPublic?documentIds=080166e5b6cd2324&appId=PPGMS}.
#'@param data Temperature array to downscale. The input array is expected to 
#'  have at least two dimensions named "lon" and "lat" by default (these default 
#'  names can be changed with the \code{lon_dim} and \code{lat_dim} parameters).
#'@param lon Vector or array of longitudes.
#'@param lat Vector or array of latitudes.
#'@param lonoro Vector or array of longitudes corresponding to the fine orography.
#'@param latoro Vector or array of latitudes corresponding to the fine orography.
#'@param oro Array containing fine-scale orography (in m). The destination 
#'  downscaling area must be contained in the orography field.
#'@param xlim Vector with longitude bounds for downscaling; the full input field 
#'  is downscaled if `xlim` and `ylim` are not specified.
#'@param ylim Vector with latitude bounds for downscaling.
#'@param lapse Float with environmental lapse rate.
#'@param lon_dim String with name of longitude dimension.
#'@param lat_dim String with name of latitude dimension.
#'@param time_dim A vector of character string indicating the name of temporal 
#'  dimension. By default, it is set to NULL and it considers "ftime", "sdate" 
#'  and "time" as temporal dimensions.
#'@param verbose Logical if to print diagnostic output.
#'@param nolapse Logical, if true `oro` is interpreted as a fine-scale 
#'  climatology and used directly for bias correction.
#'@param compute_delta Logical if true returns only a delta to be used for
#'  out-of-sample forecasts.
#'@param delta Matrix containing a delta to be applied to the downscaled
#'  input data. The grid of this matrix is supposed to be same as that of
#'  the required output field.
#'@param method String indicating the method used for interpolation:
#'  "nearest" (nearest neighbours followed by smoothing with a circular
#'  uniform weights kernel), "bilinear" (bilinear interpolation)
#'  The two methods provide similar results, but nearest is slightly better
#'  provided that the fine-scale grid is correctly centered as a subdivision
#'  of the large-scale grid.
#'@return CST_RFTemp() returns a downscaled CSTools object.
#'@return RFTemp() returns a list containing the fine-scale
#'longitudes, latitudes and the downscaled fields.
#'@examples
#'# Generate simple synthetic data and downscale by factor 4
#'t <- rnorm(7 * 6 * 4 * 3) * 10 + 273.15 + 10
#'dim(t) <- c(sdate = 3, ftime = 4, lat = 6, lon = 7)
#'lon <- seq(3, 9, 1)
#'lat <- seq(42, 47, 1)
#'o <- runif(29 * 29) * 3000
#'dim(o) <- c(lat = 29, lon = 29)
#'lono <- seq(3, 10, 0.25)
#'lato <- seq(41, 48, 0.25)
#'res <- RFTemp(t, lon, lat, o, lono, lato, xlim = c(4, 8), ylim = c(43, 46),
#'              lapse = 6.5, time_dim = 'ftime')
#'@import multiApply
#'@export
RFTemp <- function(data, lon, lat, oro, lonoro, latoro,
                   xlim = NULL, ylim = NULL, lapse = 6.5,
                   lon_dim = "lon", lat_dim = "lat", time_dim = NULL,
                   nolapse = FALSE, verbose = FALSE, compute_delta = FALSE,
                   method = "bilinear", delta = NULL) {
  # Check 'lon_dim' and 'lat_dim' parameters
  if (!all(c(lon_dim, lat_dim) %in% names(dim(data)))) {
    stop("Parameters 'lon_dim' and 'lat_dim' do not match with 'data' ", 
         "dimension names.")
  }

  # Check/detect time_dim
  if (is.null(time_dim)) {
    time_dim_names <- c("ftime", "sdate", "time")
    time_dim_num <- which(time_dim_names %in% names(dim(data)))
    if (length(time_dim_num) > 0) {
      # Find time dimension with length > 0
      ilong <- which(dim(data)[time_dim_names[time_dim_num]] > 0)
      if (length(ilong) > 0) {
        time_dim <- time_dim_names[time_dim_num[ilong]]
      } else {
        stop("No time dimension longer than zero found.")
      }
    } else {
      stop("Could not automatically detect a target time dimension ",
           "in the provided data in 'data'.")
    }
    warning(paste("Selected time dim:", time_dim, "\n"))
  }

  # Repeatedly apply .downscalet
  if (is.null(delta)) {
      result <- Apply(data, target_dims = c(lon_dim, lat_dim, time_dim),
                      fun = .downscalet, lon, lat, oro, lonoro, latoro,
                      xlim = xlim, ylim = ylim, lapse = lapse,
                      nolapse = nolapse, verbose = verbose, method = method,
                      compute_delta = compute_delta)
  } else {
      result <- Apply(list(data, delta),
                      target_dims = list(c(lon_dim, lat_dim, time_dim),
                                         c(lon_dim, lat_dim)),
                      fun = .downscalet_delta, lon, lat, oro, lonoro, latoro,
                      xlim = xlim, ylim = ylim, lapse = lapse,
                      nolapse = nolapse, verbose = verbose, method = method,
                      compute_delta = compute_delta)
  }

  names(dim(result$data))[1] <- names(dim(data))[names(dim(data)) == lon_dim]
  names(dim(result$data))[2] <- names(dim(data))[names(dim(data)) == lat_dim]
  result$lon <- array(result$lon[1:dim(result$lon)[1]])
  result$lat <- array(result$lat[1:dim(result$lat)[1]])
  names(dim(result$lon)) <- lon_dim
  names(dim(result$lat)) <- lat_dim

  names(result) <- c('data', lon_dim, lat_dim)

  return(result)
}

#'Lapse-rate temperature correction downscaling
#'
#'@description Downscales a temperature field using a lapse-rate
#'correction based on a reference orography. Time-averaging is done on all
#'dimensions after the first two.
#'@author Jost von Hardenberg, \email{j.vonhardenberg@isac.cnr.it}
#'@param lon Vector of input longitudes.
#'@param lat Vector of input latitudes.
#'@param t Matrix of input temperature data.
#'@param lono Vector of orography longitudes.
#'@param lato Vector of orography latitudes.
#'@param oro Matrix of topographical elevations (in meters). The destination 
#'  downscaling area must be contained in the orography field.
#'@param xlim Vector of longitude bounds; the full input field is downscaled if 
#'  `xlim` and `ylim` are not specified.
#'@param ylim Vector of latitude bounds.
#'@param radius Smoothing radius expressed in longitude units (default is half a 
#'  large-scale pixel).
#'@param lapse Environmental lapse rate (in K/Km).
#'@param nolapse Logical, if true `oro` is interpreted as a fine-scale
#'  climatology and used directly for bias correction.
#'@param compute_delta Logical if true returns only a delta to be used for
#'  out-of-sample forecasts.
#'@param delta Matrix containing a delta to be applied to the input data.
#'  The grid of this matrix is supposed to be same as that of the required 
#'  output field.
#'@param verbose Logical if to print diagnostic output.
#'@return A downscaled temperature matrix.
#'@examples
#'lon = 5:20
#'lat = 35:40
#'t = runif(16 * 6); dim(t) = c(16, 6)
#'lono = seq(5, 20, 0.1)
#'lato = seq(35, 40, 0.1)
#'o = runif(151 * 51) * 2000; dim(o) = c(151, 51)
#'td = .downscalet(t, lon, lat, o, lono, lato, c(8, 12), c(36, 38))
#'@noRd
.downscalet <- function(t, lon, lat, oro, lono, lato,
                        xlim = NULL, ylim = NULL,
                        radius = 0, lapse = 6.5, nolapse = FALSE,
                        verbose = FALSE, compute_delta = FALSE,
                        method = "bilinear", delta = NULL) {

    if (!is.null(delta) & compute_delta) {
        stop("Cannot `compute_delta` and provide `delta` at the same time.")
    }

    tdim <- dim(t)
    ldim <- FALSE
    if (length(tdim) == 3) {
        nt <- tdim[3]
    } else if (length(tdim) == 2) {
        nt <- 1
        dim(t) <- c(tdim, 1)
    } else if  (length(tdim) < 2) {
        stop("Input array must have at least two dimensions")
    } else {
        ldim <- TRUE
        dim(t) <- c(tdim[1:2], time = prod(tdim[-(1:2)]))
        nt <- dim(t)[3]
    }
    if (lon[2] <= lon[1]) {
        stop("Longitudes must be monotone increasing.")
    }
    # Regularize lon coords to monotone increasing
    lon[lon >= lon[1]] <- lon[lon >= lon[1]] - 360
    if (lon[length(lon)] < 0) { lon <- lon + 360 }
    lono[lono >= lono[1]] <- lono[lono >= lono[1]] - 360
    if (lono[length(lono)] < 0) { lono <- lono + 360 }

    dxl <- (lon[2] - lon[1]) / 2
    dyl <- (lat[2] - lat[1]) / 2
    if (radius == 0) {
        radius <- dxl
    }
    if (is.null(xlim)) {
        xlim <- c(lon[1] + dxl, lon[length(lon)] - dxl)
    }
    if (is.null(ylim)) {
        ylim <- c(lat[1] + dyl, lat[length(lat)] - dyl)
        if (ylim[1] > ylim[2]) {
           ylim <- ylim[2:1]
        }
    }
    #Add buffer
    lon1 <- xlim[1] - radius
    lon2 <- xlim[2] + radius
    lat1 <- ylim[1] - radius
    lat2 <- ylim[2] + radius

    orocut <- oro[(lono <= lon2) & (lono >= lon1),
                  (lato <= lat2) & (lato >= lat1)]
    lonocut <- lono[(lono <= lon2) & (lono >= lon1)]
    latocut <- lato[(lato <= lat2) & (lato >= lat1)]

    dxol <- lonocut[2] - lonocut[1]
    nrad <- as.integer(radius / abs(dxol) + 0.5)

    if (length(lonocut) == 0 | length(latocut) == 0) {
        stop("Orography not available for selected area")
    }

    if (is.null(delta) & compute_delta & nolapse) {
        if (verbose) {
            print("Time averaging")
        }
        # If we just need the delta we can work with time averages
        t <- apply(t, c(1, 2), mean)
        dim(t) <- c(dim(t), time = 1)
    }

    if (!(is.null(delta) & compute_delta & !nolapse)) {
        # This calculation is not needed if we just need the delta
        # and lapse rate is used
        if (verbose) {
            print(paste("Interpolation using", method, "method"))
        }
        tout <- .interp2d(t, lon, lat, lonocut, latocut, method = method)
        # Fine-scale smooth interpolated input field
        if (method == "nearest") {
            if (verbose) {
                print(paste("Smoothing interpolated field"))
            }
            tout <-  .smooth(tout, nrad)
        }
    }
    if (is.null(delta)) {
        # Compute delta
        if (nolapse) {
            # oro is a reference fine-scale field: use that directly
            # for bias correcting the interpolated input field's temporal mean
            t1 <- orocut - apply(tout, c(1, 2), mean)
        } else {
            # Lapse-rate correction
            orocuts <- .smooth(orocut, nrad)
            t1 <- -(orocut - orocuts) * lapse / 1000.
        }
        if (compute_delta) {
            # Return delta
            tout <- t1[(lonocut <= xlim[2]) & (lonocut >= xlim[1]),
                       (latocut <= ylim[2]) & (latocut >= ylim[1])]
        } else {
            # Apply delta
            if (verbose) {
                print("Adding computed delta")
            }
            for (i in seq_len(nt)) {
                tout[, , i] <- tout[, , i] + t1
            }
            tout <- tout[(lonocut <= xlim[2]) & (lonocut >= xlim[1]),
                         (latocut <= ylim[2]) & (latocut >= ylim[1]), ]
            if (ldim) {
                dim(tout) <- c(dim(tout)[1:2], tdim[-(1:2)])
            }
        }
    } else {
        # Just apply the provided delta
        tout <- tout[(lonocut <= xlim[2]) & (lonocut >= xlim[1]),
                     (latocut <= ylim[2]) & (latocut >= ylim[1]), ]
        if (any(dim(delta)[1:2] != dim(tout)[1:2])) {
            stop("Provided delta has not the same dimensions as required output")
        }
        if (verbose) {
            print("Adding provided delta")
        }
        for (i in seq_len(nt)) {
            tout[, , i] <- tout[, , i] + delta
        }
        if (ldim) {
            dim(tout) <- c(dim(tout)[1:2], tdim[-(1:2)])
        }
    }
    lonocut <- lonocut[(lonocut <= xlim[2]) & (lonocut >= xlim[1])]
    latocut <- latocut[(latocut <= ylim[2]) & (latocut >= ylim[1])]

    return(list(data = tout, lon = lonocut, lat = latocut))
}

# Special driver version of .downscalet to apply delta
.downscalet_delta <- function(t, delta, lon, lat, oro, lono, lato,
                        xlim = NULL, ylim = NULL, radius = 0,
                        lapse = 6.5, nolapse = FALSE, verbose = FALSE,
                        compute_delta = FALSE, method = "bilinear") {
    res <- .downscalet(t, lon, lat, oro, lono, lato, xlim = xlim,
                       ylim = ylim, radius = radius, lapse = lapse,
                       nolapse = nolapse, verbose = verbose,
                       compute_delta = compute_delta, delta = delta,
                       method = method)
}

#'Nearest neighbour interpolation
#'
#'@description The input field is interpolated onto the output
#'coordinate grid using nearest neighbours or bilinear interpolation.
#'The input lon grid must be monotone increasing. 
#'The input lat grid can be irregularly spaced (e.g. a Gaussian grid)
#'The output grid can be irregularly spaced in lon and/or lat.
#'@author Jost von Hardenberg, \email{j.vonhardenberg@isac.cnr.it}
#'@param z Matrix with the input field to interpolate (assumed to
#'  include also a third time dimension)
#'@param lon Vector of input longitudes.
#'@param lat Vector of input latitudes.
#'@param lonp Vector of output longitudes.
#'@param latp Vector of output latitudes.
#'@param method String indicating the interpolation method ("nearest" or 
#'  "bilinear" (default)).
#'@return The interpolated field.
#'@examples
#'lon = 5:11
#'lat = 35:40
#'z = runif(7 * 6 * 2); dim(z) = c(7, 6, 2)
#'lonp = seq(5, 10, 0.2)
#'latp = seq(35, 40, 0.2)
#'zo <- .interp2d(z, lon, lat, lonp, latp, method = "nearest")
#'@noRd
.interp2d <- function(z, lon, lat, lonp, latp, method="bilinear") {

    nx <- length(lonp)
    ny <- length(latp)
    nt <- dim(z)[3]
    # Interpolate nn assuming a regular grid
    zo <- array(0., c(nx, ny, nt))
    dy <- lat[2] - lat[1]
    dx <- lon[2] - lon[1]

    if (method == "nearest") {
        jj <- 1:length(latp)
        for (j in 1:length(latp)) {
            jj[j] <- which.min(abs(latp[j]-lat))
        }
        ii <- ((lonp - (lon[1] - dx / 2)) %/% dx + 1)
        # If lon are global and periodic attempt to fix issues
        if ((lon[1] - lon[length(lon)]) %% 360 == dx) { 
            ii[ii <= 0] <- ii[ii <= 0] + length(lon)
            ii[ii > length(lon)] <- ii[ii > length(lon)] - length(lon)
        }
        if ((ii[1] <= 0) | (ii[length(ii)] > length(lon)) |
           (jj[1] <= 0) | (jj[length(jj)] > length(lat))) {
               stop("Downscaling area not contained in input data")
        }
        zo[, , ] <- z[ii, jj, ]
    } else {
        jj <- 1:length(latp)
        jj2 <- jj
        for (j in 1:length(latp)) {
            jmin <- which.min(abs(latp[j]-lat))
            if ( (((latp[j]-lat[jmin]) >= 0) & ( dy >= 0)) |
                 (((latp[j]-lat[jmin]) < 0) & ( dy <= 0))) {
                jj[j] <- jmin
                jj2[j] <- jmin + 1
            } else {
                jj2[j] <- jmin
                jj[j] <- jmin - 1
            }
        }
        ii <- ((lonp - lon[1]) %/% dx + 1)
        ii2 <- ii + 1
        # If lon are global and periodic attempt to fix issues
        if ((lon[1] - lon[length(lon)]) %% 360 == dx) { 
            ii[ii <= 0] <- ii[ii <= 0] + length(lon)
            ii[ii > length(lon)] <- ii[ii > length(lon)] - length(lon)
            ii2[ii2 <= 0] <- ii2[ii2 <= 0] + length(lon)
            ii2[ii2 > length(lon)] <- ii2[ii2 > length(lon)] - length(lon)
        }
        if ((ii[1] <= 0) | (ii[length(ii)] > length(lon)) |
            (jj[1] <= 0) | (jj[length(jj)] > length(lat)) |
            (ii2[1] <= 0) | (ii2[length(ii2)] > length(lon)) |
            (jj2[1] <= 0) | (jj2[length(jj2)] > length(lat))) {
                stop("Downscaling area not contained in input data")
        }
        xx <- ((lonp - lon[ii]) / dx) %% 360
        yy <- (latp - lat[jj]) / (lat[jj2] - lat[jj])
        xx <- xx[row(zo[, , 1])]
        yy <- yy[col(zo[, , 1])]
        dim(xx) <- c(nx, ny)
        dim(yy) <- c(nx, ny)
        w00 <- (1 - xx) * (1 - yy)
        w10 <- xx * (1 - yy)
        w01 <- (1 - xx) * yy
        w11 <- xx * yy
        for (k in seq_len(nt)) {
            zo[, , k] <- z[ii, jj, k] * w00 + z[ii2, jj, k] * w10 +
                         z[ii, jj2, k] * w01 + z[ii2, jj2, k] * w11
        }
    }
    names(dim(zo)) <- names(dim(z))
    return(zo)
}

#'Smoothening using convolution with a circular kernel
#'
#'@description The input field is convolved with a circular kernel with equal
#'weights. Takes into account missing values.
#'@author Jost von Hardenberg, \email{j.vonhardenberg@isac.cnr.it}
#'@param z Matrix with the input field to smoothen, with dimensions `c(ns, ns)`
#'@param sdim The smoothing kernel radius in pixel.
#'@return The smoothened field.
#'@examples
#'z <- rnorm(64 * 64)
#'dim(z) <- c(64, 64)
#'zs <- smooth(z, 8)
#'sd(zs)
#'# [1] 0.1334648
#'@noRd
.smooth <- function(z, sdim) {
  nsx <- dim(z)[1]
  nsy <- dim(z)[2]
  zdim <- dim(z)
  if (length(dim(z)) == 2) {
      dim(z) <- c(dim(z), time = 1)
      nt <- 1
  } else {
      nt <- dim(z)[3]
  }
  imask <- !is.finite(z)
  z[imask] <- 0.
  mask <- matrix(0., nsx, nsy)
  for (i in 1:nsx) {
    for (j in 1:nsy) {
      kx <- i - 1
      ky <- j - 1
      if (i > nsx / 2 + 1) {
        kx <- i - nsx - 1
      }
      if (j > nsy / 2 + 1) {
        ky <- j - nsy - 1
      }
      r2 <- kx * kx + ky * ky
      mask[i, j] <- (r2 <= (sdim * sdim))
    }
  }
  fm <- fft(mask)
  zf <- array(0., c(nsx, nsy, nt))
  for (k in seq_len(nt)) {
      zf[, , k] <- Re(fft(fm * fft(z[, , k]), inverse = TRUE)
                    ) / sum(mask) / length(fm)
      if (sum(imask[, , k]) > 0) {
          zz <- z[, , k]
          zz[!imask[, , k]] <- 1.0
          zz <- zf[, , k] / (Re(fft(fm * fft(zz), inverse = TRUE)) /
                       sum(mask) / length(fm))
          zz[imask[, , k]] <- NA
          zf[, , k] <- zz
      }
  }
  dim(zf) <- zdim
  return(zf)
}
