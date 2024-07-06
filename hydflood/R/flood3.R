#' @name flood3
#' @rdname flood3
#' 
#' @title Function to compute flood extent or flood duration \code{SpatRaster}
#'   along the German federal waterways Elbe and Rhine using the 1d water level
#'   algorithms \code{hyd1d::waterLevel()} and \code{hyd1d::waterLevelPegelonline()}
#' 
#' @description Computes flood extent, if \code{length(seq)} equals 1, or flood 
#'   duration for the active floodplains along the German federal waterways Elbe 
#'   and Rhine based on 1d water levels computed by
#'   \code{\link[hyd1d]{waterLevel}} or
#'   \code{\link[hyd1d]{waterLevelPegelonline}} provided by package \pkg{hyd1d}.
#' 
#' @param x has to by type \code{SpatRaster} and has to include both input 
#'   raster layers \code{csa} (cross section areas) and \code{dem} (digital 
#'   elevation model). To compute water levels along the River Elbe \code{x} 
#'   has to be in the coordinate reference system 
#'   \href{https://spatialreference.org/ref/epsg/etrs89-utm-zone-33n/}{ETRS 1989 UTM 33N},
#'   for River Rhine in 
#'   \href{https://spatialreference.org/ref/epsg/etrs89-utm-zone-32n/}{ETRS 1989 UTM 32N}.
#'   Other coordinate reference systems are not permitted.
#' @param seq has to be type \code{c("POSIXct", "POSIXt")} or \code{Date} and 
#'   have a length larger than 0. If \code{seq} is type \code{c("POSIXct", "POSIXt")}, 
#'   values must be in the temporal range between 31 days ago (\code{Sys.time() 
#'   - 2678400}) and now (\code{Sys.time()}). Then 
#'   \code{\link[hyd1d]{waterLevelPegelonline}} is used internally for the water
#'   level computations. If \code{seq} is type \code{Date}, values must be in the
#'   temporal range between 1960-01-01 and yesterday (\code{Sys.Date() - 1}) 
#'   and \code{\link[hyd1d]{waterLevel}} is used internally.
#' @param filename supplies an optional output filename and has to be type 
#'   \code{character}.
#' @param \dots additional arguments as for \code{\link[terra]{writeRaster}}.
#' 
#' @return \code{SpatRaster} object with flood duration in the range of 
#'   \code{[0, length(seq)]}.
#' 
#' @details For every time step provided in \code{seq}, \code{flood3()} computes 
#'   a 1d water level along the requested river section. This 1d water level is 
#'   transfered to a \code{wl} (water level) raster layer, which is in fact a 
#'   copy of the \code{csa} (cross section areas) layer, and then 
#'   compared to the \code{dem} (digital elevation model) layer. Where the 
#'   \code{wl} layer is higher than the \code{dem}, layer flood duration is
#'   increased by 1.
#' 
#' @seealso \code{\link[hyd1d]{waterLevel}},
#'   \code{\link[hyd1d]{waterLevelPegelonline}},
#'   \code{\link[terra]{writeRaster}}, 
#'   \code{\link[terra]{terraOptions}}
#' 
#' @references 
#'   \insertRef{weber_flood3data_2022}{hydflood}
#'   
#'   \insertRef{weber_flood3data_2023}{hydflood}
#' 
#' @examples \donttest{
#'   options("hydflood.datadir" = tempdir())
#'   library(hydflood)
#'   
#'   # import the raster data and create a raster stack
#'   c <- st_crs("EPSG:25833")
#'   e <- ext(309000, 310000, 5749000, 5750000)
#'   x <- hydSpatRaster(ext = e, crs = c)
#'   
#'   # create a temporal sequence
#'   seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")
#'   
#'   # compute a flood duration
#'   fd <- flood3(x = x, seq = seq)
#' }
#' 
#' @export
#' 
flood3 <- function(x, seq, filename = '', ...) {
    
    options("rgdal_show_exportToProj4_warnings" =  "none")
    
    #####
    # check requirements
    ##
    # vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    ## x
    if (missing(x)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'x' ",
                                   "argument has to be supplied."))
    } else {
        # class
        if (!inherits(x, "SpatRaster")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'x' must be ",
                                       "type 'SpatRaster'."))
        }
        
        if (!all(c("dem", "csa") %in% names(x))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'names(x)' must",
                                       " be 'dem' and 'csa'."))
        }
        
    }
    
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    }
    
    # crs
    if (! isUTM32(x) & !isUTM33(x)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The projection",
                                   " of x must be either 'ETRS 1989 UTM 32",
                                   "N' or 'ETRS 1989 UTM 33N'."))
    } else {
        if (isUTM32(x)) {
            river <- "Rhine"
        } else if (isUTM33(x)) {
            river <- "Elbe"
        } else {
            stop(errors)
        }
    }
    
    # check position
    sf.ext <- rasterextent2polygon(x)
    if (exists("river")) {
        af <- sf.af(name = river)
        if (! (nrow(af[sf.ext,]) > 0)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The selected 'e",
                                       "xt' does NOT overlap with the active f",
                                       "loodplain of River ", river, "."))
        }
    }
    
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    }
    
    ## seq
    if (missing(seq)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'seq' ",
                                   "argument has to be supplied."))
    } else {
        # class
        if (!inherits(seq, "Date") &
            !all(c(inherits(seq, "POSIXct"), inherits(seq, "POSIXt")))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'seq' must be",
                                       " either type 'Date' or c('POSIXct', 'P",
                                       "OSIXt')."))
        }
        # length
        if (length(seq) < 1L) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'seq' must ha",
                                       "ve length larger 0."))
        }
        # NA and possible range
        if (any(is.na(seq))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'seq' or elem",
                                       "ents of it must not be NA."))
        } else {
            time_min <- trunc(Sys.time() - as.difftime(31, units = "days"),
                              units = "days")
            if (all(c(inherits(seq, "POSIXct"), inherits(seq, "POSIXt")))) {
                if (any(seq < time_min)) {
                    errors <- c(errors, paste0("Error ", l(errors), ": Values ",
                                               "of 'seq' must be between ",
                                               format(time_min, "%Y-%m-%d"),
                                               " 00:00:00 and now, if type of ",
                                               "'seq' is c('POSIXct', 'POSIXt'",
                                               ")."))
                }
                type_date <- FALSE
            }
            if (inherits(seq, "Date")) {
                if (any(seq < as.Date("1960-01-01")) |
                    any(seq > Sys.Date() - 1)) {
                    errors <- c(errors, paste0("Error ", l(errors), ": Val",
                                               "ues of 'seq' must be betwe",
                                               "en 1960-01-01 and yesterda",
                                               "y."))
                }
                type_date <- TRUE
            }
        }
    }
    
    ## filename
    if (! missing(filename)) {
        if (!inherits(filename, "character")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'filename' must",
                                       " be type 'character'."))
        }
        if (length(filename) != 1) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'filename' must",
                                       " have length 1."))
        }
    }
    
    #####
    # error messages
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    }
    
    #####
    # preprocessing
    #####
    # individual raster needed for the processing
    csa <- raster::raster(x$csa)
    dem <- raster::raster(x$dem)
    
    # out template
    out <- raster::raster(csa)
    
    # describe out's data attributes
    attributes <- out@data@attributes
    attributes <- append(attributes, paste0("flood duration computed by hydflo",
                                            "od::flood3() for the following t",
                                            "emporal sequence of type '", 
                                            class(seq), "' with length ",
                                            length(seq), ":"))
    attributes <- append(attributes, seq)
    out@data@attributes <- attributes
    
    # water level template
    waterlevel <- raster::raster(dem)
    
    # initialize the WaterLevelDataFrame
    station_int <- na.omit(as.integer(terra::unique(x$csa)$csa))
    wldf_initial <- hyd1d::WaterLevelDataFrame(river = river,
                                               time = as.POSIXct(NA),
                                               station_int = station_int)
    
    # check memory requirements
    big <- ! raster::canProcessInMemory(out, 4)
    filename <- raster::trim(filename)
    if (big & filename == '') {
        filename <- raster::rasterTmpFile()
    }
    if (filename != '') {
        out <- terra::writeStart(out, filename, ...)
        todisk <- TRUE
    } else {
        vv <- matrix(ncol = nrow(out), nrow = ncol(out))
        todisk <- FALSE
    }
    
    #####
    # processing
    ## 
    # compute all water levels through a loop over all time steps
    wldfs <- vector(mode = "list", length = length(seq))
    j <- 1
    for (i in seq) {
        if (type_date) {
            time <- as.POSIXct(format(as.Date(i, as.Date("1970-01-01")), 
                                      "%Y-%m-%d"), tz = "CET")
            wldf <- wldf_initial
            setTime(wldf) <- time
            wldfs[[j]] <- hyd1d::waterLevel(wldf)
        } else {
            time <- as.POSIXct(i, origin = "1970-01-01 00:00:00")
            wldf <- wldf_initial
            setTime(wldf) <- time
            wldfs[[j]] <- hyd1d::waterLevelPegelonline(wldf)
        }
        j <- j + 1
    }
    
    ##
    # raster processing
    bs <- raster::blockSize(csa)
    pb <- raster::pbCreate(bs$n, ...)
    
    if (todisk) {
        for (i in 1:bs$n) {
            # vectorize cross section areas (integer)
            v_csa <- raster::getValues(csa, row = bs$row[i], nrows = bs$nrows[i])
            # get unique stations for the csa subset
            v_stations <- stats::na.omit(unique(v_csa))
            # copy v_csa to v_fd to create a template results vector with the 
            # same size and type
            v_fd <- rep(0, length(v_csa))
            
            # vectorize digital elevation model (numeric)
            v_dem <- raster::getValues(dem, row = bs$row[i], nrows = bs$nrows[i])
            # copy v_dem to v_fwl to create a template vector with the same 
            # size and type
            v_wl <- rep(-999, length(v_csa))
            
            # handle NA's
            id_na <- is.na(v_csa) | is.na(v_dem) 
            id_nona <- !id_na
            
            # loop over all time steps
            for (j in 1:length(seq)) {
                # transfer the water level info to v_wl
                for (a_station in v_stations) {
                    v_wl[v_csa == a_station] <- 
                               wldfs[[j]]$w[wldfs[[j]]$station_int == a_station]
                }
                
                # compare the water level raster to the dem
                v_fd[id_nona][v_dem[id_nona] < v_wl[id_nona]] <- 
                    v_fd[id_nona][v_dem[id_nona] < v_wl[id_nona]] + 1
            }
            
            # transfer NA's
            v_fd[id_na] <- NA
            
            # write the resulting flood durations into out
            out <- terra::writeValues(out, v_fd, start = bs$row[i],
                                      nrows = bs$nrows[i])
            raster::pbStep(pb, i)
        }
        out <- terra::writeStop(out)
    } else {
        for (i in 1:bs$n) {
            # vectorize cross section areas (integer)
            v_csa <- raster::getValues(csa, row = bs$row[i], nrows = bs$nrows[i])
            # get unique stations for the csa subset
            v_stations <- stats::na.omit(unique(v_csa))
            # copy v_csa to v_fd to create a template results vector with the 
            # same size and type
            v_fd <- rep(0, length(v_csa))
            
            # vectorize digital elevation model (numeric)
            v_dem <- raster::getValues(dem, row = bs$row[i], nrows = bs$nrows[i])
            # copy v_dem to v_fwl to create a template vector with the same 
            # size and type
            v_wl <- rep(-999, length(v_csa))
            
            # handle NA's
            id_na <- is.na(v_csa) | is.na(v_dem) 
            id_nona <- !id_na
            
            # loop over all time steps
            for (j in 1:length(seq)) {
                # transfer the water level info to v_wl
                for (a_station in v_stations) {
                    v_wl[v_csa == a_station] <- 
                        wldfs[[j]]$w[wldfs[[j]]$station_int == a_station]
                }
                
                # compare the water level raster to the dem
                v_fd[id_nona][v_dem[id_nona] < v_wl[id_nona]] <- 
                    v_fd[id_nona][v_dem[id_nona] < v_wl[id_nona]] + 1
            }
            
            # transfer NA's
            v_fd[id_na] <- NA
            
            cols <- bs$row[i]:(bs$row[i] + bs$nrows[i]-1)
            vv[,cols] <- matrix(v_fd, nrow = out@ncols)
            raster::pbStep(pb, i)
        }
        out <- raster::setValues(out, as.vector(vv))
    }
    raster::pbClose(pb)
    return(terra::rast(out))
}

