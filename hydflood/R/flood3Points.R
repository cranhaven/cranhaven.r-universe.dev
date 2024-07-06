#' @name flood3Points
#' @rdname flood3Points
#' 
#' @title Function to compute flood duration for point coordinates along the
#'   German federal waterways Elbe and Rhine using the 1d water level algorithms
#'   \code{hyd1d::waterLevel()} and \code{hyd1d::waterLevelPegelonline()}
#' 
#' @description Computes flood duration for points located in the active
#'   floodplains along the German federal waterways Elbe and Rhine based on 1d
#'   water levels computed by \code{\link[hyd1d]{waterLevel}} or
#'   \code{\link[hyd1d]{waterLevelPegelonline}} provided by package \pkg{hyd1d}.
#' 
#' @param x has to by type \code{sf} possibly including columns \code{csa}
#'   (cross section areas) and \code{dem} (digital elevation model). To compute
#'   water levels along the River Elbe, \code{x} has to be in the coordinate
#'   reference system 
#'   \href{https://spatialreference.org/ref/epsg/etrs89-utm-zone-33n/}{ETRS 1989 UTM 33N},
#'   for the River Rhine in 
#'   \href{https://spatialreference.org/ref/epsg/etrs89-utm-zone-32n/}{ETRS 1989 UTM 32N}.
#'   Other coordinate reference systems are not permitted.
#' @param seq has to be type \code{c("POSIXct", "POSIXt")} or \code{Date} and
#'   have a length larger than 0. If \code{seq} is type \code{c("POSIXct", "POSIXt")},
#'   values must be in the temporal range between 31 days ago (\code{Sys.time()
#'   - 2678400}) and now (\code{Sys.time()}). Then
#'   \code{\link[hyd1d]{waterLevelPegelonline}} is used internally for the water
#'   level computations. If \code{seq} is type \code{Date}, values must be in the
#'   temporal range between 1960-01-01 and yesterday (\code{Sys.Date() - 1})
#' 
#' @return \code{sf} object with flood duration stored in column
#'   `flood3` in the range of \code{[0, length(seq)]}, elevation stored in
#'   column \code{dem} and cross section areas stored in column \code{csa}.
#' 
#' @details For every time step provided in \code{seq}, \code{flood3Points()}
#'   computes a 1d water level along the requested river section. This 1d water
#'   level is transfered to a temporary \code{wl} (water level) column and then
#'   compared to the \code{dem} (digital elevation model) column. Where the
#'   \code{wl} is higher than the \code{dem} flood duration \code{flood3} is
#'   increased by 1.
#'   
#'   Since the underlying tiled digital elevation models (dem) are rather
#'   large datasets hydflood provides options to permanentely cache these
#'   datasets. \code{options("hydflood.datadir" = tempdir())} is the default. To
#'   modify the location of your raster cache to your needs set the respective
#'   \code{options()} prior to loading the package, e.g.
#'   \code{options("hydflood.datadir" = "~/.hydflood");library(hydflood)}. The
#'   location can also be determined through the environmental variable
#'   \env{hydflood_datadir}.
#' 
#' @seealso \code{\link[hyd1d]{waterLevel}},
#'   \code{\link[hyd1d]{waterLevelPegelonline}}
#' 
#' @examples \donttest{
#'   options("hydflood.datadir" = tempdir())
#'   library(hydflood)
#'   
#'   # create a random points object
#'   c <- st_crs(25833)
#'   e <- st_as_sfc(st_bbox(c(xmin = 309000, xmax = 310000,
#'                            ymin = 5749000, ymax = 5750000)))
#'   st_crs(e) <- c
#'   set.seed(123)
#'   points <- st_sample(e, size = 10, "random")
#'   p <- data.frame(id = 1:10)
#'   st_geometry(p) <- points
#'   
#'   # create a temporal sequence
#'   seq <- seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by = "day")
#'   
#'   # compute a flood duration
#'   p <- flood3Points(x = p, seq = seq)
#' }
#' 
#' @export
#' 
flood3Points <- function(x, seq) {
    
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
        if (! all(c(inherits(x, "sf"), inherits(x, "data.frame")))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'x' must be typ",
                                       "e 'sf' and 'data.frame'."))
        }
        
        # crs
        if ( !isUTM32(x) & !isUTM33(x)) {
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
        if (exists("river")) {
            sf.tiles <- sf.tiles(name = river)
            sf.tiles$tile_name <- sf.tiles$name
            sf.tiles[, which(! names(sf.tiles) %in% c("tile_name", "geometry",
                                                      "url"))] <- NULL
            af <- sf.af(name = river)
            if (nrow(x[af,]) == 0) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'x' does NO",
                                           "T overlap with the active floodpla",
                                           "in of River ", river, "."))
            } else if (nrow(x[af,]) < nrow(x)) {
                message(paste0("'x' does not fully overlap with the active flo",
                               "odplain of River ", river, ".\nFlood durations",
                               " are computed only for overlapping points."))
            }
        }
    }
    
    ## seq
    if (missing(seq)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'seq' argument ",
                                   "has to be supplied."))
    } else {
        # class
        if (!inherits(seq, "Date") &
            !all(c(inherits(seq, "POSIXct"), inherits(seq, "POSIXt")))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'seq' must be e",
                                       "ither type 'Date' or c('POSIXct', 'POS",
                                       "IXt')."))
        }
        # length
        if (length(seq) < 1L) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'seq' must have",
                                       " length larger 0."))
        }
        # NA and possible range
        if (any(is.na(seq))) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'seq' or elemen",
                                       "ts of it must not be NA."))
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
                    errors <- c(errors, paste0("Error ", l(errors), ": Values ",
                                               "of 'seq' must be between 1960-",
                                               "01-01 and yesterday."))
                }
                type_date <- TRUE
            }
            rm(time_min)
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
    # get columns 'dem' and 'csa'
    if (!"id_tmp" %in% names(x)) {
            x$id_tmp <- 1:nrow(x)
    }
    
    tile_name <- FALSE
    mode <- ifelse(.Platform$OS.type == "windows", "wb", "w")
    if (!"dem" %in% names(x)) {
        x$dem <- numeric(nrow(x))
        if (!"tile_name" %in% names(x)) {
            x <- sf::st_join(x, sf.tiles, largest = TRUE)
            x$url <- NULL
            tile_name <- TRUE
        } else {
            warning("Attribute column 'tile_name' has been updated!")
        }
        for (i in unique(x$tile_name)) {
            id <- x[which(x$tile_name == i), ]$id_tmp
            f <- paste0(options()$hydflood.datadir, "/", i, "_DEM.tif")
            if (!file.exists(f)) {
                url <- sf.tiles$url[which(sf.tiles$tile_name == i)]
                tryCatch({
                    utils::download.file(url, f, quiet = TRUE, mode = mode)
                }, error = function(e){
                    stop(paste0("It was not possible to download:\n",
                                url, "\nTry again later!"))
                })
            }
            x$dem[id] <- round(terra::extract(terra::rast(f),
                                              sf::st_coordinates(x[id, ]))[,1],
                               2)
        }
    }
    
    if (! "csa" %in% names(x)) {
        csa_file <- paste0(options()$hydflood.datadir, "/sf.af",
                           tolower(substring(river, 1, 1)), "_csa.rda")
        if (!file.exists(csa_file)) {
            url <- paste0("https://hydflood.bafg.de/downloads/sf.af",
                          tolower(substring(river, 1, 1)), "_csa.rda")
            tryCatch({
                utils::download.file(url, csa_file, quiet = TRUE, mode = mode)
            }, error = function(e){
                message(paste0("It was not possible to download:\n", url,
                               "\nTry again later!"))
                return(NULL)
            })
        }
        load(csa_file)
        if (river == "Elbe") {
            assign("sf.af_csa", sf.afe_csa)
        } else {
            assign("sf.af_csa", sf.afr_csa)
        }
        # subset sf.af_csa
        sf.af_csa <- sf.af_csa[which(sf.af_csa$section %in% unique(x$tile_name)),
                               "station_int"]
        names(sf.af_csa)[1] <- "csa"
        x <- sf::st_join(x, sf.af_csa)
    }
    
    #####
    # processing
    #####
    id_nna <- which(!is.na(x$csa))
    if ("flood3" %in% names(x)) {
        warning("Attribute column 'flood3' has been updated!")
    }
    x$flood3 <- rep(NA_integer_, nrow(x))
    x$flood3[id_nna] <- 0
    
    wldf_initial <- WaterLevelDataFrame(river = river, time = as.POSIXct(NA),
                                        station_int = sort(unique(x$csa[id_nna])))
    
    # loop over all time steps
    for (i in seq) {
        if (type_date) {
            time <- as.POSIXct(format(as.Date(i, as.Date("1970-01-01")),
                                      "%Y-%m-%d"), tz = "CET")
            wldf <- wldf_initial
            setTime(wldf) <- time
            wldf <- hyd1d::waterLevel(wldf)
        } else {
            time <- as.POSIXct(i, origin = "1970-01-01 00:00:00")
            wldf <- wldf_initial
            setTime(wldf) <- time
            wldf <- hyd1d::waterLevelPegelonline(wldf)
        }
        df <- as.data.frame(wldf)
        xdf <- merge(x, df, by.x = "csa", by.y = "station_int",
                     all.x = TRUE, incomparables = NA)
        id <- xdf$id_tmp[which(xdf$w > xdf$dem)]
        x$flood3[id] <- x$flood3[id] + 1
    }
    
    x$id_tmp <- NULL
    
    return(x)
}

