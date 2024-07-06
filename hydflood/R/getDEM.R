#' @name getDEM
#' @rdname getDEM
#' 
#' @title Function to obtain the digital elevation models for the active
#'   floodplains along the German federal waterways Elbe and Rhine
#' 
#' @description This function downloads and patches the tiled digital elevation
#'   models (dem) along the German federal waterways Elbe and Rhine that have
#'   been published on \href{https://www.pangaea.de}{pangaea.de}.
#' 
#' @param filename supplies an optional in- and output filename and has to be
#'   type \code{character}.
#' @param ext argument of type \code{\link[terra]{SpatExtent}}.
#' @param crs argument of type \code{\link[sf:st_crs]{crs}} or 
#'   \code{\link[terra]{crs}}. It is
#'   used to select the respective river (Elbe: \href{https://spatialreference.org/ref/epsg/etrs89-utm-zone-33n/}{'ETRS 1989 UTM 33N'}; Rhine:
#'   \href{https://spatialreference.org/ref/epsg/etrs89-utm-zone-32n/}{'ETRS 1989 UTM 32N'})
#' @param \dots additional arguments as for \code{\link[terra]{writeRaster}}.
#' 
#' @return \code{SpatRaster} object containing elevation data for the selected
#'   floodplain region.
#' 
#' @details Since the underlying tiled digital elevation models (dem) are rather
#'   large datasets hydflood provides options to permanentely cache these
#'   datasets. \code{options("hydflood.datadir" = tempdir())} is the default. To
#'   modify the location of your raster cache to your needs set the respective
#'   \code{options()} prior to loading the package, e.g.
#'   \code{options("hydflood.datadir" = "~/.hydflood");library(hydflood)}. The
#'   location can also be determined through the environmental variable
#'   \env{hydflood_datadir}.
#'   
#'   Since downloads of large individual datasets might cause timeouts, it is
#'   recommended to increase \code{options("timeout")}.
#' 
#' @references 
#'   \insertRef{weber_dgms_2020}{hydflood}
#'   
#'   \insertRef{weber_dgm_elbe_2020}{hydflood}
#'   
#'   \insertRef{weber_dgm_rhine_2020}{hydflood}
#' 
#' @examples \donttest{
#'   options("hydflood.datadir" = tempdir())
#'   options("timeout" = 120)
#'   library(hydflood)
#'   dem <- getDEM(ext = ext(c(309000, 310000, 5749000, 5750000)),
#'                 crs = st_crs("EPSG:25833"))
#' }
#' 
#' @export
#' 
getDEM <- function(filename = '', ext, crs, ...) {
    
    #####
    # validate the input data
    ##
    # vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    ##
    # filename
    if (!inherits(filename, "character")) {
        errors <- c(errors, paste0("Error ", l(errors), ": 'filename' ",
                                   "must be type 'character'."))
    }
    if (length(filename) != 1) {
        errors <- c(errors, paste0("Error ", l(errors), ": 'filename' ",
                                   "must have length 1."))
    }
    
    if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
    
    if (filename == '') {
        if (missing(ext) & missing(crs)) {
            stop(paste0("Error 1: If you don't provide an existing 'filename',",
                        " you have to specify 'ext' and 'crs'."))
        }
        file_exists_dem <- FALSE
        file_create_dem <- FALSE
        ext_int_dem <- FALSE
        crs_int_dem <- FALSE
    } else {
        if (file.exists(filename)) {
            file_exists_dem <- TRUE
            file_create_dem <- FALSE
            ext_int_dem <- TRUE
            crs_int_dem <- TRUE
            raster.dem <- terra::rast(x = filename)
            ext_dem <- terra::ext(raster.dem)
            crs_dem <- terra::crs(raster.dem)
            res_dem <- terra::res(raster.dem)
        } else {
            file_exists_dem <- FALSE
            file_create_dem <- TRUE
            ext_int_dem <- FALSE
            crs_int_dem <- FALSE
        }
    }
    
    ##
    # crs
    if (missing(crs)) {
        if (crs_int_dem) {
            crs_int <- crs_dem
        } else {
            errors <- c(errors, paste0("Error ", l(errors), ": If 'filename' d",
                                       "oes not provide a crs, you must specif",
                                       "y 'crs'."))
            stop(paste0(errors, collapse="\n  "))
        }
    }
    if (!missing(crs)) {
        if (!inherits(crs, "crs")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'crs' must be t",
                                       "ype 'crs'."))
            stop(paste0(errors, collapse="\n  "))
        }
        if (crs_int_dem) {
            if (sf::st_crs(crs) != sf::st_crs(crs_dem)) {
                errors <- c(errors, paste0("Error ", l(errors), ": The supplie",
                                           "d 'crs' does not agree with the cr",
                                           "s of the raster supplied through '",
                                           "filename'."))
                stop(paste0(errors, collapse="\n  "))
            }
            crs_int <- crs_dem
        } else {
            crs_int <- crs
        }
    }
    
    # check standard projections
    if (!isUTM32(crs_int) & !isUTM33(crs_int)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The supplied 'crs' ",
                                   "must be either 'ETRS 1989 UTM 32N' or 'ETR",
                                   "S 1989 UTM 33N'."))
        stop(paste0(errors, collapse="\n  "))
    } else {
        if (isUTM32(crs_int)) {
            river <- "Rhine"
        } else if (isUTM33(crs_int)) {
            river <- "Elbe"
        } else {
            stop(errors)
        }
    }
    
    ##
    # ext
    crop <- FALSE
    if (missing(ext)) {
        if (ext_int_dem) {
            ext_int <- ext_dem
        } else {
            errors <- c(errors, paste0("Error ", l(errors), ": If 'filename' d",
                                       "oes not provide an extent, so you must",
                                       " specify the 'ext'."))
            stop(paste0(errors, collapse="\n  "))
        }
    }
    if (!missing(ext)) {
        if (!inherits(ext, "SpatExtent")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'ext' must ",
                                       "be type 'SpatExtent'."))
            stop(paste0(errors, collapse="\n  "))
        }
        if (ext_int_dem) {
            if (ext == ext_dem) {
                ext_int <- ext
            } else if (comp_ext(ext, ext_dem)) {
                message("'ext' will be used to crop the supplied raster file.")
                ext_int <- ext
                crop <- TRUE
            } else {
                errors <- c(errors, paste0("Error ", l(errors), ": The supplie",
                                           "d 'ext' must be totally within the",
                                           " raster supplied through 'filename",
                                           "'."))
                stop(paste0(errors, collapse="\n  "))
            }
        } else {
            ext_int <- ext
        }
    }
    
    ##
    # in area
    # check position
    sf.ext <- extent2polygon(ext_int, crs_int)
    if (exists("river")) {
        af <- sf.af(name = river)
        if (! (nrow(af[sf.ext,]) > 0)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The selected 'e",
                                       "xt' does NOT overlap with the active f",
                                       "loodplain of River ", river, "."))
        }
    }
    
    # error messages
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    } else {
        rm(l, errors)
    }
    
    #####
    # additional args
    args <- list(...)
    overwrite <- FALSE
    if ("overwrite" %in% names(args)) {
        overwrite <- args[["overwrite"]]
        if (overwrite) {
            file_create_dem <- TRUE
        }
    }
    
    #####
    # processing
    if (crop) {
        return(terra::crop(raster.dem, vect(sf.ext)))
    }
    if (file.exists(filename) & missing(ext) & missing(crs)) {
        return(raster.dem)
    }
    if (file.exists(filename)) {
        if (terra::ext(terra::rast(filename)) == ext_int) {
            return(raster.dem)
        }
    }
    
    nrows <- as.integer(ymax(ext_int) - ymin(ext_int))
    ncols <- as.integer(xmax(ext_int) - xmin(ext_int))
    
    in_memory <- raster::canProcessInMemory(raster::raster(nrows = nrows,
                                                           ncols = ncols,
                                                           xmn = xmin(ext_int),
                                                           xmx = xmax(ext_int),
                                                           ymn = ymin(ext_int),
                                                           ymx = ymax(ext_int),
                                                           resolution = 1
                                                           ), n = 2)
    
    sf.tiles <- sf.tiles(name = river)[sf.ext,]
    
    if (nrow(sf.tiles) > 5) {
        stop(paste0("Error: The choosen 'ext' is very large and covers more th",
                    "an 5 DEM tiles.\n   Please reduce the size of your extent",
                    "."))
    }
    if (nrow(sf.tiles) > 3) {
        warning(paste0("Error: The choosen 'ext' is large and covers more than",
                       " 3 DEM tiles.\n   Please reduce the size of your exten",
                       "t to avoid overly long computation times."))
    }
    
    merge_files <- list(nrow(sf.tiles))
    missing_files <- NA_character_
    mode <- ifelse(.Platform$OS.type == "windows", "wb", "w")
    for (i in 1:nrow(sf.tiles)) {
        file <- paste0(options()$hydflood.datadir, "/", sf.tiles$name[i],
                       "_DEM.tif")
        if (!file.exists(file)) {
            
            tryCatch({
                utils::download.file(sf.tiles$url[i], file, quiet = TRUE,
                                     mode = mode)
            }, error = function(e){
                mess <- paste0("It was not possible to download:\n",
                               sf.tiles$url[i], "\nPlease try again!")
                w <- warnings()
                w_mess <- names(w)
                w_mess <- w_mess[startsWith(w_mess, "URL")]
                if (grepl("Timeout", w_mess) & grepl("was reached", w_mess)) {
                    mess <- paste0(mess, "\nSince a timeout was reached, it is",
                                   " recommended to increase the value of \n",
                                   "options('timeout') presently set to ",
                                   options('timeout')$timeout, " seconds.")
                }
                message(mess)
            })
        }
        
        if (file.exists(file)) {
            merge_files[[i]] <- terra::rast(x = file)
        } else {
            if (is.na(missing_files)) {
                missing_files <- sf.tiles$url[i]
            } else {
                missing_files <- append(missing_files, sf.tiles$url[i])
            }
        }
    }
    
    if (length(merge_files) == 0) {
        return(NULL)
    } else if (length(merge_files) == 1) {
        if (file_create_dem) {
            raster.dem <- terra::crop(merge_files[[1]], y = ext_int,
                                      extend = TRUE)
            if (!file.exists(filename) | overwrite) {
                terra::writeRaster(raster.dem, filename = filename, ...)
            }
        } else {
            if (!in_memory) {
                tmp_dem <- tempfile(fileext = ".tif")
                raster.dem <- terra::crop(merge_files[[1]], y = ext_int,
                                          filename = tmp_dem, extend = TRUE)
            } else {
                raster.dem <- terra::crop(merge_files[[1]], y = ext_int,
                                          extend = TRUE)
            }
        }
    } else if (length(merge_files) > 1) {
        merge_rasters <- list("x" = terra::sprc(merge_files))
        if (file_create_dem) {
            merge_rasters[["filename"]] <- filename
            if (length(args) > 0) {
                for (i in 1:length(args)) {
                    merge_rasters[[names(args)[i]]] <- args[i]
                }
            }
        } else {
            tmp_dem <- tempfile(fileext = ".tif")
            merge_rasters[["filename"]] <- tmp_dem
        }
        # merge_rasters[["overlap"]] <- TRUE
        # merge_rasters[["ext"]] <- ext_int
        if (overwrite) {
            merge_rasters[["overwrite"]] <- TRUE
        }
        merge_rasters[["progress"]] <- 0
        raster.dem <- do.call("merge", merge_rasters)
        
        if (ext_int <= ext(raster.dem)) {
            raster.dem <- terra::crop(raster.dem, ext_int, extend = TRUE)
            if (file_create_dem) {
                terra::writeRaster(raster.dem, filename = filename,
                                   overwrite = TRUE, ...)
            }
        }
    } else {
        if (file_create_dem) {
            raster.dem <- terra::crop(merge_rasters$x, y = ext_int,
                                      extend = TRUE)
            if (!file.exists(filename) | overwrite) {
                terra::writeRaster(raster.dem, filename = filename, ...)
            }
        } else {
            if (!in_memory) {
                tmp_dem <- tempfile(fileext = ".tif")
                raster.dem <- terra::crop(merge_rasters$x, y = ext_int,
                                          filename = tmp_dem, extend = TRUE)
            } else {
                raster.dem <- terra::crop(merge_rasters$x, y = ext_int,
                                          extend = TRUE)
            }
        }
    }
    
    if (!is.na(missing_files)) {
        message(paste0("\nIt was not possible to download:\n ",
                       paste0(missing_files, collapse = "\n "),
                       "\nMissing parts have been replaced by NAs.\n",
                       "Please try again later to obtain a full DEM!"))
    }
    
    return(raster.dem)
    
}

