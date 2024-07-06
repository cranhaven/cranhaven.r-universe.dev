#' @name hydSpatRaster
#' @rdname hydSpatRaster
#' @title Initialize a \code{SpatRaster} for the flood-functions
#'
#' @description To initialize an object of class \code{\link[terra]{SpatRaster}}
#'   with layers dem and csa this function should be used. It checks all the
#'   required input data, downloads missing data automatically, clips and
#'   returns the final object, prepared for the \code{flood()} functions
#'   (\code{\link{flood1}}, \code{\link{flood2}} and \code{\link{flood3}}).
#'
#' @param filename_dem an optional argument of length 1 with type
#'   \code{character} specifying a filename of a **d**igital **e**levation
#'   **m**odel raster dataset.
#'   
#'   If the file exists it is imported via \code{\link[terra]{rast}} and used
#'   to build the \code{SpatRaster}, potentially cropped by argument
#'   \code{ext}. If the dem file does not exist, data are downloaded
#'   automatically and exported using \code{\link[terra]{writeRaster}} and can
#'   be reused to accelerate later computations.
#'   
#'   An existing dataset must be either in the coordinate reference system (crs)
#'   'ETRS 1989 UTM 32N' (epsg: 25832) for the River Rhine or 'ETRS 1989 UTM 33N'
#'   (epsg: 25833) for the River Elbe. It must also overlap with the active
#'   floodplains (\code{\link{sf.afe}} or \code{\link{sf.afr}}) of the river
#'   selected through the crs.
#'   
#'   If argument \code{filename_csa} is specified and exists too, 
#'   the coordinate reference system (\code{\link[terra]{crs}}), extent
#'   (\code{\link[terra]{ext}}) and resolution (\code{\link[terra]{res}})
#'   of both raster datasets must match.
#'   
#'   Supported file types depend on available
#'   \href{https://gdal.org/drivers/raster/index.html}{GDAL raster drivers}.
#' 
#' @param filename_csa an optional argument of length 1 with type 
#'   \code{character} specifying a filename of a **c**ross **s**ection 
#'   **a**rea raster dataset. 
#'   
#'   If the file exists it is imported via \code{\link[terra]{rast}} and used
#'   to build the \code{SpatRaster}, potentially cropped by argument
#'   \code{ext}. If the csa file does not exist, data are downloaded
#'   automatically and exported using \code{\link[terra]{writeRaster}} and can
#'   be reused to accelerate later computations.
#'   
#'   An existing dataset must be either in the coordinate reference system (crs)
#'   'ETRS 1989 UTM 32N' (epsg: 25832) for the River Rhine or 'ETRS 1989 UTM 33N'
#'   (epsg: 25833) for the River Elbe. It must also overlap with the active
#'   floodplains (\code{\link{sf.afe}} or \code{\link{sf.afr}}) of the river
#'   selected through the crs and be in the possible range of \code{station_int}
#'   values: Elbe (m 0 - 585700), Rhine (m 336200 - 865700).
#'   
#'   If argument \code{filename_dem} is specified too, coordinate reference
#'   system (\code{\link[terra]{crs}}), extent (\code{\link[terra]{ext}})
#'   and resolution (\code{\link[terra]{res}}) of both raster datasets must
#'   match.
#'   
#'   Supported file types depend on available
#'   \href{https://gdal.org/drivers/raster/index.html}{GDAL raster drivers}.
#' 
#' @param ext optional argument of type \code{\link[terra]{SpatExtent}}. If neither 
#'   \code{filename_dem} nor \code{filename_csa} are specified, \code{ext} is 
#'   required to download the respective data and generate temporary dem and csa
#'   datasets. If either \code{filename_dem} or \code{filename_csa} or both are
#'   specified, \code{ext} must be within the extent of provided raster layers.
#'   Then it is used to \code{\link[terra]{crop}} the supplied data.
#' 
#' @param crs optional argument of type \code{\link[sf:st_crs]{crs}} or 
#'   \code{\link[terra]{crs}}. If
#'   neither \code{filename_dem} nor \code{filename_csa} are specified,
#'   \code{crs} is used to select the respective river (Elbe:
#'   'ETRS 1989 UTM 33N' (epsg: 25833); Rhine: 'ETRS 1989 UTM 32N' (epsg:
#'   25832)) and \code{\link[terra]{crop}} downloaded dem and csa
#'   by the given \code{ext}. If either \code{filename_dem} or 
#'   \code{filename_csa} or both are specified, \code{crs} must match their 
#'   coordinate reference systems; otherwise an error is returned.
#' 
#' @param \dots additional parameters passed to
#'   \code{\link[terra]{writeRaster}}.
#' 
#' @return \code{SpatRaster} object containing digital elevation (\code{dem})
#'   and cross section area (\code{csa}) raster layers.
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
#' @seealso \code{\link[terra]{SpatRaster-class}},
#'   \code{\link[terra]{rast}}, \code{\link[terra]{writeRaster}},
#'   \code{\link{flood1}}, \code{\link{flood2}}, \code{\link{flood3}},
#'   \code{\link{sf.afe}}, \code{\link{sf.afr}}
#' 
#' @references
#'   \insertRef{wsv_dgmw_2016}{hydflood}
#'   
#'   \insertRef{brockmann_auswertung_2008}{hydflood}
#'   
#'   \insertRef{brockmann_produktblatt_2012}{hydflood}
#'   
#'   \insertRef{brockmann_digitales_2008}{hydflood}
#'   
#'   \insertRef{smile_consult_gmbh_dgm-w_2011}{hydflood}
#'   
#'   \insertRef{fugro-hgn_gmbh_aufbau_2011}{hydflood}
#'   
#'   \insertRef{arge_vermessung_schmid_-_inphoris_aufbau_2012}{hydflood}
#'   
#'   \insertRef{weber_dgms_2020}{hydflood}
#'   
#'   \insertRef{weber_dgm_elbe_2020}{hydflood}
#'   
#'   \insertRef{weber_dgm_rhine_2020}{hydflood}
#'   
#'   \insertRef{bundesanstalt_fur_gewasserkunde_flys_2016}{hydflood}
#'   
#'   \insertRef{brunotte_flussauen_data_2009}{hydflood}
#' 
#' @examples \donttest{
#'   options("hydflood.datadir" = tempdir())
#'   options("timeout" = 120)
#'   library(hydflood)
#'   
#'   e <- ext(436500, 438000, 5415000, 5416500)
#'   c <- st_crs("EPSG:25832")
#'   
#'   r <- hydSpatRaster(ext = e, crs = c)
#'   r
#' }
#' 
#' @export
#' 
hydSpatRaster <- function(filename_dem = '', filename_csa = '', ext, crs, ...) {
    
    #####
    # validate the input data
    ##
    # vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    # make parent environment accessible through the local environment
    e <- environment()
    p_env <- parent.env(e)
    
    ##
    # filename_dem
    if (missing(filename_dem)) {
        file_exists_dem <- FALSE
        file_create_dem <- FALSE
        ext_int_dem <- FALSE
        crs_int_dem <- FALSE
    } else {
        if (!inherits(filename_dem, "character")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'filename_dem' ",
                                       "must be type 'character'."))
        }
        if (length(filename_dem) != 1) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'filename_dem' ",
                                       "must have length 1."))
        }
        # 1st time error messages
        if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
        
        if (filename_dem == '') {
            file_exists_dem <- FALSE
            file_create_dem <- FALSE
            ext_int_dem <- FALSE
            crs_int_dem <- FALSE
        } else {
            if (file.exists(filename_dem)) {
                dem <- terra::rast(x = filename_dem)
                file_exists_dem <- TRUE
                file_create_dem <- FALSE
                ext_int_dem <- terra::ext(dem)
                crs_int_dem <- terra::crs(dem)
                res_int_dem <- terra::res(dem)
            } else {
                file_exists_dem <- FALSE
                file_create_dem <- TRUE
                ext_int_dem <- FALSE
                crs_int_dem <- FALSE
            }
        }
    }
    
    ## 
    # filename_csa
    if (missing(filename_csa)) {
        file_exists_csa <- FALSE
        file_create_csa <- FALSE
        ext_int_csa <- FALSE
        crs_int_csa <- FALSE
    } else {
        if (!inherits(filename_csa, "character")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'filename_csa' ",
                                       "must be type 'character'."))
        }
        if (length(filename_csa) != 1) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'filename_csa' ",
                                       "must have length 1."))
        }
        # 2nd time error messages
        if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
        
        if (filename_csa == '') {
            file_exists_csa <- FALSE
            file_create_csa <- FALSE
            ext_int_csa <- FALSE
            crs_int_csa <- FALSE
        } else {
            if (file.exists(filename_csa)) {
                csa <- terra::rast(x = filename_csa)
                file_exists_csa <- TRUE
                file_create_csa <- FALSE
                ext_int_csa <- terra::ext(csa)
                crs_int_csa <- terra::crs(csa)
                res_int_csa <- terra::res(csa)
            } else {
                file_exists_csa <- FALSE
                file_create_csa <- TRUE
                ext_int_csa <- FALSE
                crs_int_csa <- FALSE
            }
        }
    }
    
    ##
    # compare geometries of 
    if (!is.logical(ext_int_dem) & !is.logical(ext_int_csa)) {
        if (terra::compareGeom(dem, csa)) {
            ext_int_ras <- ext_int_dem
            res_int <- res_int_dem
            crs_int_ras <- crs_int_dem
        }
    }
    if (!is.logical(ext_int_dem) & is.logical(ext_int_csa)) {
        ext_int_ras <- ext_int_dem
        res_int <- res_int_dem
        crs_int_ras <- crs_int_dem
        rm(res_int_dem)
    }
    if (is.logical(ext_int_dem) & !is.logical(ext_int_csa)) {
        ext_int_ras <- ext_int_csa
        res_int <- res_int_csa
        crs_int_ras <- crs_int_csa
        rm(res_int_csa)
    }
    if (is.logical(ext_int_dem) & is.logical(ext_int_csa)) {
        ext_int_ras <- FALSE
        res_int <- c(1, 1)
        crs_int_ras <- FALSE
    }
    #rm(crs_int_csa, crs_int_dem)
    
    # 3rd time error messages
    if (!file_exists_dem & !file_exists_csa & missing(ext) & missing(crs)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The specified files",
                                   " do not exist! If you specifiy the extent ",
                                   "and a valid crs (ETRS 1989 UTM32 N for Riv",
                                   "er Rhine, ETRS 1989 UTM32 N for River Elbe",
                                   "), they will be downloaded according to yo",
                                   "ur specifications."))
    }
    if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
    
    ##
    # ext
    crop <- FALSE
    if (missing(ext) & is.logical(ext_int_ras)) {
    }
    if (missing(ext) & !is.logical(ext_int_ras)) {
        ext_int <- ext_int_ras
    }
    if (!missing(ext) & is.logical(ext_int_ras)) {
        if (!inherits(ext, "SpatExtent")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'ext' must ",
                                       "be type 'SpatExtent'."))
        } else {
            ext_int <- ext
        }
    }
    if (!missing(ext) & !is.logical(ext_int_ras)) {
        if (ext == ext_int_ras) {
            ext_int <- ext
        } else if (comp_ext(ext, ext_int_ras)) {
            message("'ext' will be used to crop the supplied raster file(s).")
            ext_int <- ext
            crop <- TRUE
        } else {
            errors <- c(errors, paste0("Error ", l(errors), ": The supplied 'e",
                                       "xt' must be totally within the supplie",
                                       "d raster(s)."))
        }
    }
    #if (exists("ext")) {rm(ext)}
    #if (exists("ext_int_ras")) {rm(ext_int_ras)}
    
    # 4th time error messages
    if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
    
    ## 
    # crs
    # check existence
    if (missing(crs) & is.logical(crs_int_ras)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'crs' ",
                                   "argument has to be supplied."))
    }
    if (missing(crs) & !is.logical(crs_int_ras)) {
        crs_int <- crs_int_ras
    }
    if (!missing(crs) & is.logical(crs_int_ras)) {
        if (!inherits(crs, "crs")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'crs' must ",
                                       "be type 'crs'."))
        } else {
            crs_int <- crs
        }
    }
    if (!missing(crs) & !is.logical(crs_int_ras)) {
        if (sf::st_crs(crs) != sf::st_crs(crs_int_ras)) {
            errors <- c(errors, paste0("Error ", l(errors), ": The supplied 'c",
                                       "rs' does not agree with the crs suppli",
                                       "ed through the raster(s)."))
        } else {
            crs_int <- crs
        }
    }
    #if (exists("crs")) {rm(crs)}
    #if (exists("crs_int_ras")) {rm(crs_int_ras)}
    # browser()
    
    # 5th time error messages
    if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
    
    # check standard projections
    # if (is.character(crs_int)) {
    #     crs_int <- sf::st_crs(crs_int)
    # }
    if ( !isUTM32(crs_int) & !isUTM33(crs_int)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The supplied crs mu",
                                   "st be either 'ETRS 1989 UTM 32N' or 'ETRS ",
                                   "1989 UTM 33N'."))
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
    
    # 6th time error messages
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    } else {
        rm(l, errors)
    }
    
    #####
    # additional args
    args <- list(...)
    
    #####
    # get missing input data
    nrows <- as.integer((ext_int$vector[4] - ext_int$vector[3]) / res_int[2])
    ncols <- as.integer((ext_int$vector[2] - ext_int$vector[1]) / res_int[1])
    # in_memory <- terra::canProcessInMemory(terra::rast(ext_int, 
    #                                                    nrows = nrows, 
    #                                                    ncols = ncols, 
    #                                                    crs = crs_int), 
    #                                         n = 2)
    
    ##
    # csa
    if (file_exists_csa) {
        if (crop) {
            csa <- terra::crop(csa, ext_int)
        }
    }
    
    if (!file_exists_csa) {
        # download the packages csa_file, if it has not yet been stored locally,
        # and load it
        csa_file <- paste0(options()$hydflood.datadir, "/sf.af",
                           tolower(substring(river, 1, 1)), "_csa.rda")
        if (!file.exists(csa_file)) {
            url <- paste0("https://hydflood.bafg.de/downloads/sf.af",
                          tolower(substring(river, 1, 1)), "_csa.rda")
            mode <- ifelse(.Platform$OS.type == "windows", "wb", "w")
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
        
        # subset it to sf.ext
        sf.csa <- sf.af_csa[sf.ext, ]
        
        # identify relevant sections
        sections_sel <- unique(sf.csa$section)
        for (a_section in sort(sections_sel, decreasing = TRUE)) {
            if (a_section %in% unique(na.omit(sf.csa$section_do))) {
                id <- which(sf.csa$section_do == a_section)
                id_min <- min(id)
                id_max <- max(id)
                if (id_min == 1 | id_max == nrow(sf.csa)) {
                    sections_sel <- unique(sf.csa$section[-id])
                }
            }
        }
        
        if (length(sections_sel) > 5) {
            stop(paste0("Error: The choosen 'ext' is very large and covers mor",
                        "e than 5 sections\n used for tiled computations. Plea",
                        "se reduce the size of you computation extent."))
        }
        if (length(sections_sel) > 3) {
            warning(paste0("Error: The choosen 'ext' is large and covers more ",
                           "than 3 sections\n used for tiled computations. Ple",
                           "ase reduce the size of your extent to\n avoid over",
                           "ly long computation times."))
        }
        
        # convert it to a raster
        csa <- terra::rast(x = ext_int,
                           ncols = terra::xmax(ext_int) - terra::xmin(ext_int),
                           nrows = terra::ymax(ext_int) - terra::ymin(ext_int),
                           resolution = 1, vals = NA)
        if (inherits(crs_int, "crs")) {
            terra::crs(csa) <- crs_int$wkt
        } else if (inherits(crs_int, "character")) {
            terra::crs(csa) <- sf::st_crs(crs_int)$wkt
        } else {
            stop("Error")
        }
        
        csa <- terra::rasterize(terra::vect(sf.csa), csa, field = "station_int",
                                update = TRUE)
        if (crop) {
            csa <- terra::crop(csa, ext_int)
        }
        if (file_create_csa & !file.exists(filename_csa)) {
                terra::writeRaster(csa, datatype = "INT4S",
                                   filename = filename_csa, ...)
        }
    }
    
    ##
    # dem
    if (file_exists_dem) {
        if (crop) {
            dem <- terra::crop(dem, ext_int)
        }
    } else {
        if (file_create_dem) {
            dem <- getDEM(filename = filename_dem, ext = ext_int,
                          crs = sf::st_crs(crs_int), ...)
        } else {
            dem <- getDEM(filename = tempfile(fileext = ".tif"), ext = ext_int,
                          crs = sf::st_crs(crs_int))
        }
    }
    
    #####
    # assemble and return the product
    x <- terra::rast(list(dem = dem, csa = csa))
    return(x)
}

