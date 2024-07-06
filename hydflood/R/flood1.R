#' @name flood1
#' @rdname flood1
#' 
#' @title Function to compute flood extent or flood duration \code{SpatRaster} 
#'   along the German federal waterways Elbe and Rhine using the 1d water level 
#'   algorithm \code{hyd1d::waterLevelFlood1()}
#' 
#' @description Computes flood extent, if \code{length(seq)} equals 1, or flood 
#'   duration for the active floodplains along the German federal waterways Elbe 
#'   and Rhine based on 1d water levels computed by
#'   \code{\link[hyd1d]{waterLevelFlood1}} provided by package \pkg{hyd1d} in
#'   analogy to the INFORM 3 module 'Flut1'.
#' 
#' @param x has to be type \code{SpatRaster} and has to include both input 
#'   layers \code{csa} (cross section areas) and \code{dem} (digital 
#'   elevation model). To compute water levels along the River Elbe, \code{x} 
#'   has to be in the coordinate reference system 
#'   \href{https://spatialreference.org/ref/epsg/etrs89-utm-zone-33n/}{ETRS 1989 UTM 33N},
#'   for the River Rhine in 
#'   \href{https://spatialreference.org/ref/epsg/etrs89-utm-zone-32n/}{ETRS 1989 UTM 32N}.
#'   Other coordinate reference systems are not permitted.
#' @param seq has to be type \code{c("POSIXct", "POSIXt")} or \code{Date} and 
#'   have a length larger than 0. Values of \code{seq} must be in the 
#'   temporal range between 1960-01-01 and yesterday (\code{Sys.Date() - 1}). 
#'   Internally \code{\link[hyd1d]{waterLevelFlood1}} uses \code{\link[hyd1d]{getGaugingDataW}}
#'   to obtain daily water level information from \code{\link[hyd1d]{df.gauging_data}}.
#' @param gauging_station has to be type \code{character} and has to have a
#'   length of one. Permitted values are: 'SCHOENA', 'PIRNA', 'DRESDEN',
#'   'MEISSEN', 'RIESA', 'MUEHLBERG', 'TORGAU', 'PRETZSCH-MAUKEN', 'ELSTER',
#'   'WITTENBERG', 'COSWIG', 'VOCKERODE', 'ROSSLAU', 'DESSAU', 'AKEN', 'BARBY',
#'   'SCHOENEBECK', 'MAGDEBURG-BUCKAU', 'MAGDEBURG-STROMBRUECKE',
#'   'MAGDEBURG-ROTHENSEE', 'NIEGRIPP AP', 'ROGAETZ', 'TANGERMUENDE', 'STORKAU',
#'   'SANDAU', 'SCHARLEUK', 'WITTENBERGE', 'MUEGGENDORF', 'SCHNACKENBURG',
#'   'LENZEN', 'GORLEBEN', 'DOEMITZ', 'DAMNATZ', 'HITZACKER', 'NEU DARCHAU',
#'   'BLECKEDE', 'BOIZENBURG', 'HOHNSTORF', 'ARTLENBURG', 'GEESTHACHT',
#'   'RHEINWEILER', 'BREISACH', 'RUST', 'OTTENHEIM', 'KEHL-KRONENHOF',
#'   'IFFEZHEIM', 'PLITTERSDORF', 'MAXAU', 'PHILIPPSBURG', 'SPEYER', 'MANNHEIM',
#'   'WORMS', 'NIERSTEIN-OPPENHEIM', 'MAINZ', 'OESTRICH', 'BINGEN', 'KAUB',
#'   'SANKT GOAR', 'BOPPARD', 'BRAUBACH', 'KOBLENZ', 'ANDERNACH', 'OBERWINTER',
#'   'BONN', 'KOELN', 'DUESSELDORF', 'RUHRORT', 'WESEL', 'REES', 'EMMERICH'.
#' @param uuid has to be type \code{character} and has to have a length of one.
#'   Permitted values are: '7cb7461b-3530-4c01-8978-7f676b8f71ed',
#'   '85d686f1-55b2-4d36-8dba-3207b50901a7',
#'   '70272185-b2b3-4178-96b8-43bea330dcae',
#'   '24440872-5bd2-4fb3-8554-907b49816c49',
#'   'b04b739d-7ffa-41ee-9eb9-95cb1b4ef508',
#'   '16b9b4e7-be14-41fd-941e-6755c97276cc',
#'   '83bbaedb-5d81-4bc6-9f66-3bd700c99c1f',
#'   'f3dc8f07-c2bb-4b92-b0b0-4e01a395a2c6',
#'   'c093b557-4954-4f05-8f5c-6c6d7916c62d',
#'   '070b1eb4-3872-4e07-b2e5-e25fd9251b93',
#'   '1ce53a59-33b9-40dc-9b17-3cd2a2414607',
#'   'ae93f2a5-612e-4514-b5fd-9c8aecdd73c7',
#'   'e97116a4-7d30-4671-8ba1-cdce0a153d1d',
#'   '1edc5fa4-88af-47f5-95a4-0e77a06fe8b1',
#'   '094b96e5-caeb-46d3-a8ee-d44182add069',
#'   '939f82ec-15a9-49c8-8828-dc2f8a2d49e2',
#'   '90bcb315-f080-41a8-a0ac-6122331bb4cf',
#'   'b8567c1e-8610-4c2b-a240-65e8a74919fa',
#'   'ccccb57f-a2f9-4183-ae88-5710d3afaefd',
#'   'e30f2e83-b80b-4b96-8f39-fa60317afcc7',
#'   '3adf88fd-fd7a-41d0-84f5-1143c98a6564',
#'   '133f0f6c-2ca1-4798-9360-5b5f417dd839',
#'   '13e91b77-90f3-41a5-a320-641748e9c311',
#'   'de4cc1db-51cb-4b62-bee2-9750cbe4f5c4',
#'   'f4c55f77-ab80-4e00-bed3-aa6631aba074',
#'   'e32b0a28-8cd5-4053-bc86-fff9c6469106',
#'   'cbf3cd49-91bd-49cc-8926-ccc6c0e7eca4',
#'   '48f2661f-f9cb-4093-9d57-da2418ed656e',
#'   '550e3885-a9d1-4e55-bd25-34228bd6d988',
#'   'c80a4f21-528c-4771-98d7-10cd591699a4',
#'   'ac507f42-1593-49ea-865f-10b2523617c7',
#'   '6e3ea719-48b1-408a-bc55-0986c1e94cd5',
#'   'c233674f-259a-4304-b81f-dce1f415d85b',
#'   'a26e57c9-1cb8-4fca-ba80-9e02abc81df8',
#'   '67d6e882-b60c-40d3-975c-a6d7a2b4e40a',
#'   '6aa1cd8e-e528-4bcb-ba8e-705b6dcb7da2',
#'   '33e0bce0-13df-4ffc-be9d-f1a79e795e1c',
#'   'd9289367-c8aa-4b6a-b1ad-857fec94c6bb',
#'   'b3492c68-8373-4769-9b29-22f66635a478',
#'   '44f7e955-c97d-45c8-9ed7-19406806fb4c',
#'   '06b978dd-8c4d-48ac-a0c8-2c16681ed281',
#'   '9da1ad2b-88db-4cbb-8132-eddfab07d5ba',
#'   '5389b878-fad5-4f37-bb87-e6cb36b7078b',
#'   '787e5d63-61e2-48cc-acf0-633e2bf923f2',
#'   '23af9b02-5c82-4f6e-acb8-f92a06e5e4da',
#'   'b02be240-1364-4c97-8bb6-675d7d842332',
#'   '6b774802-fcb5-49ae-8ecb-ecaf1a278b1c',
#'   'b6c6d5c8-e2d5-4469-8dd8-fa972ef7eaea',
#'   '88e972e1-88a0-4eb9-847c-0925e5999a46',
#'   '2cb8ae5b-c5c9-4fa8-bac0-bb724f2754f4',
#'   '57090802-c51a-4d09-8340-b4453cd0e1f5',
#'   '844a620f-f3b8-4b6b-8e3c-783ae2aa232a',
#'   'd28e7ed1-3317-41c5-bec6-725369ed1171',
#'   'a37a9aa3-45e9-4d90-9df6-109f3a28a5af',
#'   '665be0fe-5e38-43f6-8b04-02a93bdbeeb4',
#'   '0309cd61-90c9-470e-99d4-2ee4fb2c5f84',
#'   '1d26e504-7f9e-480a-b52c-5932be6549ab',
#'   '550eb7e9-172e-48e4-ae1e-d1b761b42223',
#'   '2ff6379d-d168-4022-8da0-16846d45ef9b',
#'   'd6dc44d1-63ac-4871-b175-60ac4040069a',
#'   '4c7d796a-39f2-4f26-97a9-3aad01713e29',
#'   '5735892a-ec65-4b29-97c5-50939aa9584e',
#'   'b45359df-c020-4314-adb1-d1921db642da',
#'   '593647aa-9fea-43ec-a7d6-6476a76ae868',
#'   'a6ee8177-107b-47dd-bcfd-30960ccc6e9c',
#'   '8f7e5f92-1153-4f93-acba-ca48670c8ca9',
#'   'c0f51e35-d0e8-4318-afaf-c5fcbc29f4c1',
#'   'f33c3cc9-dc4b-4b77-baa9-5a5f10704398',
#'   '2f025389-fac8-4557-94d3-7d0428878c86',
#'   '9598e4cb-0849-401e-bba0-689234b27644'.
#' @param filename supplies an optional output filename and has to be type 
#'   \code{character}.
#' @param \dots additional arguments as for \code{\link[terra]{writeRaster}}.
#' 
#' @return \code{SpatRaster} object with flood duration in the range of 
#'   \code{[0, length(seq)]}.
#' 
#' @details For every time step provided in \code{seq}, \code{flood1()} computes 
#'   a 1d water level using \code{\link[hyd1d]{waterLevelFlood1}} along the 
#'   requested river section. This 1d water level is transfered to a \code{wl} 
#'   (water level) raster layer, which is in fact a copy of the \code{csa} 
#'   (cross section areas) layer, and then compared to the \code{dem} 
#'   (digital elevation model) layer. Where the \code{wl} layer is
#'   higher than the \code{dem}, layer flood duration is increased by 1.
#' 
#' @seealso \code{\link[hyd1d]{df.gauging_data}},
#'   \code{\link[hyd1d]{getGaugingDataW}},
#'   \code{\link[hyd1d]{waterLevelFlood1}},
#'   \code{\link[terra]{writeRaster}}, 
#'   \code{\link[terra]{terraOptions}}
#' 
#' @references 
#'   \insertRef{rosenzweig_inform_2011}{hydflood}
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
#'   fd <- flood1(x = x, seq = seq, gauging_station = "ROSSLAU")
#' }
#' 
#' @export
#' 
flood1 <- function(x, seq, gauging_station, uuid, filename = '', ...) {
    
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
    
    # initialize the WaterLevelDataFrame
    station_int <- na.omit(as.integer(terra::unique(x$csa)$csa))
    wldf_initial <- hyd1d::WaterLevelDataFrame(river = river,
                                               time = as.POSIXct(NA),
                                               station_int = station_int)
    start_f <- min(station_int)/1000
    end_f <- max(station_int)/1000
    
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
    
    ## gauging_station &| uuid
    ##
    # gauging_station &| uuid
    #  get the names of all available gauging_stations
    id <- which(hyd1d::df.gauging_station_data$data_present & 
                    hyd1d::df.gauging_station_data$river == toupper(river))
    df.gauging_station_data_sel <- hyd1d::df.gauging_station_data[id, ]
    gs <- df.gauging_station_data_sel$gauging_station
    uuids <- df.gauging_station_data_sel$uuid
    
    if (missing(gauging_station) & missing(uuid)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'gauging_statio",
                                   "n' or 'uuid' argument has to be supplied."))
    } else {
        if (!(missing(gauging_station))) {
            if (!inherits(gauging_station, "character")) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'gauging_st",
                                           "ation' must be type 'character'."))
            }
            if (length(gauging_station) != 1) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'gauging_st",
                                           "ation' must have length 1."))
                stop(paste0(errors, collapse="\n  "))
            }
            if (!(gauging_station %in% gs)) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'gauging_st",
                                           "ation' must be an element of c('", 
                                           paste0(gs, collapse = "', '"),
                                           "') for the river ", river,
                                           "."))
            } else {
                
                id_gs <- which(gs == gauging_station)
                uuid_internal <- uuids[id_gs]
                df.gs <- df.gauging_station_data_sel[
                    which(uuids == uuid_internal),]
                
                if (df.gs$km_qps < start_f | df.gs$km_qps > end_f) {
                    id <- which(df.gauging_station_data_sel$km_qps > 
                                    start_f & 
                                    df.gauging_station_data_sel$km_qps < end_f)
                    id <- c(min(id) - 1, id, max(id) + 1)
                    gs_possible <- stats::na.omit(
                        df.gauging_station_data_sel$gauging_station[id])
                    if (!(df.gs$gauging_station %in% gs_possible)) {
                        errors <- c(errors, paste0("Error ", l(errors), ": The",
                                                   " selected 'gauging_station",
                                                   "' has to be in the river s",
                                                   "tretch\n  covered by 'x' o",
                                                   "r the next to it up- or do",
                                                   "wnstream.\n  Permitted gau",
                                                   "ging stations for the supp",
                                                   "lied area are:\n    '",
                                                   paste0(gs_possible,
                                                          collapse = 
                                                              "'\n    '"),
                                                   "'"))
                    }
                }
            }
        }
        
        if (!(missing(uuid))) {
            if (!inherits(uuid, "character")) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'uuid' must",
                                           " be type 'character'."))
            }
            if (length(uuid) != 1) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'uuid' must",
                                           " have length 1."))
                stop(paste0(errors, collapse="\n  "))
            }
            if (!(uuid %in% uuids)) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'uuid' must",
                                           " be an element of c('", 
                                           paste0(uuids, collapse = "', '"),
                                           "') for the river ", river, "."))
            } else {
                
                id_uu <- which(uuids == uuid)
                uuid_internal <- uuids[id_uu]
                df.gs <- df.gauging_station_data_sel[
                    which(uuids == uuid_internal),]
                
                if (df.gs$km_qps < start_f | df.gs$km_qps > end_f) {
                    id <- which(df.gauging_station_data_sel$km_qps > 
                                    start_f & 
                                    df.gauging_station_data_sel$km_qps < end_f)
                    id <- c(min(id) - 1, id, max(id) + 1)
                    uuid_possible <- stats::na.omit(
                        df.gauging_station_data_sel$uuid[id])
                    if (!(df.gs$uuid %in% uuid_possible)) {
                        errors <- c(errors, paste0("Error ", l(errors), ": The",
                                                   " selected 'uuid' has to be",
                                                   " in the river stretch\n  c",
                                                   "overed by 'x' or the next ",
                                                   "to it up- or downstream.\n",
                                                   "  Permitted uuid's for the",
                                                   " supplied 'x' are:\n    '",
                                                   paste0(uuid_possible,
                                                          collapse = 
                                                              "'\n    '"),
                                                   "'"))
                    }
                }
            }
        }
        
        if (!(missing(gauging_station)) & !(missing(uuid))) {
            if (id_gs != id_uu) {
                errors <- c(errors, paste0("Error ", l(errors), ": 'gaugin",
                                           "g_station' and 'uuid' must fit",
                                           " to each other.\nThe uuid for ",
                                           "the supplied 'gauging_station'",
                                           " is ", uuids[id_gs], ".\nThe g",
                                           "auging station for the supplie",
                                           "d 'uuid' is ", gs[id_uu], "."))
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
                                            "od::flood1() for the following t",
                                            "emporal sequence with length ",
                                            length(seq), ":"))
    attributes <- append(attributes, strftime(seq, "%Y-%m-%d"))
    out@data@attributes <- attributes
    
    # water level template
    waterlevel <- raster::raster(dem)
    
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
        } else {
            time <- as.POSIXct(i, origin = "1970-01-01 00:00:00")
        }
        wldf <- wldf_initial
        setTime(wldf) <- time
        wldfs[[j]] <- hyd1d::waterLevelFlood1(wldf, uuid = uuid_internal)
        j <- j + 1
    }
    rm(j)
    
    ##
    # raster processing
    # for (j in 1:length(seq)) {
    #     wldf <- wldfs[[j]]
    #     for (a_station in station_int) {
    #         waterlevel[csa == a_station] <- wldf$w[which(wldf$station_int == 
    #                                                      a_station)]
    #     }
    #     out[dem < waterlevel] <- out[dem < waterlevel] + 1
    # }
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
    
    # reintroduce NA's for areas with missing dem and csa data
    # out[is.na(csa)] <- NA
    # out[is.na(dem)] <- NA
    
    return(terra::rast(out))
}

