#' @name classifyToPNV
#' @rdname classifyToPNV
#' 
#' @title Function to reclassify flood durations to potential natural vegetation
#' 
#' @description This function is a wrapper to the function
#'   \code{\link[terra]{classify}} to convert flood durations computed with
#'   \code{\link{flood3}} into potential natural vegetation (PNV) distributions using
#'   reclassification rules supplied with \code{\link{df.pnv}}. Alternative
#'   reclassification rules may be applied, but they must match column names and
#'   types as given by \code{\link{df.pnv}}. \code{\link[terra]{classify}} is
#'   called with \code{include.lowest = TRUE} and \code{right = FALSE}.
#' 
#' @param x argument of type \code{\link[terra]{SpatRaster}}.
#' @param rcl optional argument of type \code{data.frame} with columns and
#'    column types as specified in \code{\link{df.pnv}}.
#' @param filename supplies an optional output filename of type
#'   \code{character}.
#' @param \dots additional arguments as for \code{\link[terra]{writeRaster}}.
#' 
#' @return \code{\link[terra]{SpatRaster}} object containing potential natural
#'   vegetation distribution as categorical raster.
#' 
#' @seealso \code{\link{df.pnv}}
#' 
#' @references 
#'   \insertRef{ochs_potential_2020}{hydflood}
#' 
#' @examples \donttest{
#'   cache <- tempdir()
#'   options("hyd1d.datadir" = cache)
#'   options("hydflood.datadir" = cache)
#'   options(timeout = 200)
#'   library(hydflood)
#'   
#'   # import the raster data and create a raster stack
#'   c <- st_crs("EPSG:25833")
#'   e <- ext(309000, 309300, 5749000, 5749300)
#'   x <- hydSpatRaster(ext = e, crs = c)
#'   
#'   # create a temporal sequence
#'   seq <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day")
#'   
#'   # compute a flood duration
#'   fd <- flood3(x = x, seq = seq)
#'   
#'   # reclassify to PNV
#'   pnv <- classifyToPNV(fd)
#'   
#'   # plot pnv map
#'   plot(pnv)
#' }
#' 
#' @export
#' 
classifyToPNV <- function(x, rcl = NULL, filename = "", ...) {
    
    #####
    # validate the input data
    ##
    # vector and function to catch error messages
    errors <- character()
    l <- function(errors) {as.character(length(errors) + 1)}
    
    ##
    # x
    if (!inherits(x, "SpatRaster")) {
        errors <- c(errors, paste0("Error ", l(errors), ": 'x' must be type 'S",
                                   "patRaster'."))
    }
    if (length(x) != 1) {
        errors <- c(errors, paste0("Error ", l(errors), ": 'length(x)' must be",
                                   " 1."))
    }
    if (minmax(x)[1,] != 0) {
        errors <- c(errors, paste0("Error ", l(errors), ": 'minmax(x)[1,]' mu",
                                   "st be 0."))
    }
    if (minmax(x)[2,] > 366 & minmax(x)[2,] < 365) {
        warning("'minmax(x)[2,]' should be between 365 or 366 (a full year).")
    }
    if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
    
    ##
    # rcl
    if (!is.null(rcl)) {
        if (!inherits(rcl, "data.frame")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'rcl' must be t",
                                       "ype 'data.frame'."))
        }
        if (!"from" %in% names(rcl)) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'names(rcl)' mu",
                                       "st contain 'from'."))
        }
        if (!"to" %in% names(rcl)) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'names(rcl)' mu",
                                       "st contain 'to'."))
        }
        if (!"class" %in% names(rcl)) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'names(rcl)' mu",
                                       "st contain 'class'."))
        }
        if (!"vegtype" %in% names(rcl)) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'names(rcl)' mu",
                                       "st contain 'vegtype'."))
        }
        if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
        
        if (!inherits(rcl$from, "numeric") & !inherits(rcl$from, "integer")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'class(rcl$from",
                                       ")' must be either 'numeric' or 'intege",
                                       "r'."))
        }
        if (!inherits(rcl$to, "numeric") & !inherits(rcl$to, "integer")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'class(rcl$to",
                                       ")' must be either 'numeric' or 'intege",
                                       "r'."))
        }
        if (!inherits(rcl$class, "integer")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'class(rcl$clas",
                                       "s)' must be 'integer'."))
        }
        if (!inherits(rcl$vegtype, "character")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'class(rcl$vegt",
                                       "ype)' must be 'character'."))
        }
        if (l(errors) != "1") {stop(paste0(errors, collapse="\n  "))}
        
        if (!all(sapply(list(order(rcl$to), order(rcl$class)),
                        FUN = identical, order(rcl$from)))) {
            errors <- c(errors, paste0("Error ", l(errors), ": The columns fro",
                                       "m, to and class must have the same ord",
                                       "er."))
        }
    } else {
        rcl <- hydflood::df.pnv
    }
    
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
    
    # reclassify
    rcl <- rcl[order(rcl$class), ]
    r <- rcl[, c("from", "to", "class")]
    
    res <- terra::classify(x, r, include.lowest = TRUE, right = FALSE)
    terra::set.cats(res, value = rcl[, c("class", "vegtype")])
    names(res) <- "PNV"
    
    # append colortab
    cols <- grDevices::rgb(df.pnv$r, df.pnv$g, df.pnv$b, 255,
                           names = df.pnv$vegtype, maxColorValue = 255)
    terra::coltab(res) <- data.frame(values = 1:8, cols = cols)
    
    if (filename != "") {
        terra::writeRaster(res, filename = filename, ...)
    }
    
    return(res)
}
