#' @name createTiles
#' @rdname createTiles
#' 
#' @title Function to split large areas (\code{sfc_POLYGON}) into
#'   tiles
#' 
#' @description To simplify and accelerate the computation of flood duration
#'   with \code{\link{flood3}} in massive areas this function provides a simple
#'   tiling algorithm.
#' 
#' @param x has to by type \code{sf}.
#' @param size_x tile size along the x-axis in the units of the current projection (\code{numeric}).
#' @param size_y tile size along the y-axis in the units of the current projection (\code{numeric}).
#' @param subset \code{boolean} determining whether all or only intersecting tiles are returned.
#' 
#' @return \code{sf} object containing tiles covering \code{x}.
#' 
#' @examples
#'   options("hydflood.datadir" = tempdir())
#'   library(hydflood)
#'   tiles <- createTiles(x = sf.af(name = "Elbe"),
#'                        size_x = 10000, size_y = 10000)
#'   plot(tiles["tile_ID"])
#' 
#' @export
#' 
createTiles <- function(x, size_x, size_y, subset = TRUE) {
    
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
        if (! inherits(x, "sf")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'x' must be typ",
                                       "e 'sf'."))
        }
    }
    
    ## size_x
    if (missing(size_x)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'size_x' ",
                                   "argument has to be supplied."))
    } else {
        # class
        if (! inherits(size_x, "numeric")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'size_x' must b",
                                       "e type 'numeric'."))
        }
    }
    
    ## size_y
    if (missing(size_y)) {
        errors <- c(errors, paste0("Error ", l(errors), ": The 'size_y' ",
                                   "argument has to be supplied."))
    } else {
        # class
        if (! inherits(size_y, "numeric")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'size_y' must b",
                                       "e type 'numeric'."))
        }
    }
    
    if (!missing(subset)) {
        if (!inherits(subset, "logical")) {
            errors <- c(errors, paste0("Error ", l(errors), ": 'subset' must b",
                                   "e type 'logical'."))
        }
    }
    
    #####
    # error messages
    if (l(errors) != "1") {
        stop(paste0(errors, collapse="\n  "))
    }
    
    #####
    # processing
    # calculate the number of columns along the x-axis and the number of rows 
    # along the y-axis
    x_min <- floor(sf::st_bbox(x)$xmin / size_x) * size_x
    y_min <- floor(sf::st_bbox(x)$ymin / size_y) * size_y
    
    n_col_x <- ceiling((ceiling(sf::st_bbox(x)$xmax) - x_min) / size_x)
    n_row_y <- ceiling((ceiling(sf::st_bbox(x)$ymax) - y_min) / size_y)
    n_zfill <- max(nchar(as.character(n_col_x)), nchar(as.character(n_row_y)))
    
    # calculate the coordinates of the upper right corner of the extent
    x_max <- x_min + n_col_x * size_x
    y_max <- y_min + n_row_y * size_y
    
    # create "tiles"
    l.res <- list()
    df.res <- data.frame(tile_ID = character(n_col_x * n_row_y),
                         x_column = numeric(n_col_x * n_row_y),
                         y_row = numeric(n_col_x * n_row_y),
                         stringsAsFactors = FALSE)
    k <- 1
    for (i in 1:n_row_y) {
        for (j in 1:n_col_x) {
            l.res[[k]] <- sf::st_polygon(
                list(matrix(c(x_min + (j - 1) * size_x,
                              x_min + size_x + (j - 1) * size_x,
                              x_min + size_x + (j - 1) * size_x,
                              x_min + (j - 1) * size_x,
                              x_min + (j - 1) * size_x,
                              y_max - (i - 1) * size_y,
                              y_max - (i - 1) * size_y,
                              y_max - size_y - (i - 1) * size_y,
                              y_max - size_y - (i - 1) * size_y,
                              y_max - (i - 1) * size_y),
                              ncol = 2)))
            df.res$tile_ID[k] <- paste0(sprintf(paste0("%0", n_zfill, "d"), j),
                                        "_",
                                        sprintf(paste0("%0", n_zfill, "d"), i))
            df.res$x_column[k] <- j
            df.res$y_row[k] <- i
            k <- k + 1
        }
    }
    
    tiles <- sf::st_sfc(l.res, crs = sf::st_crs(x))
    tiles <- sf::st_sf(tiles, df.res)
    
    if (subset) {
        tiles <- tiles[x,]
    }
    
    return(tiles)
}
