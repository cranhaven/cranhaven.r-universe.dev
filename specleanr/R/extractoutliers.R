#' @title Extract outliers for a one species
#'
#' @param x \code{list}. Outlier outputs for both single and multiple species.
#' @param sp \code{string}. Species name or index in the list from datacleaner output. NULL for a single species
#'
#' @return \code{data frame} Outliers for each method
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' data(efidata)
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))
#'
#'
#' extdf <- pred_extract(data = efidata, raster = wcd,
#'                      lat = 'decimalLatitude', lon = 'decimalLongitude',
#'                      colsp = 'scientificName',
#'                      list = TRUE,verbose = FALSE,
#'                    minpts = 6,merge = FALSE)
#'
#' #outlier detection
#'
#' outliersdf <- multidetect(data = extdf, output='outlier', var = 'bio6',
#'                          exclude = c('x','y'), multiple = TRUE,
#'                          methods = c('mixediqr', "iqr", "iqr", "logboxplot"),
#'                          silence_true_errors = FALSE,
#'                         verbose = FALSE, sdm = TRUE)
#'
#' extoutlier <- extractoutliers(x=outliersdf, sp = 3)
#' }
#'
#'
extractoutliers <- function(x, sp = NULL){

  if(!is(x, 'datacleaner')) stop('Only datacleaner class is accpeted')

  if(x@out!='outlier') stop('Only extracts outliers yet clean data has been produced.')

  if(isFALSE(x@mode)){

    dx <- x@result

    checknull <- sapply(dx, nrow) #methods which genuinely failed to implement

    metds <- dx[!sapply(checknull,is.null)]

    dxlists <- sapply(names(metds), function(xx){

      totout <- nrow(metds[[xx]])

      df <- data.frame(method = xx, totaloutliers = totout)

    }, simplify = FALSE)

    dfinal <- do.call(rbind, dxlists)

    row.names(dfinal) <- NULL

    return(dfinal)
  }else{

    if(!is.null(sp)) {

      dx <- x@result[sp]

      if(is.null(unlist(dx))) stop("Either index ", sp, " is out of bounds or the variable was not considered in the outlier detection.", call. = FALSE)

    } else {

      dx <- x@result

    }
    #loop through species
    spdata <- sapply(names(dx), function(spnames){

      spdf <- dx[[spnames]]

      checknull <- sapply(spdf, nrow)

      metds <- spdf[!sapply(checknull,is.null)]

      dxlists <- sapply(names(metds), function(xx){

        totout <- nrow(metds[[xx]])

        df <- data.frame(method = xx, totaloutliers = totout)

      }, simplify = FALSE)

      dfout <- do.call(rbind, dxlists)

      dfout["groups"] <- spnames

      return(dfout)

    }, simplify = FALSE)

    dfinal <- do.call(rbind, spdata)

    row.names(dfinal) <- NULL

  }
  return(dfinal)
}
