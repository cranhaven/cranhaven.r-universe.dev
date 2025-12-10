#' @title Identifies the best method for outlier detection for a single species.
#'
#' @param x List of dataframes for each methods used to identify outliers in \strong{\code{multdetect}} function.
#' @param sp species name or index if multiple species are considered during outlier detection.
#' @param threshold Maximum value to denote an absolute outlier. The threshold ranges from \code{0} which indicates a point has not been flagged by any outlier detection method
#'        as an \code{outlier} or \code{1}, when means the record is an absolute or true outlier sicen it has been identified by all methods. At both extremes, at low threshold values,
#'        many records are classified, which may be due to individual method weakness or strength and data distribution. Also, at higher threshold values, the true outliers are retained
#'        Fo example, if 10 methods are considered and 9 methods flags a record as an outlier, If a cut off 1 is used, then that particular record is retained.
#'        Therefore the \code{default} cutoff is 0.6 but \code{autothreshold} can be used to select the appropriate threshold.
#' @param autothreshold Identifies the threshold with mean number of absolute outliers.The search is limited within 0.51 to 1 since thresholds less than
#'        are deemed inappropriate for identifying absolute outliers. The autothreshold is used when \code{threshold} is set to \code{NULL}.
#' @param verbose if \code{TRUE} then messages and warnings will be produced. Default \code{FALSE}.
#' @param warn If \strong{\code{TRUE}}, warning on whether absolute outliers obtained at a low threshold is indicated. Default \strong{\code{TRUE}}.
#'
#' @return best method for identifying outliers.
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#'
#' data("efidata")
#' data("jdsdata")
#'
#' matchdata <- match_datasets(datasets = list(jds = jdsdata, efi=efidata),
#'                            lats = 'lat',
#'                            lons = 'lon',
#'                            species = c('speciesname','scientificName'),
#'                            date = c('Date', 'sampling_date'),
#'                            country = c('JDS4_site_ID'))
#'
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' rdata <- pred_extract(data = matchdata,
#'                      raster= worldclim ,
#'                      lat = 'decimalLatitude',
#'                      lon= 'decimalLongitude',
#'                      colsp = 'species',
#'                      bbox = db,
#'                      minpts = 10,
#'                      list=TRUE,
#'                     merge=FALSE)
#'
#'
#'out_df <- multidetect(data = rdata, multiple = TRUE,
#'                      var = 'bio6',
#'                      output = 'outlier',
#'                      exclude = c('x','y'),
#'                      methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel', 'kmeans',
#'                                  'logboxplot', 'lof','iforest', 'mahal', 'seqfences'))
#'
#'bmout <- bestmethod(x = out_df, sp= 1, threshold = 0.2)
#'}

bestmethod <- function(x, sp = NULL, threshold= NULL, autothreshold=FALSE,
                       warn=FALSE, verbose=FALSE){

  if(missing(x)) stop('List of species data with outliers is not provided.')

  if(!is(x, 'datacleaner')) stop('Only datacleaner class accepeted.')

  if(x@out!='outlier') stop('Only extracts outliers yet clean data has been produced.')

  if(!is.null(threshold)) if(threshold>1 | threshold<0) stop('threshold must range from 0 to 1.')

  simethods <- c('jaccard', 'overlap','sorensen', 'hamming','ocindex', 'smc', 'cosine')

  simoutput <- sapply(simethods, function(xi){

    if(xi=='overlap'){

      overlap(x= x, sp = sp,  threshold = threshold, warn = warn,
              autothreshold = autothreshold)
    }else if(xi == 'jaccard'){

      jaccard(x= x, sp = sp,  threshold = threshold, warn = warn,
              autothreshold = autothreshold)

    }else if(xi == 'sorensen'){

      sorensen(x= x, sp = sp, threshold = threshold, warn = warn,
               autothreshold = autothreshold)

    }else if(xi == 'smc'){

      smc(x= x, sp =sp,  threshold = threshold, warn = warn,
          autothreshold = autothreshold)

    }else if(xi == 'cosine'){

      cosine(x= x, sp =sp, threshold = threshold, warn = warn,
             autothreshold = autothreshold)

    }else if(xi == 'hamming'){

      hamming(x= x, sp =sp,  threshold = threshold, warn = warn,
              autothreshold = autothreshold)

    }else{

      ocindex(x= x, sp = sp, threshold = threshold, warn = warn,
              autothreshold = autothreshold)
    }

  })

  freqm <- table(simoutput) == max(table(simoutput))

  #select for the method with TRUE meaning maximum selection or frequency

  bstmethod <- names(freqm[which(freqm==TRUE)])

  if(length(bstmethod)==1){

    bstfinal <- as.character(bstmethod)

    if(isTRUE(verbose)) message('Criteria: Best method selected with majority votes of ', max(table(simoutput)), '.')

  } else if(length(bstmethod)>1){

    #obtain the method identified by ocindex to be used as a standard in case its among the maximum

    ocimethodflag <- simoutput[which(names(simoutput)=="ocindex")]

    #get methods with maximum votes

    maxmethods <- freqm[which(freqm==TRUE)]

    checkbest <- names(maxmethods)%in%ocimethodflag

    if(any(checkbest)==TRUE) {

      bstfinal = names(maxmethods)[which(checkbest==TRUE)]

      if(isTRUE(verbose)) message('Criteria: Method with maximum number of votes and similar to OCI is selected.')

    }else{
      bstfinal = names(maxmethods)[1]

      if(isTRUE(verbose)) message('Criteria: One of the method with maximum number of votes selected.')
    }
  }else{
    stop("No method identified and possibly no absolute outliers identified.")
  }

  return(bstfinal)
}


#' @title Identify best method for outlier removal for multiple species using majority votes.
#'
#' @param x Output from the outlier detection.
#' @param threshold value to consider whether the outlier is an absolute outlier or not.
#' @param autothreshold Identifies the threshold with mean number of absolute outliers.The search is limited within 0.51 to 1 since thresholds less than
#'        are deemed inappropriate for identifying absolute outliers. The autothreshold is used when \code{threshold} is set to \code{NULL}.
#' @param verbose Produce messages on the process or not. Default \strong{FALSE}.
#'
#' @param warn If \strong{TRUE}, warning on whether absolute outliers obtained at a low threshold is indicated. Default \strong{FALSE}.
#'
#' @return best method for outlier detection for each species
#' @export
#'
#' @examples
#' \donttest{
#'
# data(efidata)
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#' wcd <- terra::rast(system.file('extdata/worldclim.tiff', package = "specleanr"))
#'
#' preddata <- pred_extract(data = efidata, raster = wcd,
#'                       lat = 'decimalLatitude', lon = 'decimalLongitude',
#'                      colsp = 'scientificName',
#'                      list = TRUE,verbose = FALSE,
#'                      minpts = 6,merge = FALSE)#'basin removed
#'
#'  #outlier detection
#'
#' outliersdf <- multidetect(data = preddata, multiple = TRUE,
#'                       var = 'bio6',
#'                       output = 'outlier',
#'                       exclude = c('x','y'),
#'                       methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel', 'kmeans',
#'                                   'logboxplot', 'lof','iforest', 'mahal', 'seqfences'))
#'
#' multbm <- multibestmethod(x = outliersdf, threshold = 0.2)#
#'}
#'

multibestmethod<- function(x, threshold = NULL, warn=FALSE, verbose=FALSE, autothreshold = FALSE){

  if(missing(x)) stop('List of species data with outliers is not provided')

  if(!is(x, 'datacleaner')) stop('Only datacleaner class accepted')

  if(x@out!='outlier') stop('Only extracts outliers yet clean data has been produced.')

  if(!is.null(threshold)) if(threshold>1 | threshold<0) stop('threshold must range from 0 to 1.', call. = FALSE)

  if(x@mode==FALSE) stop("Use bestmethod function to identify the best outlier detection method for a single species.")

  metout <- c()

  species <- c()
  for (dii in seq_along(x@result)) {

    spnames <- names(x@result)[dii]

    bstout <- tryCatch(
      expr = bestmethod(x= x, sp = dii, threshold = threshold, warn = warn, autothreshold = autothreshold,
                        verbose = verbose),
      error= function(e){

        if(isTRUE(verbose))message('No absolute outliers exist for ', names(x@result)[dii], '.')

        return("None")
      })

    if(length(bstout)>=1){

      metout[dii] <- bstout

    }else{
      metout[dii] <- 'None'
    }
    species[dii] <- spnames

  }

  return(data.frame(species = species, bestm = metout))
}
