#' @noRd
#'
cleandata <- function(data, outliers,
                           sp = NULL,
                           mode = 'best', threshold = NULL,
                           var_col = NULL,
                           warn = FALSE, verbose = FALSE,
                           autothreshold = FALSE,
                           pabs = 0.1, loess = FALSE, cutoff = 0.6 ){
  varc <- outliers@varused

  if(outliers@mode==FALSE){
    var <- varc
  }else{
    if(length(varc)>1) var <- sp else var <- varc
  }

  if(isTRUE(loess)){
    loess = TRUE
    optthreshold <- search_threshold(data = data, sp = sp, outliers = outliers,
                                     warn = warn,
                                     verbose = verbose, cutoff = cutoff)
    if(!is.null(optthreshold)){

      maxima = unname(optthreshold[2])
      #change maxima to 0.8 if less than 0.8
      if(maxima<0.8) threshold <- 0.8 else threshold <- maxima
    }else{
      #return error and capture it during data retrieval
      stop('The threshold could not be found because the data is not enough for varialble ', sp)
    }

  }else if(!is.null(threshold)){
    threshold
  }else{
    autothreshold = TRUE
    threshold = NULL
    loess = FALSE
  }
  if(mode =='best'){

    bs <- bestmethod(x= outliers,
                     threshold =  threshold,
                     warn = warn,
                     sp = sp,
                     verbose = verbose,
                     autothreshold = autothreshold)

      if(outliers@mode==FALSE) datOut <- outliers@result[[bs]][[var]] else datOut <- outliers@result[[sp]][[bs]][[var]]

  }else{

    datOut <- ocindex(x= outliers,
                      absolute = TRUE,
                      threshold = threshold,
                      sp = sp,
                      warn = warn,
                      autothreshold = autothreshold)
  }
  varc <- unlist(data[, var])

  indx <- which(!varc%in%datOut)

  pctabs <- (length(varc)-length(indx))/length(varc)

  if(pctabs >= pabs && mode=='best'){

    datOut <- ocindex(x= outliers, absolute = TRUE,
                      sp = sp,
                      threshold = threshold, warn = warn,
                      autothreshold = autothreshold)

    if(isTRUE(verbose)) message('the number of rows removed exceed ', pabs, ', so only absolute outliers removed.' )

    indx <- which(!varc %in% datOut)

    datIn <- data[indx,]

  }else{
    datIn <- data[indx,]
  }
  return(datIn)
}


#' @title Extract final clean data using either absolute or best method generated outliers.
#'
#' @inheritParams search_threshold
#' @param refdata \code{dataframe}. The reference data for the species used in outlier detection.
#' @param outliers \code{string}. Output from the outlier detection process.
#' @param mode \code{character}. Either \code{abs} to use absolute outliers to filter data or \code{best} to outliers from best method.
#' @param threshold \code{numeric}. Value to consider whether the outlier is an absolute outlier or not.
#' @param autothreshold \code{vector}. Identifies the threshold with mean number of absolute outliers.The search is limited within 0.51 to 1 since thresholds less than
#'        are deemed inappropriate for identifying absolute outliers. The autothreshold is used when \code{threshold} is set to \code{NULL}.
#' @param var_col \code{string}. A parameter to be used if the \code{data} is a data frame and the user must indicate the column wih species names.
#' @param warn \code{logical}. If \strong{FALSE}, warning on whether absolute outliers obtained at a low threshold is indicated. Default \strong{TRUE}.
#' @param pabs \code{numeric}. Percentage of outliers allowed to be extracted from the data. If \code{best} is used to extract outliers and the \code{pabs} is exceeded,
#'      the absolute outliers are removed instead. This because some records  in the best methods are repeated and they will likely to remove true values as outliers.
#' @param verbose \code{logical}. Produces messages or not. Default \strong{FALSE}.
#' @param loess \code{logical}. Set to \code{TRUE} to use loess threshold optimization to extract clean data.
#' @param outlier_to_NA \code{logical} If \code{TRUE} a clean dataset will have outliers replaced with NAs.
#'        This parameter is experimented to ouput dataframe when multiple variables of concerns are considered
#'        during outlier detection.
#'
#' ###param multiple TRUE for multiple species and FALSE for single species considered during outlier detection.
#'
#' @return Either a \code{list} or \code{dataframe} of cleaned records for multiple species.
#'
#' @seealso \code{\link{search_threshold}}
#'
#' @examples
#'\donttest{
#' data(jdsdata)
#' data(efidata)
#' matchdata <- match_datasets(datasets = list(jds = jdsdata, efi = efidata),
#'                             lats = 'lat',
#'                             lons = 'lon',
#'                             species = c('speciesname','scientificName'),
#'                             country= c('JDS4_site_ID'),
#'                             date=c('sampling_date', 'Date'))
#'
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' db <- sf::st_read(danube, quiet=TRUE)
#'
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' rdata <- pred_extract(data = matchdata,
#'                       raster= worldclim ,
#'                       lat = 'decimalLatitude',
#'                       lon= 'decimalLongitude',
#'                       colsp = 'species',
#'                       bbox = db,
#'                       minpts = 10,
#'                       list=TRUE,
#'                       merge=FALSE)
#'
#'
#' out_df <- multidetect(data = rdata, multiple = TRUE,
#'                       var = 'bio6',
#'                       output = 'outlier',
#'                       exclude = c('x','y'),
#'                       methods = c('zscore', 'adjbox','iqr', 'semiqr','hampel'))
#'
#' #extracting use the absolute method for one species
#'
#' extractabs <- extract_clean_data(refdata = rdata, outliers = out_df,
#'                                  mode = 'abs', threshold = 0.6,
#'                                  autothreshold = FALSE)
#'
#' bestmout_bm <- extract_clean_data(refdata = rdata, outliers = out_df,
#'                                   mode = 'best', threshold = 0.6,
#'                                  autothreshold = FALSE)
#'}
#'
#' @export
#'

extract_clean_data <- function(refdata, outliers, mode ='abs',var_col = NULL,
                               threshold =NULL, warn=FALSE, verbose=FALSE,
                               autothreshold =FALSE, pabs = 0.1, loess = FALSE,
                               outlier_to_NA  = FALSE,
                               cutoff = 0.6){

  #the allowed modes: best for best method and abs : extract out only absolute outliers.
  match.argc(mode, choices = c('best', 'abs'))

  if(deparse(substitute(refdata))!= outliers@dfname)stop('The reference dataset used in outlier detection and the output of outlier detection are different.')

  #handle multiple variables especially for general data not species distribution models.
  var <- outliers@varused

  if(isTRUE(outlier_to_NA) && length(var)>1){

    if(is.null(threshold))stop("For this aspect only setting threshold is allowed. Set threshold to 0.8 or above.")

    for (ivar in var) {
      #try catch to handle parameters with no outliers
      absout <- tryCatch(expr =  {if(mode=='abs'){
        ocindex(x=outliers, sp = ivar, absolute = TRUE,
                threshold = threshold,
                warn = warn)
      }else{
        metd <- bestmethod(x= outliers,
                         threshold =  threshold,
                         warn = warn,
                         sp = ivar,
                         verbose = verbose,
                         autothreshold = autothreshold)

         datOut <- outliers@result[[ivar]][[metd]][[ivar]]

      } }, error = function(e)return(NULL))

      if(!is.null(absout)){

        vals <- unlist(refdata[,ivar])

        datOut <- which(vals %in% absout)

        refdata[,ivar][datOut] <- NA

        dfcleaned <- refdata
      }else{
        message('No absolute outliers for ',ivar , ' at the indicated threshold of ', threshold)
      }
    }
  }else{

    #for a single species: clean data extraction

    if(outliers@mode==FALSE){

      splist <- list(data = refdata)

    }else{

      if(is(refdata, 'list')){

        if(length(refdata)!= length(outliers@result)) stop('Number of species in refdata and outlier detection are not equal')

        splist <- refdata

      } else if(is(refdata, 'data.frame')){

        if(length(outliers@varused)>1){

          vars <- outliers@varused

          splist <- sapply(vars, function(x) x <-  refdata, simplify = FALSE)

        }else{

          if(is.null(var_col)) stop('Provide the column with species names in parameter, var_col .')

          splist <- split(refdata, f= refdata[,var_col])

          if(length(splist)!= length(outliers@result)) stop('Number of species in data and outlier detection are not equal')
        }

      }else{
        stop('Only list and dataframes accepted.')
      }
    }


    dfdata <- sapply(names(splist), function(fd){

      if(isFALSE(outliers@mode)) spnames <- NULL else spnames <- fd

      df_out <- tryCatch(cleandata(data = splist[[fd]], outliers = outliers,
                                        sp = spnames,
                                        mode = mode, threshold = threshold,
                                        var_col = var_col,
                                        warn = warn, verbose = verbose,
                                        autothreshold = autothreshold,
                                        pabs = pabs, loess = loess, cutoff = cutoff),
                        error=function(e){
                          if(grepl('The threshold could not be found because', e$message)==TRUE){

                           if(verbose==TRUE) message('The threshold could not be found because the data is not enough for variable ', spnames)

                            return(NULL)
                          }else{
                          return(NULL)
                        }}
                         )
      if(nrow(df_out)==0 || is.null(df_out)) cdata <- NULL else cdata <- df_out

      if(!is.null(cdata)) spdata <- cdata else spdata <- splist[[fd]]

        if(outliers@mode==FALSE) spdata else spdata['groups'] <- fd

        spdata
    }, simplify = FALSE)

    dfcleaned <- do.call(rbind, dfdata)

    rownames(dfcleaned) <- NULL
  }
  return(dfcleaned)
}
