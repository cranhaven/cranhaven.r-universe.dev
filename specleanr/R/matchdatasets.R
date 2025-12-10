
#When standard column names are NULL
#' @noRd
ifnull <- function(x, y, std) {

  if(is.null(y)){

    #supply column standard names

    cls <- sapply(sapply(x, colnames), intersect, std)

    ck <- sapply(cls, length)

    if((all(ck)>0)==FALSE){

      dt <- names(ck)[which(ck==0)]

      stop('Datasets ', unlist(dt),' dont have standard columns for ', std , ' and yet you have left it NULL.')
    }
  }else{
    lst <-  x
  }
  return(lst)
}

#Data harmonizing for offline data based on Darwin Core terms.


#' @title Data harmonizing for offline data based on Darwin Core terms .
#'
#' @param datasets List of offline or online data to be merge. Each offline data set should be given a specific
#' name for identification in the match data set.
#' @param lats Match the column names for latitude for each data set to be matched.
#' The default latitude name is \strong{decimalLatitude}.
#' So, indicate the latitude name as it is referenced in all data sets to be matched.
#' @param lons Match the column names for latitude for each data set to be match.
#' The default longitude name is \strong{decimalLongitude}.
#' So, indicate the longitude name as it is referenced in all data sets to be match.
#' @param species Indicate the species columns as they appear in the data sets to be matched.
#' The default is \strong{species}, so if the data set doesn't have species as the column name
#' for scientific species names names, indicate the column name here.
#' @param date Indicate the \strong{date} column names as they appear in the data sets to be matched.
#' @param country Indicate the \strong{country} column names as they appear in the data sets to be merged.
#' @param verbose Messages during data matching. Default \strong{FALSE}
#'
#' @details
#' If a data set being matched has standard columns, namely decimalLatitude, decimalLatutide,
#' and species, then they are not indicated while matching. Otherwise all column names with
#' varying names for the 5 parameters should be indicated.
#'
#' @return Harmonized data set with standardized column names foe species names, latitude, longitude, country and dates.
#'
#' @export
#'
#' @examples
#'
#' data(jdsdata)
#'
#' data(efidata)
#'
#' matchdfs <- match_datasets(datasets = list(jds = jdsdata, efi = efidata),
#'                            lats = 'lat',
#'                            lons = 'lon',
#'                            species = c('speciesname','scientificName'),
#'                            country=c('JDS4_site_ID'),
#'                            date=c('Date', 'sampling_date'))
#'
#' @references
#' Wieczorek J, Bloom D, Guralnick R, Blum S, Döring M, Giovanni R, et al. (2012)
#' Darwin Core: An Evolving Community-Developed Biodiversity Data Standard. PLoS ONE 7(1):
#' e29715. https://doi.org/10.1371/journal.pone.0029715.
#'
#'
match_datasets <- function(datasets, country = NULL, lats=NULL, lons=NULL, species = NULL,
                           date=NULL, verbose = FALSE){

  if(!is(datasets, 'list'))  stop('Only a named list of datasets should be provided.')

  #check if null columns in the different parameters have standard names across all data sets.

  ifnull(x=datasets, y=country, std = 'country')

  ifnull(x=datasets, y=lats, std='decimalLatitude')

  ifnull(x=datasets, y=lons, std='decimalLongitude')

  ifnull(x=datasets, y=species, std='species')

  datalists <- list()

  #standardize column names for main 4 columns (species, latitude, longitude, country, and dates columns)

  xsdata <- sapply(seq_along(datasets), function(dnum){

    datnames <- names(datasets)[dnum]

    dataset <- datasets[[dnum]]

    if(is.null(dataset) || nrow(dataset)<0){

      stop('No records for species in dataset')
    }else{
      if('decimalLatitude'%in%colnames(dataset)){

        dataset
      }else{
        latitude <- intersect(lats, colnames(dataset))

        if(length(latitude)==0)stop('check if latitude options provided  are in dataset ', datnames )

        colnames(dataset)[colnames(dataset)==latitude] <- 'decimalLatitude'
      }
      if('decimalLongitude'%in%colnames(dataset)){

        dataset
      }else{
        longitude <- intersect(lons,colnames(dataset))

        if(length(longitude)==0) stop('check if longitude options provided  are in dataset ', datnames )

        colnames(dataset)[colnames(dataset) == longitude] <- 'decimalLongitude'
      }

      if('species'%in%colnames(dataset)){
        dataset
      }else{
        spp <- intersect(species, colnames(dataset))

        if(length(spp)==0)stop('check if species options provided  are in dataset ', datnames )

        colnames(dataset)[colnames(dataset)==spp] <- 'species'
      }
      if('country'%in%colnames(dataset)){
        dataset
      }else{
        countryname <- intersect(country, colnames(dataset))

        if(length(countryname)==0)stop('check if species options provided  are in dataset ', datnames )

        colnames(dataset)[colnames(dataset)==countryname] <- 'country'
      }

      if(!is.null(date)){

        dates <- intersect(date, colnames(dataset))

        if(length(dates)==0){

          stop('check if date options provided  are in dataset ', datnames)

        }else if(length(dates)>1) {

          dates2<- dates[1]

          if(isTRUE(verbose))message('Dataset ', datnames, ' has more than column options indicated for date standardisation.')

           }else{
          dates2 <- dates
        }

        colnames(dataset)[colnames(dataset)==dates2] <- 'dates'
      }

      if(!'quality_grade'%in%colnames(dataset)){

        dataset[, 'quality_grade'] <- 'expert based' #for local data
      }
      dataset[,'datasetname'] <- datnames
      dataset[, 'decimalLatitude'] <- as.numeric(dataset$decimalLatitude)
      dataset[, 'decimalLongitude'] <- as.numeric(dataset$decimalLongitude)
      dataset
    }
  }, simplify= FALSE)

  if(length(xsdata)>1){

    ident_colnames <- Reduce(intersect, sapply(xsdata, colnames))

    datafinal <- do.call(rbind, lapply(xsdata, function(x) x[, ident_colnames]))
  }else{
    dfinal <- do.call(rbind, xsdata)

    if(is(dfinal, 'data.frame')){

      stdcols <- c("country", "species", 'decimalLatitude',
                "decimalLongitude", 'dates',"quality_grade", "datasetname")

      dfcols<- colnames(dfinal)

      colsin <- dfcols[dfcols %in% stdcols ==TRUE]

      datafinal <- dfinal[, colsin]
    }
  }
  return(datafinal)
}





