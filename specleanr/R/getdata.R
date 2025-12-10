
#' @title Download species records from online database.
#'
#' @param data \code{dataframe}, \code{list}, \code{vector}, \code{string}. data to retrieve records from online
#'      databases (GBIF, VertNET, and iNaturalist).
#' @param colsp \code{string}. A variable of species names. Provided if data is a data frame, so not
#'      required for lists and vector.
#' @param extent \code{vector} or \code{sf}. Bounding box to limit the download of records within a particular area. Otherwise all
#'      records from the GBIF will be downloaded. These can be provided in two forms,
#'      either a shapefile \code{(sf)} class accepted or provide a list of named
#'      \code{xmin}, \code{ymin}, \code{xmax}, and \code{ymax} in this particular order.
#' @param db \code{vector}. The different databases allowed including \code{'gbif', 'vertnet', and 'inat'}.
#' @param gbiflim \code{integer}. Limits on the records from the Global Biodiversity Information Platform
#' @param vertlim \code{integer}. Limits on the records from VertNET.
#' @param inatlim \code{integer}. Limits on the records from iNaturalist database.
#' @param warn \code{logical}. To indicate if warning messages should be shown. Default \code{FALSE}.
#' @param verbose \code{logical}. \strong{TRUE} if detailed messages should be indicated and \strong{FALSE}
#'      if download messages are not needed. Default \strong{TRUE}.
#' @param ... More function for species data download can be used.
#'      See \code{rgbif::occ_data} for more information, \code{rinat::get_inat_obs}, and
#'      \code{rvertnet::searchbyterm}.
#' @inheritParams check_names
#'
#'
#' @details
#'        Note always check the validity of the species name with standard database FishBase or World Register of Marine Species.
#'        If the records are more than 50000 in GBIF, and extent can be provide to limit the download.
#'
#' @return Lists of species records from online databases
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' gbdata <- getdata(data = 'Gymnocephalus baloni', gbiflim = 100, inatlim = 100, vertlim = 100)
#'
#' #Get for two species
#' sp_records <- getdata(data=c('Gymnocephalus baloni', 'Hucho hucho'),
#'                             gbiflim = 100,
#'                             inatlim = 100,
#'                             vertlim = 100)
#' #for only two databases
#' sp_records_2db <- getdata(data=c('Gymnocephalus baloni', 'Hucho hucho'),
#'                            db= c('gbif','inat'),
#'                             gbiflim = 100,
#'                             inatlim = 100,
#'                             vertlim = 100)
#'
#' }
#'
#'
getdata <- function(data, colsp = NULL, extent = NULL,
                     db = c("gbif", 'vertnet', 'inat'),
                     gbiflim = 5e4, vertlim = 1e3,
                     inatlim =3e3, verbose= FALSE, warn =FALSE, pct = 80, sn = FALSE, ...){

  if(is(data, 'data.frame') && is.null(colsp)){

    stop('Provide the column list for species in the data to be downloaded.')

  } else if(is(data, 'data.frame') && !is.null(colsp)){

   if(isFALSE(colsp%in%colnames(data))) stop("Species column not found in the dataset.")

    data <- unique(unlist(data[, colsp]))

  }else if(is(data, 'vector') || is(data, 'atomic')){

    data <- unique(data)

  }else if(is(data, 'list')){

    data <- unique(unlist(data))

  } else{
    stop('Data either list or dataframe not provided for download.')
  }
  if(isFALSE(gbiflim%%1==0) | isFALSE(vertlim%%1==0)| isFALSE(inatlim%%1==0)) stop("The vertlim, gblim, or inatlim parameters must be integers.")

  pkgs <- c('curl', 'rgbif', 'rvertnet', 'rinat', 'sf')

  #check if these packages are installed on user computer
  check_packages(pkgs)

  if (!curl::has_internet()) stop('No internet connection, connect and try again later.')

  sppdata <- sapply(data, function(spp){

      checkFB<- check_names(data = spp, verbose = verbose, pct = pct, sn = sn)

      #check if a name is found in the FishBase

      if(is.na(checkFB)) {

        checksppx <- spp

      } else{

        checksppx <- checkFB
      }

    #loop through databases

    sapply(db, FUN = function(xdb) {

      if(xdb=='gbif'){

        #try catch the Error in the HTTP2 framing layer

        ndata <- tryCatch(expr =rgbif::occ_count(scientificName = checksppx),
                       error = function(e){
                         if(grepl(" HTTP2 framing layer", e$message)==TRUE){
                           if(isTRUE(warn))warning("GBIF webpage is misbehvaing and return HTTP2 frame layer message", call. = FALSE)
                           return(NULL)
                         }
                       })

        if(!is.null(ndata)){

          if(ndata==0){

            if(isTRUE(verbose)) message('No records found for ', checksppx, ' in GBIF')

            gbifx <- NULL

          } else if(ndata <= 50000 & is.null(extent)){

            if(gbiflim <= 50000){

              gbifsp <- rgbif::occ_data(scientificName = checksppx, limit = gbiflim)

              if(isTRUE(verbose)) message(nrow(gbifsp$data) ,' records for ', checksppx,' in GBIF were downloaded based on the gbiflimit of ', gbiflim)

              gbifx <- gbifsp$data

            }else{

              gbifsp <- rgbif::occ_data(scientificName = checksppx, limit = ndata)

              if(isTRUE(verbose)) message(nrow(gbifsp$data) ,' records for ', checksppx,' in GBIF were download as they were the maximum records found.')

              gbifx <- gbifsp$data
            }

          }else if (!is.null(extent)){

            extval <- extentvalues(extent, xdb)

            if(gbiflim <= 50000){

              gbifsp <- rgbif:: occ_data(scientificName = checksppx, limit = gbiflim,
                                         decimalLongitude = paste0(extval[1],',' ,extval[3]),
                                         decimalLatitude = paste0(extval[2],',' ,extval[4]), ...)

              if(isTRUE(verbose)) message(nrow(gbifsp$data) ,' records for ', checksppx,' in GBIF were downloaded based on the gbif limit of ', gbiflim)

            }else{

              gbifsp <- rgbif::occ_data(scientificName = checksppx, limit = gbiflim,
                                        decimalLongitude = paste0(extval[1],',' ,extval[3]),
                                        decimalLatitude = paste0(extval[2],',' ,extval[4]), ...)

              if(isTRUE(verbose)) message('All ', nrow(gbifsp$data) ,' records for ', checksppx,' in GBIF were downloaded')
            }

            gbifx <- gbifsp$data

          }else if (ndata>50000 && is.null(extent)){

            if(isTRUE(verbose))message("Only ", gbiflim, " records will be downloaded.")

            gbifsp <- rgbif::occ_data(scientificName = checksppx, limit = gbiflim,...)

            gbifx <- gbifsp$data

          }else{
            gbifx = NULL
          }

          #check if gbif dataset has coordinates decimalLatitude or decimalLongitude among column names

          if(is(gbifx, 'data.frame')){

            if("decimalLatitude"%in%colnames(gbifx) == TRUE){
              gbifx
            }else{
              if(isTRUE(warn)) warning("The data for ", checksppx, " will be removed since no cordinates were found in GBIF database.", call. = FALSE)

              gbifx <- NULL
            }
          }else{
            gbifx = NULL
          }

        }else{
          gbifx <- NULL
        }
      }else if(xdb=='vertnet'){

        sptx <- scan(text = checksppx, what = ' ', quiet = TRUE)

        if(!is.null(extent)) vbbox <- extentvalues(extent, xdb) else vbbox <- NULL #vector of bbox values

        vertx <- tryCatch(rvertnet::searchbyterm(genus= tolower( sptx[1]), specificepithet = tolower(sptx[2]),
                                        limit = vertlim, messages = FALSE, bbox = vbbox),
                 error= function(e){
                   if(grepl('Internal Server Error \\(HTTP 500\\)', e$message)==TRUE | grepl("Service Unavailable \\(HTTP 503\\)", e$message)==TRUE){

                     if(isTRUE(warn))warning("The VertNet database has returned 500 or 503 error message and will be skipped for ",checksppx,".", call. = FALSE)

                     return(NULL)
                   }else{
                     return(NULL)
                   }
                 })

        if(is.null(vertx)){

          if(isTRUE(verbose)) message('No records for ', checksppx, ' in vertnet were found')

          vertxdf <- NULL

        }else{
          vertxdf  <- vertx$data

          if(isTRUE(verbose)) message(nrow(vertxdf), ' records for ', checksppx, ' in vertnet downloaded.')
          vertxdf
        }

        #handle iNaturalist data download using rinat::get_inat_obs
      }else if(xdb=='inat'){

        if(!is.null(extent)) vbbox <- extentvalues(extent, xdb) else vbbox <- NULL #vector of bbox values

        inatx <- tryCatch(
          expr = {
            sx <- rinat::get_inat_obs(taxon_name= checksppx, maxresults = inatlim, bounds = vbbox)
          },
          error= function(e){

            if(isTRUE(verbose)) message('No data exist for species ', checksppx, ' in inaturalist were found.')

            return(0)
          })

        if(length(inatx) >1 ){

          inatx <-  sx

          if(isTRUE(verbose))message(nrow(inatx), ' records for ', checksppx, ' in inaturalist downloaded.')

          inatx
        }else{
          inatx <- NULL
        }
      }else{
        stop('Database name not acceptable. Use only gbif, vertnet, or inat')
      }

    }, simplify = FALSE)

  }, simplify = FALSE)

  #extracting online data

  dfout <- sapply(seq_along(sppdata), function(spno){

    sp <- names(sppdata)[spno]

    splistdb <- sppdata[[sp]]

    len <- sapply(splistdb, length)

    if(all(len==0)==FALSE) {

      if(any(len==0)) splistdb_check <- splistdb[len !=0] else splistdb_check <-  splistdb

      #check if the decimallatitude and decimallongitude exists in vertnet data for match_datasets to run

      if(("vertnet" %in% names(splistdb_check))==TRUE){

        vertcoords <- c("decimallatutide", "decimallongitude")

        if(any(vertcoords%in%sapply(splistdb_check, colnames, simplify = FALSE)[["vertnet"]]) == FALSE){

          if(isTRUE(verbose)) message("vertnet data for ", sp, " has been removed because it lacks the coordinates columns.")

          splistdb_check[["vertnet"]] <- NULL
        } else {
          splistdb_check
        }

      }else{
        splistdb_check
      }

      spdata <- match_datasets(datasets = splistdb_check,
                               lats = c('latitude','decimallatitude'),
                               lons = c('decimallongitude', 'longitude'),
                               species = c('scientificname','scientific_name'),
                               date = c('datetime', 'year','eventdate','dates', 'lastParsed'),
                               country = c('place_guess'))
    }else{
      #handles if no data is returned by all repositories
      if(isTRUE(verbose)) message("The species ", sp, " has been removed because no data was returned from all repositories.")

    }
  }, simplify = FALSE)

  lst <- dfout[!sapply(dfout, is.null)]

  getsharedcols <- Reduce(intersect, lapply(lst, names))

  finalmatch_df <- do.call(rbind, lapply(lst, function(x) x[, getsharedcols]))

  return(finalmatch_df)
}
