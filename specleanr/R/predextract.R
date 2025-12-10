
#' @title  Preliminary data cleaning including removing duplicates, records
#'   outside a particular basin, and NAs.
#'
#' @param data \code{dataframe}. Data frame with multiple species or only one
#'   species for checking records with no coordinates, duplicates, and check for
#'   records that fall on land, sea, country or city centroids, and geographical
#'   outliers(Zzika et al., 2022).
#' @param raster \code{raster}. Environmental layers from different providers
#'   such as WORLDCLIM (), Hydrogaphy90m (), CHELSA, Copernicus ().
#' @param lat,lon \code{coordinates}. variable for latitude and longitude column
#'   names.
#' @param colsp \code{string}. variable already in the data that determine the groups to
#'        considered when extracting data.
#' @param mp \code{logical}. If \code{TRUE}, then number of minimum records \code{minpts} should be provided to allow dropping groups
#'        with less records. This is significant if species distribution are going to be fitted.
#' @param minpts \code{numeric}. Minimum number of records for the species after
#'   removing duplicates and those within a particular basin.
#' @param rm_duplicates \code{logical} TRUE if the duplicates will removed based species coordinates and names. Default \code{TRUE}.
#' @param list \code{logical}. If TRUE the a list of multiple species data frames will be
#'   generated and FALSE for a dataframe of species data sets. Default TRUE
#' @param bbox \code{sf} or \code{vector}. Object of class 'shapefile' If only a particular basin is
#'   considered. Bounding box vector points can also be provided in the form
#'   \code{"c(xmin, ymin, xmax, ymax)"}. \code{xmin} is the minimum longitude,
#'   \code{ymin} is the minimum latitude, \code{xmax} is the maximum longitude
#'   and \code{xmax} is the minimum latitude.
#' @param verbose \code{logical}. if TRUE message and warnings will be produced. Default \code{TRUE}.
#' @param warn \code{logical}. indicating to whether to show implementation warning or
#'   not. Default \code{FALSE}.
#' @param merge \code{logical}. To add the other columns in the species data after data
#'   extraction. Default \strong{TRUE}.
#' @param na.rm \code{logical} If TRUE, the missing values will be discarded after data extracted.
#' DEFAULT TRUE.
#' @param na.inform \code{logical} If TRUE, the missing values will be discarded after data extracted and message will
#'    be returned. DEFAULT FALSE.
#' @param coords \code{logical}. If TRUE, the original coordinates are also returned attached on the extracted dataset. Default FALSE.
#'
#' @return \code{dataframe} or \code{list} of precleaned data sets for single or multiple species.
#' @export
#'
#' @examples
#'\donttest{
#' data("efidata")
#'
#' danube <- system.file('extdata/danube.shp.zip', package='specleanr')
#'
#' danubebasin <- sf::st_read(danube, quiet=TRUE)
#'
#' #Get environmental data
#'
#' worldclim <- terra::rast(system.file('extdata/worldclim.tiff', package='specleanr'))
#'
#' referencedata <- pred_extract(data = efidata,
#'                           raster= worldclim ,
#'                           lat ="decimalLatitude",
#'                           lon = 'decimalLongitude',
#'                           colsp = 'scientificName',
#'                           bbox = danubebasin,
#'                           list= TRUE, #list will be generated for all species
#'                           minpts = 7, merge=TRUE)
#'}

pred_extract <- function(data, raster, lat = NULL,
                         lon = NULL, bbox = NULL,
                         colsp, minpts = 10, mp = TRUE,
                         rm_duplicates = TRUE,
                         na.rm = TRUE,
                         na.inform = FALSE,
                         list=TRUE,
                         merge=FALSE,
                         verbose= FALSE,
                         warn = FALSE,
                         coords = FALSE){

  if(missing(data)) stop('Data frame with species record missing')

  if(isTRUE(mp))if(minpts<=2) stop('Minimum number of species records should be  atleast greater than ', minpts,'. Default is 10')

  if(length((colnames(data)[colnames(data)==colsp]))<1) stop(colsp, ' is  not found in the ', deparse(substitute(data)),' data provided')

  #To ungroup data if tidyverse function were applied on it.

  if(is(data, 'grouped_df')|| is(data, 'tbl') ||is(data, 'tbd_df')) dfnew <- as.data.frame(data) else dfnew <- data

  #check and removing missing coordinates

  #check if sf is installed

  check_packages(pkgs = c('terra', 'sf'))

  #change the object into sf file format to enable geographical filtering outside the bounding box using st_filter
  if(!is(data, 'sf')){

    if(is.null(lat) | is.null(lon)) stop("Provide the latitude and longitude parameters.")

    species_lon_df <- dfnew[complete.cases(dfnew[ , c(lat, lon, colsp)]), ]

    spdata_new <- species_lon_df |> sf::st_as_sf(coords = c(lon, lat), crs = sf::st_crs(4326))

  }else{
    #no need to indicate the geometry since the geometry already has no NA,
    #otherwise it can not be converted# drop geometry for complete cases to work well

    spdata_new <- dfnew[complete.cases(as.data.frame(dfnew)[ , c(colsp)]), ]

  }

  #filter out records outside the bounding box if provided.
  if(!is.null(bbox)){

    if(inherits(bbox, "sf")) {

      basin_df <- sf::st_filter(spdata_new, bbox)

    }else if(inherits(bbox, 'numeric') && length(bbox) == 4){

      class(bbox) <- "bbox"


      bb <- sf::st_as_sfc(bbox) |> sf::st_set_crs(sf::st_crs(spdata_new))

      basin_df <- sf::st_filter(spdata_new, bb)
    }else{
      stop("The bounding box provided is wrong.")
    }
  } else{
    basin_df <- spdata_new
  }

  #remove duplicate data for coordinates and each species if TRUE

  if(isTRUE(rm_duplicates)){

    dupdata <- as.data.frame(basin_df[!duplicated(basin_df[c('geometry',colsp)]),])

  }else{
    dupdata <- as.data.frame(basin_df)
  }

  #check there is enough species data after duplicate removal for at some species

  zz <- as.data.frame(table(dupdata[,colsp] |> sf::st_drop_geometry()))$Freq

  if(isTRUE(mp))if(zz[which.max(zz)]<minpts) stop('All species do not have enough data after removing missing values and duplicates.')

  #run through each species
  unx <- unique(unlist(dupdata[, colsp]))

  rastdata <- list()

  if(length(unx)<1) stop('Species column ', colsp ,' provided must have species names.')

  if(length(unx)==1){

    spdfext <- dupdata |>  sf::st_as_sf()

    if(isTRUE(mp))if(nrow(spdfext)<minpts) warning(unx, ' has less than ', minpts, 'records')

    #extract species environmental data where species were recorded

    if(isTRUE(coords)){

      coordcols <- sf::st_coordinates(spdfext)

      colnames(coordcols) <- c(lon, lat)

      bout<- cbind(as.data.frame(terra::extract(raster, spdfext , ID=FALSE, xy=TRUE, bind = merge)), coordcols)

    }else{
      bout<- as.data.frame(terra::extract(raster, spdfext , ID=FALSE, xy=TRUE, bind = merge))
    }
    #remove NAs after data extract from the raster layer

    if(isTRUE(na.rm)){
      biodata <- bout[complete.cases(bout[ , names(raster)]), ]

      NROWS <- nrow(bout[!complete.cases(bout[ , names(raster)]), ])

      if(isTRUE(na.inform) && NROWS>0) message(NROWS, " rows removed. Returned NAs in extracting environmental predictors from rasters.")
    } else{
      biodata <- bout
    }

  }else{

    for (ci in seq_along(unx)) {

      spnames <- unlist(unx)[ci]

      spdfdata <- dupdata[dupdata[, colsp]==spnames,]

      spdfext <- spdfdata|> sf::st_as_sf()

      if(isTRUE(mp))if(nrow(spdfext)<minpts) {

        if(isTRUE(verbose)==TRUE) message(spnames, ' will be removed because the records are less than  ', minpts, '.')
        next
      }

      if(list==FALSE){

        if(isTRUE(coords)){

          coordcols <- sf::st_coordinates(spdfext)

          colnames(coordcols) <- c(lon, lat)

          dextract <- cbind(as.data.frame(terra::extract(raster, spdfext , ID=FALSE, xy = TRUE, bind = merge)), coordcols)

        }else{
          dextract <- as.data.frame(terra::extract(raster, spdfext , ID=FALSE, xy = TRUE, bind = merge))
        }

        if(isTRUE(na.rm)){

          rastdata[[ci]] <- dextract[complete.cases(dextract[ , names(raster)]), ]

          NROWS <- nrow(dextract[!complete.cases(dextract[ , names(raster)]), ])

          if(isTRUE(na.inform) && NROWS>0) message(NROWS, " rows removed. Returned NAs in extracting environmental predictors from rasters for ", spnames, ".")

        } else{
          rastdata[[ci]] <- dextract
        }

        rastdata[[ci]][,'species'] <- spnames

        biodata <- do.call(rbind, rastdata)

      }else if (list==TRUE){

        if(isTRUE(coords)){

          coordcols <- sf::st_coordinates(spdfext)

          colnames(coordcols) <- c(lon, lat)

          dextract <- cbind(as.data.frame(terra::extract(raster, spdfext , ID=FALSE, xy = TRUE, bind = merge)), coordcols)

        }else{
          dextract <- as.data.frame(terra::extract(raster, spdfext , ID=FALSE, xy = TRUE, bind = merge))
        }

        if(isTRUE(na.rm)){

          rastdata[[ci]] <- dextract[complete.cases(dextract[ , names(raster)]), ]

          NROWS <- nrow(dextract[!complete.cases(dextract[ , names(raster)]), ])

          if(isTRUE(na.inform)&& NROWS>0) message(NROWS, " rows removed. Returned NAs in extracting environmental predictors from rasters for ", spnames, ".")

        } else{
          rastdata[[ci]] <- dextract
        }

        #if(isTRUE(merge)) rastdata[[ci]]<- cbind(rastdata[[ci]], spdfext |>  sf::st_drop_geometry()) else rastdata[[ci]]

        names(rastdata)[ci] <- spnames

        lss <- sapply(rastdata, length)

        if(any(lss==0)) ldata <- rastdata[lss !=0] else ldata <- rastdata

        biodata <- ldata


      }else{

        stop('set list either TRUE or FALSE to return either dataframe or lists of species data')
      }
    }

  }
  return(biodata)
}


