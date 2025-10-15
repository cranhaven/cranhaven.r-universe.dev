#' @import sf
#' @importFrom graphics par
#' @importFrom grDevices rgb
#' @importFrom utils download.file
#' @importFrom httr2 req_url_query
#' @importFrom httr2 req_headers


#' @noMd
pt2bbox <- function(x, y, r, proj, longlat){
  coor <- data.frame(lon=x, lat=y)
  suppressWarnings(
    pt <- sp::SpatialPoints(coor, proj4string=longlat)
  )
  suppressWarnings(
    pt <- sp::spTransform(pt, proj)
  )
  xmin <- pt@coords[1,1] - r
  xmax <- pt@coords[1,1] + r
  ymin <- pt@coords[1,2] - r
  ymax <- pt@coords[1,2] + r
  coor_ <- data.frame(lon=c(xmin, xmax), lat=c(ymin, ymax))
  suppressWarnings(pt_ <- sp::SpatialPoints(coor_, proj))
  suppressWarnings(
    pt_ <- sp::spTransform(pt_, CRSobj=longlat)
  )
  return(list(c(pt_@coords[1,1], pt_@coords[1,2], pt_@coords[2,1], pt_@coords[2,2]),
              c(xmin, ymin, xmax, ymax)))
}

#' @noMd
convertBbox <- function(bbox, proj, longlat) {
  coor <- data.frame(lon=c(bbox[1],bbox[3]),
                      lat=c(bbox[2],bbox[4]))
  pt <- sp::SpatialPoints(coor, proj4string=longlat)
  pt <- sp::spTransform(pt, proj)
  xmin <- pt@coords[1,1]
  ymin <- pt@coords[1,2]
  xmax <- pt@coords[2,1]
  ymax <- pt@coords[2,2]
  return(list(bbox,c(xmin, ymin, xmax, ymax)))
}

#' @noMd
# create a request of the TNMAccess API
return_response <- function(bbox, max_return) {
  api1 <- 'https://tnmaccess.nationalmap.gov/api/v1/products?bbox='
  api2 <- paste0(bbox[1], ",",
                 bbox[2], ",",
                 bbox[3], ",",
                 bbox[4])
  api3 <- paste0('&datasets=Lidar%20Point%20Cloud%20(LPC)&max=',
                 max_return,
                 '&prodFormats=LAS,LAZ')
  json <- httr2::request(paste0(api1, api2, api3)) %>%
    httr2::req_timeout(10000) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  items <- length(json$items)
  titles <- c()
  sourceId <- c()
  metaUrl <- c()
  sizeInBytes <- c()
  startYear <- c()
  previewGraphicURL <- c()
  downloadLazURL <- c()
  if (items >= 1) {
    for (i in 1:items) {
      item <- json[[2]][[i]]
      titles <- c(titles, item$title)
      sourceId <- c(sourceId, item$sourceId)
      url <- paste0(item$metaUrl, "?format=json")
      metaUrl <- c(metaUrl, url)
      sizeInBytes <- c(sizeInBytes, item$sizeInBytes)
      startYear <- c(startYear, find_year(url))
      previewGraphicURL <- c(previewGraphicURL, item$previewGraphicURL)
      downloadLazURL <- c(downloadLazURL, item$downloadLazURL)
    }
    df <- data.frame(titles, sourceId,
                     metaUrl, sizeInBytes,
                     startYear, previewGraphicURL,
                     downloadLazURL)
    return(df)
  }
}

#' @noMd
# find year
find_year <- function(url) {
  j <- httr2::request(url) %>%
    httr2::req_timeout(10000) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  date <- j$dates[[2]]$dateString %>% strsplit("-") %>% unlist()
  return(as.integer(date[1]))
}

#' @noMd
# Terms of Use: https://opentopography.org/usageterms#:~:text=You%20agree%20to%2C%20and%20will,and%20their%20OpenTopography%20accounts%20closed.&text=We%20retain%20the%20right%20to,who%20abuse%20the%20system%20intentionally.
# Version: As of May 24th 2021 OpenTopography is supplying V3.2 (Jan 2021) from:
# ftp://ftp.eorc.jaxa.jp//pub/ALOS/ext1/AW3D30/release_v2012_single_format/
# Data downloaded prior to May 24th 2021 was in format: May 2016: Global terrestrial region
# (within approx. 82 deg. of N/S latitudes) of Version 1 released (approx. 22,100 tiles)
return_response2 <- function(bbox, key, datatype) {
  if (datatype %in% c('AW3D30','SRTMGL1')) {
    url_ <- "https://portal.opentopography.org/API/globaldem"
  } else if (datatype %in% c('USGS1m','USGS10m', 'USGS30m')) {
    url_ <- "https://portal.opentopography.org/API/usgsdem"
  }
  response <- httr2::request(url_) %>%
    httr2::req_url_query(demtype = datatype,
                         south = bbox[2],
                         north = bbox[4],
                         west = bbox[1],
                         east = bbox[3],
                         outputFormat = "GTiff",
                         API_Key = key) %>%
    httr2::req_headers(accept = "*/*") %>%
    httr2::req_perform()
  return(response)
}

#' @noMd
retry_download <- function(url, destination, method = "auto", retries = 5, quiet = TRUE) {
  attempt <- 1
  success <- FALSE

  while (attempt <= retries && !success) {
    try({
      download.file(url, destination, method = method, quiet = quiet)
      success <- TRUE
    }, silent = TRUE)

    if (!success) {
      # message(paste("Attempt", attempt, "failed for URL:", url))
      Sys.sleep(5) # Wait before retrying, to avoid hammering the server
    }
    attempt <- attempt + 1
  }

  if (success) {
    return(1)
  } else {
    return(0)
  }
}

