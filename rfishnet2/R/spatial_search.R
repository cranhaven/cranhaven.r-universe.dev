#' Filter data by longitude and latitude.
#'
#' \code{spatialsearch} returns the data that falls within radius given radius,
#' and latitude and longitude coordinates.
#'
#' This is  a function to filter data given in the format of a csv file from
#' FishNet2. For this to work properly, the dataframe must have column names
#' using names given in standard csv format from FishNet2 website.
#'
#' @export
#' @param df A dataframe in FishNet2 standard format (by using read.csv())
#' @param lat Latitude coordinate
#' @param lon Longitude coordinate
#' @param r Radius in kilometers
#' @return Rows in file that fall within circle with center (lat,long) and
#'   radius r
#'
#' @examples
#' spatial_search(ictaluridae, 36.12, -77.63, 1)
#'
#' \dontrun{
#' spatial_search(ictaluridae, -173,44,10)
#' }
#'
spatial_search <- function(df, lat, lon, r){

  if(!is.data.frame(df)) {
    stop("Error: Function input is not a dataframe")
  }

  if(r < 0 ){
    stop("Error: radius must be nonnegative.")
  }

  if((lat > 90) | (lat < -90)){
    stop("Error: Incorrect latitude input.")
  }

  if((lon > 180) | ( lon  < -180)) {
    stop("Error: Incorrect longitude input.")
  }

  if(("Latitude" %in% names(df)) && ("Longitude" %in% names(df))){

    coord = c(lat,lon)
    data_coord = vector("integer", 2)

    d<- subset(df, is.na(Latitude) == FALSE & is.na(Longitude) == FALSE)
    len = nrow(d)
    distance = vector("integer", len)

    # use haversine formula
    for (row in 1:len) {

      data_coord[1] <- d[row, "Latitude"]
      data_coord[2] <- d[row, "Longitude"]
      distance[row] <- pracma::haversine(coord,data_coord)
    }

    d$distance <- distance
    output <- subset(d, distance <= r)

    return(output)
  }
  else{

    stop("Error: Missing columns 'Latitude' and/or 'Longitude'")
  }
}
