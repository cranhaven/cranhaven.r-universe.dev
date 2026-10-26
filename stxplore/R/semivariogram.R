#' Computes the semi-variogram using a dataframe or a stars object.
#'
#'@description
#' Computes the semi-variogram from a stars or a dataframe. Input arguments differ
#' for each case. Function autoplot can plot the output.
#'
#' When the input is a dataframe, the locations, time and the quantity of interest needs to
#' be given. When the input is a stars object, a 3 dimensional stars object needs to be given
#' as input with the first 2 dimensions being spatial and the third being time.
#'
#' @param x The dataframe or stars object. If it is a dataframe, then it should have the locations.
#' @param latitude_linear If \code{TRUE}  a linear model is fitted with latitude as a covariate is fitted.
#' @param longitude_linear If \code{TRUE}  a linear model is fitted with longitude as a covariate is fitted.
#' @param missing_value If a certain value such as -9999 denotes the missing values for given locations and times.
#' @param width A parameter to the \code{gstat::variogram} function. The width of the distance intervals to be considered.
#' @param cutoff A parameter to the \code{gstat::variogram} function. The spatial separation distance.
#' @param tlagmax A parameter to the \code{gstat::variogram} function. The maximum time lag.
#' @param ... Other arguments that need to be used for datafames  or currently ignored.
#' @param times_df For dataframes: the dataframe containing the dates in \code{Date} format.
#' @param values_df For dataframes: the dataframe of dimension \code{length(times) x length(locations)} containing the quantity of interest.
#' @param object For autoplot: the output from the semivariogram function.
#'
#' @return A semivariogram object with a gstat variogram and the original data.
#'
#' @examples
#' # Dataframe example
#' library(dplyr)
#' data(locs)
#' data(Times)
#' data(Tmax)
#' temp_part <- with(Times, paste(year, month, day, sep = "-"))
#' temp_part <- data.frame(date = as.Date(temp_part)[913:923])
#' Tmax <- Tmax[913:923, ]
#' semidf <- semivariogram(locs,
#'         temp_part,
#'         Tmax,
#'         latitude_linear = FALSE,
#'         longitude_linear = FALSE,
#'         missing_value = -9999,
#'         width = 50,
#'         cutoff = 1000,
#'         tlagmax = 7
#' )
#' autoplot(semidf)
#'
#' # Stars example
#' library(stars)
#' # Create a stars object from a data frame
#' precip_df <- NOAA_df_1990[NOAA_df_1990$proc == 'Precip', ] %>%
#'   filter(date >= "1992-02-01" & date <= "1992-02-05")
#' precip <- precip_df[ ,c('lat', 'lon', 'date', 'z')]
#' st_precip <- st_as_stars(precip, dims = c("lon", "lat", "date"))
#' semist <- semivariogram(st_precip)
#' autoplot(semist)
#'
#' @export
semivariogram <- function(x,
                          latitude_linear = TRUE,
                          longitude_linear = TRUE,
                          missing_value = -9999,
                          width = 80,
                          cutoff = 1000,
                          tlagmax = 6,
                          ...){
  UseMethod("semivariogram")
}

#' @rdname semivariogram
#' @export
semivariogram.data.frame <- function(x,
                                     latitude_linear = TRUE,
                                     longitude_linear = TRUE,
                                     missing_value = -9999,
                                     width = 80,
                                     cutoff = 1000,
                                     tlagmax = 6,
                                     times_df,
                                     values_df,
                                     ...){
  if(missing(x)){
    stop("Empty dataframe x. Please give a dataframe with latitude and longitude.")
  }

  locations_df <- x

  if(missing(times_df)){
    stop("Empty dataframe times_df. Please give a dataframe with times.")
  }

  if(missing(values_df)){
    stop("Empty dataframe values_df. Please give an N x T dataframe with values of the quantity you are interested in.  ")
  }

  if(!setequal(colnames(locations_df), c("id", "lon", "lat"))){
    stop("The column names of locations_df needs to be id, lon and lat. Please update locations_df.")
  }

  if(dim(times_df)[2] !=1 ){
    stop("The dataframe times_df needs to have only 1 column of dates.")
  }

  colnames(times_df) <- "date"
  if(!lubridate::is.Date(times_df$date)){
    stop("The dataframe times_df doesn't contain any dates.")
  }

  if(!identical(dim(values_df), c(dim(times_df)[1], dim(locations_df)[1]) )){
    stop("The dataframe values_df needs to have T x N dimensions, T = number of times N = number of locations.")
  }

  names(values_df)  <- locations_df$id
  values_df <-  cbind(times_df,  values_df)
  id <- NULL

  # Define spatial and temporal components
  spat_part <- sp::SpatialPoints(coords = locations_df[, c("lon", "lat")])
  temp_part <- as.Date(times_df$date)

  values_long <- tidyr::pivot_longer(values_df, cols = 2:dim(values_df)[2])
  colnames(values_long) <- c("date", "id", "z")
  values_long$id <- as.integer(values_long$id)
  values_long <- dplyr::arrange(values_long, date, id)

  STObj3 <- spacetime::STFDF(sp = spat_part,
                            time = temp_part,
                            data = values_long)

  # Assign a projection
  sp::proj4string(STObj3) <- sp::CRS("+proj=longlat +ellps=WGS84")

  # Label missing values
  STObj3$z[STObj3$z == missing_value] <- NA

  if(latitude_linear & longitude_linear){
    formula <- z ~ 1 + lat + lon
  }else if(latitude_linear){
    formula <- z ~ 1 + lat
  }else if(longitude_linear){
    formula <- z ~ 1 + lon
  }else{
    formula <- z ~ 1
  }

  vv <- gstat::variogram(object = formula,            # fixed effect component
                  data = STObj3,                      #
                  width = width,                      #
                  cutoff = cutoff,                    # only consider pts < cutoff km apart
                  tlags = 0.01:tlagmax)               # 0-tlagmaxdays days (will create an NA if start at 0)

 structure(list(
    variogram = vv,
    loc_data = locations_df,
    times_data = times_df,
    values_data = values_df,
    call = match.call()
  ), class='semivariogramobj')
}


#' @rdname semivariogram
#' @export
semivariogram.stars <- function(x,
                                latitude_linear = TRUE,
                                longitude_linear = TRUE,
                                missing_value = -9999,
                                width = 80,
                                cutoff = 1000,
                                tlagmax = 6,
                                ...){
   if(missing(x)){
     stop("Empty stars object x. Please give a stars object with 3 dimensions.")
   }

   x <- stats::setNames(x, "dat")

   # get x, y and t values
   x1 <- stars::st_get_dimension_values(x, 1)
   x2 <- stars::st_get_dimension_values(x, 2)
   t_vals <- stars::st_get_dimension_values(x, 3)

   times_df <- data.frame(time = t_vals)

   # make an xy grid
   gridxy <- meshgrid2d(x1, x2)
   locations_df <- data.frame(id = 1:dim(gridxy)[1], lon = gridxy[ ,1], lat = gridxy[ ,2])
   # flatten the stars to 2D
   x_sf <- stars::st_xy2sfc(x, as_points = TRUE, na.rm = FALSE)
   values_df <- t(x_sf$dat)
   semivariogram.data.frame(locations_df,
                            latitude_linear = latitude_linear,
                            longitude_linear = longitude_linear,
                            missing_value = missing_value,
                            width = width,
                            cutoff = cutoff,
                            tlagmax = tlagmax,
                            times_df,
                            values_df)

}

#' @rdname semivariogram
#' @export
autoplot.semivariogramobj<- function(object, ...){
  # display the empirical semi-variogram
  vv <- object$variogram
  color_pal <- rev(grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(16))
  plot(vv, col = color_pal)
}


