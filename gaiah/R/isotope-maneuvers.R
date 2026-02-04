


#' convert columns of an ISOMAP isoscape to a raster object
#'
#' Just simple conversion, but nice to have this in a brief function
#' @param isoscape the data frame of prediction.txt from ISOMAP.  The latitude column must be named
#' "lat" and the longitude column must be named "long".
#' @param column the name of the column to turn into a raster object.  This should be
#' a quoted string, like "predkrig".
#' @param Proj  the desired projection.
#' By default it is raster::projection(get_wrld_simpl()), i.e. the same projection as the wrld_simpl map.
#' @export
#' @examples
#' isorast <- isomap2raster(isomap_job54152_prediction, "predreg")
#' isorast
isomap2raster <- function(isoscape,
                          column,
                          Proj = raster::projection(get_wrld_simpl())) {

  tmp <- data.frame(x = isoscape$long, y = isoscape$lat, z = isoscape[[column]])
  sp::coordinates(tmp) <- ~ x + y # setting coordinates according to long and lat
  sp::gridded(tmp) <- T # set it to know that it is a gridded spatial object
  ret <- raster::raster(tmp) # make a raster out of it
  raster::projection(ret) <- Proj # setting coordinate reference system for raster
  ret
}




#' Add the isomap prediction and sd in columns attached to the feather isotope data frame
#'
#' Rasterizes the isomap predictions and standard deviation (using isomap2raster) and then extracts
#' the values associated with each location from the raster and returns the full data frame
#' with those values joined on in columns named iso_pred and iso_sd.  It overwrites
#' those columns with a warning if either of those columns already exists in the data.
#' @inheritParams isomap2raster
#' @param birds data frame of the individual isotope values for the birds/feathers.  Should be
#' something like \code{\link{breeding_wiwa_isotopes}}.
#' @param pred name of the column holding the prediction (like "predkrig") in the
#' isoscape data frame
#' @param sd name of the column holding the standard deviation (like "stdkrig") in the
#' isoscape data frame
#' @export
#' @examples
#' # Using the provided data from breeding Wilson's warblers and the provided
#' # predictions from isomap_job54152:
#' x <- extract_isopredictions(isoscape = isomap_job54152_prediction,
#'                        birds = breeding_wiwa_isotopes,
#'                        pred = "predkrig",
#'                        sd = "stdkrig")
extract_isopredictions <- function(isoscape,
                                   birds,
                                   pred = "predkrig",
                                   sd = "stdkrig") {

  if("iso_pred" %in% names(birds)) warning("Values in column iso_pred in birds data frame will be overwritten.")
  if("iso_sd" %in% names(birds)) warning("Values in column iso_sd in birds data frame will be overwritten.")

  if(!("lat" %in% names(birds))) stop("Bad news.  No column named \"lat\" in birds data frame.  Has to be \"lat\", not Lat or latitude or Latitude, etc.")
  if(!("long" %in% names(birds))) stop("Bad news.  No column named \"long\" in birds data frame.  Has to be \"long\", not Long or longitude or Longitude, etc.")

  # get the rasters
  iso <- isomap2raster(isoscape, pred)
  std <- isomap2raster(isoscape, sd)

  # get the lat-longs in a data frame
  ll <- data.frame(x = birds$long, y = birds$lat)

  # extract the values and add to the data frame
  birds$iso_pred <- raster::extract(iso, ll)
  birds$iso_sd <- raster::extract(std, ll)

  birds
}



#' Group bird isotope data by locations
#'
#' This takes as input a data frame of feather isotope data that also has the
#' isoscape predictions attached to it, just like the data frame returned by
#' \code{\link{extract_isopredictions}}.  The data frame must have a column
#' that gives the general location by which you will group birds for the
#' rescaling function.  The isoscape predictions by default should be in columns named
#' \code{iso_pred} for the actual prediction, and \code{iso_sd} for the standard deviation,
#' as produced by \code{\link{extract_isopredictions}}, but those are user configurable,
#' as well.
#' @param D the data frame of feather isotope data with the isoscape predictions
#' extracted for each location, as well, and a column giving general grouping
#' locations for the birds.
#' @param feather_isotope_col the string name of the column holding the feather isotope
#' data.
#' @param location_col the string name of the column holding the locations to be
#' used for grouping.
#' @param iso_pred_col name of the column holding the predicted values from the isoscape. Default
#' is \code{iso_pred}.
#' @param iso_sd_col name of the column holding the standard deviations of the predicted values
#' from the isoscape. Default is \code{iso_sd_col}.
#' @details This function returns a data frame with columns for the mean and SD of feather/bird values,
#' (\code{meanH} and \code{sdH}) and the mean predicted isotope value and the mean sd of the predicted
#' isotope values (\code{meaniso} and \code{sdiso}) for all the samples within each location.  It
#' also returns the Location column itself and a column \code{cnt} that gives the number of bird/tissue
#' samples from each location.
#'
#' This function
#' throws an error if any of the locations has only 1 sample.  If that is the case, you may consider
#' merging that sample with another location (or dropping it?).
#' @export
#' @examples
#' # first run the example for extract_isopredictions to get the variable "x"
#' example("extract_isopredictions")
#'
#' # If this were run it gives an error because there is only 1 bird at the
#' # location "Charlevoix"
#' \dontrun{
#' group_birds_by_location(x, feather_isotope_col = "Isotope.Value", location_col = "Location")
#' }
#'
#'
#' # remove that one bird at Charlevoix and re-run
#' y <- x %>%
#'   dplyr::filter(Location != "Charlevoix")
#'
#' # then group birds by location
#' gbl <- group_birds_by_location(D = y,
#'                                feather_isotope_col = "Isotope.Value",
#'                                location_col = "Location")
group_birds_by_location <- function(D,
                                    feather_isotope_col,
                                    location_col,
                                    iso_pred_col = "iso_pred",
                                    iso_sd_col = "iso_sd") {

  # Deal with quasiquotation.  I have to jump through some hoops because the values
  # are passed as strings, and I want to keep in that way so I don't have to rewrite
  # the documentation and also so that it doesn't break any older code that used these
  # functions.
  location_col_S <- as.symbol(location_col)
  feather_isotope_col_S <- as.symbol(feather_isotope_col)
  iso_pred_col_S <- as.symbol(iso_pred_col)
  iso_sd_col_S <- as.symbol(iso_sd_col)


  location_col <- rlang::enquo(location_col_S)
  feather_isotope_col <- rlang::enquo(feather_isotope_col_S)
  iso_pred_col <- rlang::enquo(iso_pred_col_S)
  iso_sd_col <- rlang::enquo(iso_sd_col_S)

  ret <- D %>%
    dplyr::group_by(!! location_col) %>%
    dplyr::summarise(
      cnt = n(),
      meanH = mean(!! feather_isotope_col),
      sdH = stats::sd(!! feather_isotope_col),
      meaniso = mean(!! iso_pred_col),
      sdiso = mean(!! iso_sd_col)
    )

  if (any(ret$cnt == 1)) {
    stop("The following locations have only a single bird (not enough to compute an SD): ", paste(ret$Location[ret$cnt == 1], collapse = ", "), ".")
  }

  ret
}
