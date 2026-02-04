

#### Functions for summarizing the accuracy of the assignments given known location ####

# make a raster that holds the distances between a point and the center of every cell
# this is straightforward and fast.  Here is a synopsis:
#  bunk <- xyFromCell(riso, 1:ncell(riso))
#  risoDist <- riso
#  values(risoDist) <- distCosine(c(-100, 50), bunk)
#
# That is pretty much all there is to it!
# I can plot those as a density like so, roughly:
# ggplot(whoa, aes(x = dist, y = ..density.., weight = posterior_prob)) + geom_density()



#' return a raster of great circle distances (in km)
#'
#' Given an input raster R, this returns a raster of the same dimension where
#' every cell is the great circle distance between lat, and long, and the
#' center of every cell in R.
#' @param R a raster
#' @param lat a latitude value (must be of length 1)
#' @param long a longitude value (must be of length 1)
#' @export
#' @examples
#' # We compute the great circle distance between the lat/long of my office in
#' # California, to every cell in the raster denoting the breeding habitat
#' # of Wilson's warbler:
#' gcr <- great_circle_raster(wiwa_breed, lat = 36.951564, long = -122.065116)
#'
#' # plot that if you want
#' \dontrun{
#' plot(gcr)
#' lines(get_wrld_simpl())
#' }
great_circle_raster <- function(R, lat, long) {
  stopifnot(length(lat) == 1, length(long) == 1)

  ret <- R  # just initialize this way to get the right resolution, etc
  raster::values(ret) <- geosphere::distCosine(c(long, lat), raster::xyFromCell(R, 1:raster::ncell(R)))
  ret * 0.001
}




#' finds the cumulative posterior probability of each cell for HPD calculation
#'
#' This function is not exported currently, but is used in min_hpd_inclusion_area
#' @param B a raster of posterior probs
#' @keywords internal
hpd_cumul <- function(B) {
  ord <- order(raster::values(B), decreasing = TRUE, na.last = TRUE)
  cumul = cumsum(raster::values(B)[ord])
  raster::values(B)[ord] <- cumul
  B
}



#' compute the smallest area of an HPD credible set that includes the true bird location
#'
#' This function computes the area of the HPD credible set that is as small as possible
#' but just barely includes the cell with the true location (or, if the true location is not
#' actually inluded in the range map, we use the closest cell on the range map to the true
#' location).  This operates on a single rasterLayer.  To operate on a rasterBrick of posteriors
#' and a data frame of lat-longs, use `min_hpd_inc_area_df()`.
#' @param M the posterior surface matrix
#' @param lat the true latitude
#' @param long the true longitude
#' @keywords internal
min_hpd_inclusion_area <- function(M, lat, long) {
  hpc <- hpd_cumul(M)
  gcd <- great_circle_raster(M, lat, long)

  # here are the distances from the point to non-NA cells
  dd <- gcd * (hpc > 0)

  # here is the minimum distance to a nonNA cell
  mindist <- raster::cellStats(dd, min, na.rm = TRUE)

  # here is a mask that picks out the closest cell(s)
  cc <- abs(dd - mindist) < 10-8  # don't test equality of numerics...

  # here is the hpd value at that point
  hpd_at_ll <- raster::cellStats(cc * hpc, sum, na.rm = TRUE) / raster::cellStats(cc, sum, na.rm = TRUE)

  # now, get the area of the hpd_at_ll  HPD CI
  hpc_area <- raster::cellStats( (hpc < (hpd_at_ll + 1e-9)) * raster::area(hpc), stat = sum, na.rm = TRUE)

  # return both the cumulative prob and the cumulative HPD area
  list(area = hpc_area, cumul_prob = hpd_at_ll)
}


#' compute the minimum hpd inclusion area for all the birds in a rasterStack
#'
#' This is a convenient wrapper that lets you pass in a data frame of bird
#' names with lats and longs and a rasterStack or rasterBrick and it returns
#' the min hpd inclusion area for each bird in a tbl_df-ed data frame.
#' @param birds  a data frame that minimally has columns \code{Short_Name}, \code{lat}, and
#' \code{long}.
#' @param R a rasterStack or rasterBrick of posteriors that each bird originated from
#' each of the cells.  The names of the layers must correspond to the ID column in
#' kbirds.
#' @return This returns a list with three components:
#' \itemize{
#' \item{area} {the minimum hpd inclusion area as a data frame with columns "Short_Name" and "min_hpd_area"}
#' \item{missingFromRaster}{character vector of the names of birds found in birds$Short_Name but not found in
#' the names of R}
#' \item{missingFromDF}{ character vector of names in R that are not found in birds$Short_Name.}
#' }
#' @keywords internal
min_hpd_inc_area_df <- function(birds, R) {

  lats <- birds$lat
  longs <- birds$long
  names(lats) <- birds$Short_Name
  names(longs) <- birds$Short_Name
  birds_df <- birds$Short_Name
  birds_rast <- names(R)

  missingFromRaster <- setdiff(birds_df, birds_rast)
  missingFromDF <- setdiff(birds_rast, birds_df)

  birds_common <- intersect(birds_df, birds_rast)

  names(birds_common) <- birds_common

  tmp <- unlist(lapply(birds_common, function(b) {min_hpd_inclusion_area(R[[b]], lats[b], longs[b])$area}))

  area <- dplyr::data_frame(Short_Name = names(tmp), min_hpd_area = tmp)

  list(area = area,
       missingFromRaster = missingFromRaster,
       missingFromDF = missingFromDF)
}
