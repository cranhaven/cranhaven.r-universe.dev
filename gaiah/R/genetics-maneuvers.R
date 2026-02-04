
# processing data from genetic assignments



#' Convert posteriors to particular genetic reporting groups into raster
#'
#' When birds have been assigned to breeding groups or "general areas" as in Ruegg et al. 2014
#' then the posterior probabilty with which the birds were assigned to the groups needs to be
#' "smeared out" in a raster over the spatial extent of the groups.
#' @param G long format data frame like breeding_wiwa_genetic_posteriors.  Has to have columns of ID, region, and posterior
#' @param R a RasterStack like "genetic_regions".  The sum of these should be the total known range.
#' The names of the regions in R must be the same as the entries in the "region" column in G.
#' @return This returns a list of rasters for each bird in G.  The entries in the raster
#' are the posterior probability of being from that cell.  This assumes that birds are equally likely
#' to come from any cell within the group's region.  It doesn't return a rasterStack because you
#' can't subset rasterStacks to change orders, etc., and it mangles names.
#' @export
#' @examples
#' library(raster)  # needed to deal with "genetic_regions" variable
#' # get a small subset of individuals so it doesn't take too long
#' data(breeding_wiwa_genetic_posteriors)
#' data(genetic_regions)
#' BW <- breeding_wiwa_genetic_posteriors %>%
#'   dplyr::filter(Short_Name %in% c("eNBFR01", "wABCA05", "wORHA21"))
#'
#' # run the function on those
#' GPRs <-  genetic_posteriors2rasters(BW, genetic_regions)
genetic_posteriors2rasters <- function(G, R) {

  inGnotR <- setdiff(unique(G$region), names(R))
  inRnotG <- setdiff(names(R), unique(G$region))
  if(length(inGnotR > 0)) stop("Region names in G that are not in R: ", paste(inGnotR, collapse = ", "))
  if(length(inRnotG > 0)) stop("Region names in R that are not in G: ", paste(inRnotG, collapse = ", "))

  colnamesMissing <- setdiff(c("ID", "region", "posterior"), names(G))
  if(length(colnamesMissing) > 0) stop("G appears to be missing the required columns ", paste(colnamesMissing, collapse = ", "))

  # get the number of non-zero cells in each of the genetic regions
  Ncell <- raster::cellStats(R, sum)

  # split the genetic data frame into a list by individual
  gList <- base::split(G, G$ID)

  lapply(gList, function(df) {
    # make a named vector of posteriors to region
    gp <- df$posterior
    names(gp) <- as.character(df$region)
    gp <- gp[names(R)]  # make sure they are in the correct order (FUTURE: error check the names)

    # this smears posterior probability into each region so that the sum over cells
    # in each region is the posterior probability of being from that region, then the calc
    # function sums over all the layers.  Way to go Robert Hijman! ...lovely, compact sytax
    raster::calc(R * gp / Ncell, fun = sum)
  })


}
