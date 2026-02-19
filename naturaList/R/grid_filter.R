#' Filter the occurrence with most confidence in species identification inside
#' grid cells
#'
#' In each grid cell it selects the occurrence with the highest confidence level
#' in species identification made by \code{\link{classify_occ}} function.
#'
#' @param occ.cl data frame with occurrence records information already
#'   classified by \code{\link{classify_occ}} function.
#' @param grid.resolution numeric vector with width and height of grid cell in
#'   decimal degrees.
#' @param r raster from which the grid cell resolution is derived.
#' @param occurrence.id column name of \code{occ} with link or code for the
#'  occurrence record. See in
#'  \href{https://dwc.tdwg.org/terms/#dwc:occurrenceID}{Darwin Core Format}
#' @param occ.id deprecated, use \code{occurrence.id} instead
#' @param species column name of \code{occ} with the species names.
#' @param scientific.name deprecated, use \code{species} instead.
#' @param identified.by column name of \code{occ.cl} with the name of who
#'  determined the species.
#' @param determined.by deprecated, use \code{identified.by} instead
#' @param decimal.longitude column name of \code{occ.cl} longitude in decimal
#'  degrees.
#' @param longitude deprecated, use \code{decimal.longitude} instead
#' @param decimal.latitude column name of \code{occ.cl} latitude in decimal
#'  degrees.
#' @param latitude deprecated, use \code{decimal.latitude} instead
#' @param basis.of.record column name with the specific nature of the data
#'  record. See details.
#' @param basis.of.rec deprecated, use \code{basis.of.record} instead.
#' @param media.type column name of \code{occ.cl} with the media type of recording.
#'  See details.
#' @param institution.code column name of \code{occ.cl} with the name (or acronym)
#'  in use by the institution having custody of the object(s) or information
#'  referred to in the record.
#' @param institution.source deprecated, use \code{institution.code} instead.
#' @param collection.code column name of \code{occ.cl} with The name, acronym,
#'  code, or initials identifying the collection or data set from which the
#'  record was derived.
#' @param catalog.number column name of \code{occ.cl} with an identifier
#'  (preferably unique) for the record within the data set or collection.
#' @param year Column name of \code{occ.cl} the four-digit year in which the
#'  Event occurred, according to the Common Era Calendar.
#' @param year.event deprecated, use \code{year} instead.
#' @param date.identified Column name of \code{occ.cl} with the date on which the
#'  subject was determined as representing the Taxon.
#'
#' @return Data frame with the same columns of \code{occ.cl}.
#'
#' @seealso \code{\link[naturaList]{classify_occ}}
#'
#' @examples
#'
#' \dontrun{
#'
#' data("A.setosa")
#' data("speciaLists")
#'
#' occ.class <- classify_occ(A.setosa, speciaLists)
#' occ.grid <- grid_filter(occ.class)
#'
#' }
#'
#'
#'
#'
#' @author Arthur V. Rodrigues
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export

grid_filter <- function(occ.cl,
                        grid.resolution = c(0.5,0.5),
                        r = NULL,
                        institution.code ="institutionCode",
                        collection.code = "collectionCode",
                        catalog.number = "catalogNumber",
                        year = "year",
                        date.identified = "dateIdentified",
                        species = "species",
                        identified.by = "identifiedBy",
                        decimal.latitude = "decimalLatitude",
                        decimal.longitude = "decimalLongitude",
                        basis.of.record = "basisOfRecord",
                        media.type = "mediaType",
                        occurrence.id = "occurrenceID",
                        institution.source , #deprecated
                        year.event, #deprecated
                        scientific.name, #deprecated
                        determined.by, #deprecated
                        latitude, #deprecated
                        longitude, #deprecated
                        basis.of.rec, #deprecated
                        occ.id){ #deprecated

  # new arguments  ----------------------------------------------------------

  if (!missing(institution.source)) {
    warning("argument 'institution.source' is deprecated; please use 'institution.code' instead.",
            call. = FALSE)
    institution.code <- institution.source
  }

  if (!missing(year.event)) {
    warning("argument 'year.event' is deprecated; please use 'year' instead.",
            call. = FALSE)
    year <- year.event
  }

  if (!missing(scientific.name)) {
    warning("argument 'scientific.name' is deprecated; please use 'species' instead.",
            call. = FALSE)
    species <- scientific.name
  }

  if (!missing(determined.by)) {
    warning("argument 'determined.by' is deprecated; please use 'identified.by' instead.",
            call. = FALSE)
    identified.by <- determined.by
  }

  if (!missing(latitude)) {
    warning("argument 'latitude' is deprecated; please use 'decimal.latitude' instead.",
            call. = FALSE)
    decimal.latitude <- latitude
  }

  if (!missing(longitude)) {
    warning("argument 'longitude' is deprecated; please use 'decimal.longitude' instead.",
            call. = FALSE)
    decimal.longitude <- longitude
  }

  if (!missing(basis.of.rec)) {
    warning("argument 'basis.of.rec' is deprecated; please use 'basis.of.record' instead.",
            call. = FALSE)
    basis.of.record <- basis.of.rec
  }

  if (!missing(occ.id)) {
    warning("argument 'occ.id' is deprecated; please use 'occurrence.id' instead.",
            call. = FALSE)
    occurrence.id <- occ.id
  }


# initial checkings -------------------------------------------------------


  natList_column <- "naturaList_levels" %in% colnames(occ.cl)
  if(!natList_column){
    stop("occurrences must has 'naturaList_levels' classification.")
  }

  spp <- unique(occ.cl[,species])

  if(length(spp) > 1){
    stop("there is more than 1 species in 'occ.cl'. Please, use 'grid_filter' with one species at a time. You can create a loop for filter more species.")
  }

  if(nrow(occ.cl) == 1){
    return(occ.cl)
  }

  if(!is.null(r)){
    if(!inherits(r, what =  "RasterLayer")){stop("'r' must be of class RasterLayer")}
  }

  if(is.null(r)){

    spt.spp_DF <- sp::SpatialPointsDataFrame(
      occ.cl[,c(decimal.longitude, decimal.latitude)], occ.cl)

    ext <- raster::extent(spt.spp_DF)[1:4]

    new.ext <- c(ext[1] - grid.resolution[1],
                 ext[2] + grid.resolution[2],
                 ext[3] - grid.resolution[1],
                 ext[4] + grid.resolution[2])

    r <- raster::raster(resolution = grid.resolution, ext = raster::extent(new.ext))
  }

  occ.xy <- as.matrix(occ.cl[,c(decimal.longitude, decimal.latitude)])
  occ.cell <- dplyr::mutate(occ.cl, cell = raster::cellFromXY(r, occ.xy))
  cols.occ <- names(occ.cl)

  occ.grid <- occ.cell %>%
    dplyr::arrange(.data$cell, .data$naturaList_levels, desc(.data[[date.identified]]), desc(.data[[year]])) %>%
    dplyr::group_by(.data$cell) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(cols.occ) %>%
    as.data.frame()

  return(occ.grid)

}
