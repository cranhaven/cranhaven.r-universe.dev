#' Add together anonymous ID entries
#'
#' Anonymous vessel IDs are supplied when there are less than 2 vessels. Therefore when adding
#' these values, if there is one or more values containing an empty string (? 2 vessels)
#' then the results is an empty string also. Otherwise, a unique vector of vessel IDs is
#' computed and if there are 2 or less unique entries they are returned, otherwise an empty string
#' is returned.
#'
#' @note
#' the format of the vessel IDs is a semi-colon separated list of IDs contained in a character
#' vector of length one.
#'
#' @param id a vector vessel Ids entries: string vector of semi-colon separated IDs
#' @param n a vector corresponding to the number of unique vessel ids in the vector
#'          supplied in \code{id} where 3 codes for anything greater than 2.
#'
#' @return a single character
#'
#' @examples
#'
#' sum_vessel_ids(c("id1;id2", "id1", "id2", "id1;id3", ""), c(2, 1, 1, 2, 3))
#' sum_vessel_ids(c("id1;id2", "id1", "id2", "id1;id3"), c(2, 1, 1, 2))
#'
#'
#' sum_distinct_vessels(c("id1;id2", "id1", "id2", "id1;id3", ""), c(2, 1, 1, 2, 3))
#' sum_distinct_vessels(c("id1;id2", "id1", "id2", "id1;id3"), c(2, 1, 1, 2))
#'
#' \dontrun{
#' require(dplyr)
#'
#' data(vms)
#' vms <-
#'   vms %>%
#'   group_by(year, cSquare) %>%
#'   summarise(
#'     fishingHours = sum(fishingHours, na.rm = TRUE),
#'     totweight = sum(totweight, na.rm = TRUE),
#'     noDistinctVessels = sum_distinct_vessels(anonymizedVesselID, noDistinctVessels),
#'     anonymizedVesselID = sum_vessel_ids(anonymizedVesselID, noDistinctVessels),
#'     .groups = "drop"
#'   )
#' }
#'
#' @rdname anonymousIDs
#' @export
sum_distinct_vessels <- function(id, n) {
  if (any(n >= 3)) {
    return(3)
  }

  ids <- paste(id, collapse = ";")
  ids <- strsplit(ids, ";")[[1]]
  ids <- unique(ids)
  pmin(3, length(ids))
}
