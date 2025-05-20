
#' Return the perimeters of all the relationship zones
#'
#' This is primarily for plotting a figure in the paper about this package,
#' showing where all the relationship zones are.  It merely cycles over the
#' possible relationships in [`relationship_zone_names`] and produces one or
#' two rows in a tibble for each that has the corners of the rectangle of that
#' zone in the columns xmin, xmax, ymin, and ymax.  It is designed to be overlaid
#' upon the ancestry_match_matrix plots. There are some additional columns that give
#' us the midpoint of the area, etc.
#' @return Returns a tibble with the following columns:
#'    - `which_matrix`: a column of values `M1` or `M2`.  M1 denotes that the row's values
#'      are for the relationship zone found in or below the lower diagonal of the ancestry match matrix
#'      and M2 denotes that the row's value are of the zone found in the upper part of the
#'      ancestry match matrix.  Symmetrical relationships are considered to be M1.
#'    - `zone`: The abbreviation for the relationship (e.g., Se, PO, Si, etc.)
#'    - `xmin`: The left-hand x value of the zone.
#'    - `xmax`: The right-hand x value of the zone.
#'    - `ymin`: The bottom y value of the zone.
#'    - `ymax`: The top y value of the zone.
#'    - `area`: The area in unit squares of the zone.
#'    - `xmid`: The x midpoint of the zone.
#'    - `ymid`: The y midpoint of the zone.
#'
#' @export
#' @examples
#' relationship_zone_perimeters()
relationship_zone_perimeters <- function() {
  lapply(relationship_zone_names, function(R) {
    tmp_tib <- lapply(anc_match_masks(4, R), function(m) {
      xmin <- min(which(apply(m, 2, function(x) any(x==TRUE))))
      xmax <- max(which(apply(m, 2, function(x) any(x==TRUE))))
      ymin <- min(which(apply(m, 1, function(x) any(x==TRUE))))
      ymax <- max(which(apply(m, 1, function(x) any(x==TRUE))))

      tibble(
        zone = R,
        xmin = xmin - 0.5,
        xmax = xmax + 0.5,
        ymin = ymin - 0.5,
        ymax = ymax + 0.5
      )
    }) %>%
      bind_rows(.id = "which_matrix")

    if(R %in% c("Se", "Si", "FC", "SC", "TC")) {
      tmp_tib <- tmp_tib[1,]
    }
    tmp_tib
  }) %>%
    bind_rows() %>%
    mutate(
      which_matrix = paste0("M", which_matrix),
      area = (xmax - xmin) * (ymax - ymin),
      xmid = (xmax + xmin) / 2,
      ymid = (ymax + ymin) / 2,
    )

}

