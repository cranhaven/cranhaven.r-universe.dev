#' @title \code{filterCoAccessibleLinks}
#'
#' @description \code{filterCoAccessibleLinks} will filter the output from
#'   getCoAccessibleLinks by a threshold, retaining links with a
#'   absolute correlation greater than the threshold.
#'   This function also adds the chr, start, and end site of each link to
#'   the output table.
#'
#'
#' @param TileCorr The correlation table output from getCoAccessibleLinks
#' @param threshold Keep
#'
#' @return FilteredTileCorr The filtered correlation table with chr,
#'   start, and end site of each link
#'
#' @examples
#' \dontrun{
#' # links is the output of MOCHA::getCoAccessibleLinks
#' MOCHA::filterCoAccessibleLinks(links, threshold = 0.5)
#' }
#'
#' @export
filterCoAccessibleLinks <- function(TileCorr, threshold = 0.5) {
  if (!any(abs(TileCorr$Correlation) > threshold)) {
    stop("Error: There are no values above the threshold.")
  }

  FilteredTileCorr <- TileCorr[abs(TileCorr$Correlation) > threshold, ]
  start1 <- as.numeric(gsub("chr.*\\:|\\-.*", "", FilteredTileCorr$Tile1))
  end1 <- as.numeric(gsub("chr.*\\:|.*\\-", "", FilteredTileCorr$Tile1))

  start2 <- as.numeric(gsub("chr.*\\:|\\-.*", "", FilteredTileCorr$Tile2))
  end2 <- as.numeric(gsub("chr.*\\:|.*\\-", "", FilteredTileCorr$Tile2))

  FilteredTileCorr$chr <- gsub("\\:.*", "", FilteredTileCorr$Tile2)
  FilteredTileCorr$start <- apply(data.table(start1, start2), 1, min)
  FilteredTileCorr$end <- apply(data.table(end1, end2), 1, max)

  return(FilteredTileCorr)
}
