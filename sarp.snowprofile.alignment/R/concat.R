#' Concatenate time series of average profiles
#'
#' This is useful in operations to update a time series that was computed in the past
#' with a newly computed average time series. The routine merges all entries with duplicated
#' entries (read dates) being taken from `avgSP2`.
#'
#' @param avgSP1 old time series of average profiles as returned by [averageSPalongSeason]
#' @param avgSP2 new time series of average profiles as returned by [averageSPalongSeason]
#'
#' @seealso [averageSPalongSeason]
#' @author fherla
#' @export
concat_avgSP_timeseries <- function(avgSP1, avgSP2) {

  ## Delete all entries (dates) in avgSP1 that are contained in avgSP2
  k_rm <- which(avgSP1$meta$date >= min(avgSP2$meta$date))
  if (length(k_rm) == nrow(avgSP1$meta)) {
    stop("All entries from avgSP1 are outdated and contained in avgSP2 already!")
  }
  avgSP1$avgs[k_rm] <- NULL
  try({avgSP1$sets[k_rm] <- NULL})
  avgSP1$meta <- avgSP1$meta[-k_rm, ]

  ## return concatenated object
  OUT <- list(
    avgs = snowprofileSet(c(avgSP1$avgs, avgSP2$avgs)),
    sets = c(avgSP1$sets, avgSP2$sets),
    call = c(avgSP1$call, avgSP2$call),
    meta = meta <- rbind(avgSP1$meta, avgSP2$meta)
  )

  class(OUT) <- append("avgSP_timeseries", class(OUT))

  return(OUT)
}
