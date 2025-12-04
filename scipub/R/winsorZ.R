#' Winsorize outliers based on z-score cutoff to next
#' most extreme non-outlier value
#'
#' The `winsorZ` function identifies outliers based on Z-score cutoff
#' and replaces with the next most extreme non-outlier value.
#' This involves z-scoring the variable and identifying/replacing
#'  any cases beyond the z-score threshold.
#' The `winsorZ_find` function is an optional companion
#' to flag any Z-score outliers to tally as needed.
#' @param x The input variable to Winsorize.
#' @param zbound The Z-score cutoff (default=3, i.e. outliers are Z>3 | Z<-3).
#' @return Output Winsorized variable
#' @export
#' @examples
#' winsorZ(psydat$iq)
#' \dontrun{
#' psydat %>%
#'   dplyr::select(c(iq, anxT)) %>%
#'   map(winsorZ)
#' psydat %>% mutate_at(c("iq", "anxT"), list(~ winsorZ(.)))
#' psydat %>% mutate_if(is.double, list(~ winsorZ(.)))
#' }
#'
winsorZ <- function(x,
                    zbound = 3) {
  x <- as.numeric(as.character(x)) # convert to numeric just in case
  z <- scale(x) # gives z-scores for vector x
  x[!is.na(z) & z > zbound] <- max(x[!is.na(z) & z <= zbound])
  x[!is.na(z) & z < (-1 * zbound)] <- min(x[!is.na(z) & z >= (-1 * zbound)])
  return(x)
}
