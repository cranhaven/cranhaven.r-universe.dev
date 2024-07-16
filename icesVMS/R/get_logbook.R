#' Download Logbook data
#'
#' RESTRICTED.  Only core members of the ICES VMS data call can access this data.
#' Download a data.frame of VMS data from the ICES VMS and logbook database.
#'
#' @param country country code
#' @param year integer year
#' @param month integer month
#' @param gear_code benthis gear code
#' @param metier level 6 metier code
#' @param stat_rec ICES statistical rectangle
#' @param ices_area ICES area
#' @param ecoregion ICES ecoregion
#'
#' @return a data.frame of VMS data
#'
#' @examples
#' \dontrun{
#' # requires authorization
#' logbook <- get_logbook(country = "DK", year = 2021, month = 1)
#' }
#' 
#' @export
get_logbook <- function(country, year, month,
                    gear_code, metier,
                    stat_rec, ices_area, ecoregion) {

  if (!missing(ecoregion)) {
    check_ecoregion(ecoregion)
  }
  
  args <- lapply(as.list(match.call())[-1], eval, parent.frame())
  if (any(sapply(args, length) > 1)) {
    args_grid <- do.call(expand.grid, c(args, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))

    args_list <- lapply(1:nrow(args_grid), function(i) as.list(args_grid[i, ]))

    message("running ", length(args_list), " calls to get_logbook().")
    out <- lapply(args_list, function(args) do.call(get_logbook, args))

    return(do.call(rbind, out))
  }

  url <- do.call(vms_api, c("logbook", args))

  vms_get(url, use_token = TRUE)
}
