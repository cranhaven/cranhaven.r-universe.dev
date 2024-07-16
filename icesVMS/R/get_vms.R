#' Download VMS data
#'
#' RESTRICTED.  Only core members of the ICES VMS data call can access this data.
#' Download a data.frame of VMS data from the ICES VMS and logbook database.
#'
#' @param country country code
#' @param year integer year
#' @param month integer month
#' @param c_square character 0.05 degree c-square name
#' @param gear_code benthis gear code
#' @param metier level 6 metier code
#' @param stat_rec ICES statistical rectangle
#' @param ices_area ICES area
#' @param ecoregion ICES ecoregion
#' @param datacall integer year giving which data call year to inquire about.
#'   If NULL returns the a summary of the most recent approved data.
#'
#' @return a data.frame of VMS data
#'
#' @examples
#' \dontrun{
#' # requires authorization
#' vms <- get_vms(country = "DK", year = 2021, month = 1)
#' }
#' 
#' @export
get_vms <- function(country, year, month, c_square,
                    gear_code, metier,
                    stat_rec, ices_area, ecoregion, datacall = NULL) {

  if (!missing(ecoregion)) {
    check_ecoregion(ecoregion)
  }
  
  args <- lapply(as.list(match.call())[-1], eval, parent.frame())
  if (any(sapply(args, length) > 1)) {
    args_grid <- do.call(expand.grid, c(args, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))

    args_list <- lapply(1:nrow(args_grid), function(i) as.list(args_grid[i,]))

    message("running ", length(args_list), " calls to get_vms().")
    out <- lapply(args_list, function(args) do.call(get_vms, args))

    return (do.call(rbind, out))
  }

  url <- do.call(vms_api, c("vms", args))

  vms_get(url, use_token = TRUE)
}
