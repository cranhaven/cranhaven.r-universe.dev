#' Download VMS data
#'
#' RESTRICTED.  Only core members of the ICES VMS data call can access this data.
#' Download a data.frame of VMS swept area ratio values from the ICES VMS and logbook database.
#'
#' @param year integer year
#' @param c_square character 0.05 degree c-square name
#' @param gear_code benthis gear code
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
#' sar <- get_sar(2021, stat_rec = "40F1")
#' }
#' 
#' @export
get_sar <- function(year, c_square,
                    gear_code,
                    stat_rec, ices_area, ecoregion, datacall = NULL) {
  
  if (!missing(ecoregion)) {
    check_ecoregion(ecoregion)
  }
  
  args <- lapply(as.list(match.call())[-1], eval, parent.frame())
  if (any(sapply(args, length) > 1)) {
    args_grid <- do.call(expand.grid, c(args, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))

    args_list <- lapply(1:nrow(args_grid), function(i) as.list(args_grid[i, ]))

    message("running ", length(args_list), " calls to get_sar().")
    out <- lapply(args_list, function(args) do.call(get_sar, args))

    return(do.call(rbind, out))
  }

  url <- do.call(vms_api, c("vms/vmssar", args))

  vms_get(url, use_token = TRUE)
}
