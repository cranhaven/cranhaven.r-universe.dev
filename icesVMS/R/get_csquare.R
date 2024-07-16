#' Download C-square information
#'
#' Download a data.frame of information on a set of C-squares.
#'
#' @param c_square character 0.05 degree c-square name
#' @param stat_rec ICES statistical rectangle
#' @param ices_area ICES area
#' @param ecoregion ICES ecoregion
#' @param convert2sf logical, default FALSE, should an simple features object
#'   be returned if the \code{sf} package is installed?
#'
#' @return a data.frame of VMS data
#'
#' @details
#'
#' If the \code{sf} package is installed then a simple features object
#' will be returned, if convert2sf flag is set to TRUE.
#'
#' @examples
#' \donttest{
#' sq40F3 <- get_csquare(stat_rec = "40F3")
#'
#' # if the sf package is installed, an simple feature object will be returned
#'  NS <- get_csquare(ecoregion = "Greater North Sea", convert2sf = TRUE)
#'  plot(NS["ices_area"], border = "transparent")
#' }
#' @export
get_csquare <- function(c_square, stat_rec, ices_area, ecoregion, convert2sf = FALSE) {

  if (!missing(ecoregion)) {
    check_ecoregion(ecoregion)
  }

  args <- lapply(as.list(match.call())[-1], eval, parent.frame())
  if (any(sapply(args, length) > 1)) {
    args_grid <- do.call(expand.grid, c(args, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))

    args_list <- lapply(1:nrow(args_grid), function(i) as.list(args_grid[i, , drop = FALSE]))

    message("running ", length(args_list), " calls to get_csquare().")
    out <- lapply(args_list, function(args) do.call(get_csquare, args))

    return(do.call(rbind, out))
  }

  url <- do.call(vms_api, c(list(service = "csquares"), args))

  out <- vms_get(url)

  if (convert2sf) {
    convert_df2sf(out)
  } else {
    out
  }
}
