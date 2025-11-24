#' Extract methods
#'
#' WIP extract variables from sspm objects
#'
#' @param x **\[sspm_...\]** An object from this package.
#' @param name **\[character\]** The name of the column
#'
#' @return
#' The `data.frame` matching the request.
#'
#' @examples
#' sfa_boundaries
#' bounds <- spm_as_boundary(boundaries = sfa_boundaries,
#'                           boundary = "sfa")
#' bounds$area_sfa
#'
#' @export
#' @aliases extract
#' @rdname extract-methods
setMethod("$",
          "sspm_boundary",
          function(x, name) {
            x@boundaries %>%
              dplyr::select(c(dplyr::all_of(name), spm_boundary(x), "geometry"))
          }
)

#' @export
#' @rdname extract-methods
setMethod("$",
          "sspm_discrete_boundary",
          function(x, name) {
            x@boundaries %>%
              dplyr::select(c(dplyr::all_of(name), spm_boundary(x), "geometry"))
          }
)

#' @export
#' @rdname extract-methods
setMethod("$",
          "sspm_dataset",
          function(x, name) {
            if (is.null(x@smoothed_data)) {
              x@data %>%
                dplyr::select(c(dplyr::all_of(name), spm_time(x), "geometry"))
            } else {
              x@smoothed_data %>%
                dplyr::select(c(dplyr::all_of(name), spm_time(x), "geometry"))
            }
          }
)

#' @export
#' @rdname extract-methods
setMethod("$",
          "sspm",
          function(x, name) {
            if (is.null(x@smoothed_data)) {
              x@data %>%
                dplyr::select(c(dplyr::all_of(name), spm_time(x), "geometry"))
            } else {
              x@smoothed_data %>%
                dplyr::select(c(dplyr::all_of(name), spm_time(x), "geometry"))
            }
          }
)
