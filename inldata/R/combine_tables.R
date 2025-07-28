#' Combine Package Datasets
#'
#' @description Combine package datasets that can be coerced into a data frame into a single list.
#'
#' @param package 'character' string.
#'   Package name.
#'
#' @return A list of dataset elements of class 'data.frame'.
#'  A list element is named using its dataset name.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'

combine_tables <- function(package) {

  # check arguments
  checkmate::assert_string(package)

  # combine tables sourced from Rdata files
  if (test_pkg_dir(package)) {
    ds_paths <- list.files("data", pattern = "*.rda", full.names = TRUE)
    ds_names <- basename(ds_paths) |> tools::file_path_sans_ext()
    l <- lapply(ds_paths,
      function(x) {
        env <- environment()
        nm <- load(file = x, envir = env)
        tryCatch(
          {
            env[[nm]] |> as.data.frame()
          },
          error = function(e) {
            return(NULL)
          }
        )
      }
    )
    names(l) <- ds_names
    return(ccp(l))
  }

  # combine tables sourced from package datasets
  ds_names <- utils::data(package = package)$results[, "Item"]
  l <- lapply(ds_names,
    function(x) {
      env <- environment()
      nm <- utils::data(list = x, package = package, envir = env)[1]
      tryCatch(
        {
          env[[nm]] |> as.data.frame()
        },
        error = function(e) {
          return(NULL)
        }
      )
    }
  )
  names(l) <- ds_names
  ccp(l)
}
