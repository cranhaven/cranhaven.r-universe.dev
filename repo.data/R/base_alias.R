#' Base R's alias
#'
#' Retrieve alias available on R.
#' @param packages A vector with packages or `NULL` for all packages.
#' @returns A data.frame with three columns: Package, Source and Target.
#' `NA` if not able to collect the data from CRAN.
#' @export
#' @family alias
#' @seealso The raw source of the data is: \code{\link[tools:base_aliases_db]{base_aliases_db()}}.
#' @examples
#' \donttest{
#' ba <- base_alias()
#' head(ba)
#' }
base_alias <- function(packages = NULL) {
    stopifnot("Requires at least R 4.5.0" = check_r_version())
    check_packages(packages, NA)
    first <- empty_env("base_aliases") && is.null(packages)
    alias <- save_state("base_aliases",
                               r_os_alias(alias2df(tools::base_aliases_db())))
    if (!is.data.frame(alias) && !is.matrix(alias)) {
        return(NA)
    }
    alias <- get_package_subset("base_aliases", packages)
    if (first) {
        warnings_alias(alias)
    }
    as.data.frame(alias[, c("Package", "Source", "Target")])
}
