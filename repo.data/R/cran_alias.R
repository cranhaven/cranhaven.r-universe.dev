#' CRAN's alias
#'
#' Retrieve alias available on CRAN.
#' @inheritParams base_alias
#' @returns A data.frame with three columns: Package, Source and Target.
#' `NA` if not able to collect the data from CRAN.
#' @export
#' @family alias
#' @seealso The raw source of the data is: \code{\link[tools:CRAN_aliases_db]{CRAN_aliases_db()}}.
#' @family meta info from CRAN
#' @examples
#' ca <- cran_alias("BWStest")
#' head(ca)
cran_alias <- function(packages = NULL) {
    stopifnot("Requires at least R 4.5.0" = check_r_version())
    stopifnot("NULL or a character string" = is.null(packages) || is.character(packages))
    raw_alias <- save_state("cran_aliases", tools::CRAN_aliases_db())
    if (is_not_data(raw_alias)) {
        return(NA)
    }
    check_packages(packages, NA)
    # Place to store modified data
    env <- "full_cran_aliases"
    # Check for random packages
    current_packages <- names(raw_alias)
    omit_pkg <- setdiff(packages, current_packages)
    if (length(omit_pkg)) {
        warning("Omitting packages ", toString(omit_pkg),
                ".\nMaybe they are currently not on CRAN?", immediate. = TRUE,
                call. = FALSE)
    }
    # Keep only packages that can be processed
    packages <- setdiff(packages, omit_pkg)
    if (!is.null(packages) && !length(packages)) {
        return(NULL)
    }

    # Check if there is already data
    first_alias <- empty_env(env)
    if (first_alias) {
        alias <- NULL
    } else {
        alias <- pkg_state[[env]]
    }

    # Decide which packages are to be added to the data
    if (!is.null(packages) && !first_alias) {
        new_packages <- setdiff(packages, alias[, "Package"])
    } else if (!is.null(packages) && first_alias) {
        new_packages <- intersect(packages, current_packages)
    } else if (is.null(packages) && first_alias) {
        new_packages <- current_packages
    } else if (is.null(packages) && !first_alias) {
        new_packages <- setdiff(current_packages, alias[, "Package"])
    }

    # Add new package's data
    if (length(new_packages)) {
        new_alias <- alias2df(raw_alias[new_packages])
        warnings_alias(new_alias)
        alias <- rbind(alias, new_alias)
        pkg_state[[env]] <- alias[, c("Package", "Source", "Target")]
    }
    if (is.null(packages)) {
        as.data.frame(alias)
    } else {
        as.data.frame(alias[alias[, "Package"] %in% packages, , drop = FALSE])
    }
}
