#' Base R's links
#'
#' Retrieve links on R documentation files.
#' @inheritParams base_alias
#' @returns A data.frame with the links on R's files.
#' It has 4 columns: Package, Anchor, Target and Source.
#' `NA` if not able to collect the data from CRAN.
#' @family links from R
#' @seealso The raw source of the data is: \code{\link[tools:base_rdxrefs_db]{base_rdxrefs_db()}}.
#' @export
#' @examples
#' bl <- base_links()
#' head(bl)
base_links <- function(packages = NULL) {
    stopifnot("Requires at least R 4.5.0" = check_r_version())
    out <- save_state("base_rdxrefs", xrefs2df(tools::base_rdxrefs_db()))
    if (!is.data.frame(out) && !is.matrix(out)) {
        return(NA)
    }
    check_packages(packages, NA)
    links <- get_package_subset("base_rdxrefs", packages)
    as.data.frame(links)[, c("Package", "Source", "Target", "Anchor")]
}

#' Links between help pages by target
#'
#' Explore the relationship between base R packages and other help pages by the target they use.
#' @inheritParams base_alias
#' @family links from R
#' @returns A data.frame with 6 columns: from_pkg, from_Rd, to_pkg, to_target, to_Rd, n (Number of links).
#' `NA` if not able to collect the data from CRAN.
#' @export
#' @examples
#' \donttest{
#' btl <- base_targets_links()
#' head(btl)
#' }
base_targets_links <- function(packages = NULL) {
    out <- NULL
    check_packages(packages, NA)
    out <- save_state("base_targets_links", out, verbose = FALSE)
    if (!is.data.frame(out) && !is.matrix(out)) {
        return(NA)
    }
    if (is.null(out)) {
        bl <- base_links()
        bal <- base_alias()
        cal <- cran_alias()
        bl2 <- split_anchor(bl)

        t2b2 <- targets2files(bl2, rbind(bal, cal))
        out <- uniq_count(t2b2)
        out <- save_state("base_targets_links", out, verbose = FALSE)
    }
    if (!is.null(packages)) {
        packages_in_links(out, packages)
    } else {
        out
    }
}

#' Links between help pages by page
#'
#' Explore the relationship between base R packages and other help pages.
#' If the target help page is ambiguous it is omitted.
#' @inheritParams base_alias
#' @family links from R
#' @returns A data.frame with 6 columns: from_pkg, from_Rd, to_pkg, to_Rd, n (Number of links).
#' `NA` if not able to collect the data from CRAN.
#' @export
#' @examples
#' \donttest{
#' bpl <- base_pages_links()
#' head(bpl)
#' }
base_pages_links <- function(packages = NULL) {
    target_links <- save_state("base_targets_links", base_targets_links())
    if (!is.data.frame(target_links) && !is.matrix(target_links)) {
        return(NA)
    }
    check_packages(packages, NA)
    w <- which(colnames(target_links) %in% "to_target")
    keep_rows <- nzchar(target_links$to_pkg)
    if (!is.null(packages)) {
        keep_rows <- keep_rows & target_links %in% packages
    }
    pages_links <- add_uniq_count(target_links[keep_rows, -w])
}

#' Links between help pages by package
#'
#' Explore the relationship between base R packages and other packages.
#' If the target package is ambiguous it is omitted.
#' @inheritParams base_alias
#' @family links from R
#' @returns A data.frame with 6 columns: from_pkg, to_pkg, n (Number of links).
#' `NA` if not able to collect the data from CRAN.
#' @export
#' @examples
#' \donttest{
#' bpkl <- base_pkges_links()
#' head(bpkl)
#' }
base_pkges_links <- function(packages = NULL) {
    target_links <- save_state("base_targets_links", base_targets_links())
    if (!is.data.frame(target_links) && !is.matrix(target_links)) {
        return(NA)
    }
    check_packages(packages, NA)
    w <- which(!colnames(target_links) %in% c("from_pkg", "to_pkg", "n"))
    keep_rows <- nzchar(target_links$to_pkg)
    if (!is.null(packages)) {
        keep_rows <- keep_rows & target_links %in% packages
    }
    pkges_links <- add_uniq_count(target_links[keep_rows, -w])
    sort_by(pkges_links, pkges_links[, c("from_pkg", "n")])
}
