#' Retrieve CRAN archive
#'
#' Retrieve the archive and the current database.
#'
#' Some packages would get an NA in Version, if [package_version()] were to be
#' used with `strict = FALSE`.
#' Packages might have been on CRAN but could have been removed and won't show up.
#' Depending on the data requested and packages currently on CRAN, you might get
#' a warning regarding a package being both archived and current.
#' @inheritParams base_alias
#' @returns A data.frame with 6 columns: Package, Date (of publication), Version,
#'  User, size and status (archived or current).
#'  It is sorted by package name and date.
#' `NA` if not able to collect the data from CRAN.
#' @export
#' @family meta info from CRAN
#' @seealso The raw source of the data is: \code{\link[tools:CRAN_archive_db]{CRAN_archive_db()}},
#' \code{\link[tools:CRAN_current_db]{CRAN_current_db()}}.
#'  For some dates and comments about archiving packages: [cran_comments()].
#' @examplesIf NROW(available.packages())
#' \donttest{
#' ap <- available.packages()
#' if (NROW(ap)) {
#'     a_package <- rownames(ap)[startsWith(rownames(ap), "A")][2]
#'     ca <- cran_archive(a_package)
#'     head(ca)
#' }
#' }
cran_archive <- function(packages = NULL) {
    stopifnot("Requires at least R 4.5.0" = check_r_version())
    check_packages(packages, NA)
    current <- save_state("current", tools::CRAN_current_db(), FALSE)
    if (is_not_data(current)) {
        return(NA)
    }
    archive <- save_state("archive", tools::CRAN_archive_db(), FALSE)
    if (is_not_data(archive)) {
        return(NA)
    }
    env <- "full_cran_archive"
    arch_names <- names(archive)
    curr_names <- gsub("_.+", "", rownames(current)) # Rownames without version
    # Check for random packages
    all_names <- unique(c(arch_names, curr_names))
    omit_pkg <- setdiff(packages, all_names)
    if (length(omit_pkg)) {
        warning("Omitting packages ", toString(omit_pkg),
                ".\nMaybe they were not on CRAN?", immediate. = TRUE)
    }
    # Keep only packages that can be processed
    packages <- setdiff(packages, omit_pkg)
    if (!is.null(packages) && !length(packages)) {
        return(NULL)
    }

    # Check if there is already data
    first_arch <- empty_env(env)
    if (first_arch) {
        arch <- curr2m(current)
    } else {
        arch <- pkg_state[[env]]
    }

    # Packages with archive data to add
    pkgs2add <- setdiff(arch_names, arch[arch[, "status"] != "current", "package"])

    # Decide which packages are to be added to the data
    new_packages <- if (!is.null(packages)) {
        packages
    } else {
        all_names
    }
    new_packages <- intersect(new_packages, pkgs2add)

    # Add new package's data
    if (length(new_packages)) {
        new_arch <- arch2m(archive[new_packages])
        arch <- rbind(arch, new_arch)
        # To be able to detect current and archived versions
        warnings_archive(arch)
        pkg_state[[env]] <- arch
    }

    if (is.null(packages)) {
        arch2df(arch)
    } else {
        arch2df(arch[arch[, "package"] %in% packages, , drop = FALSE])
    }
}


# Like CRAN archive but provides the published date and the date of archival if known
cran_archive_dates <- function() {
    ca <- save_state("full_cran_archive", cran_archive())
    if (is_not_data(ca)) {
        return(NA)
    }
    dates <- split(ca$published_date, ca$package)
    l <- lapply(dates, function(x) {
        c(x[-length(x)] - 1L, NA)
    })
    ca$archived_date <- as.POSIXlt(funlist(l), tz = cran_tz)
    ca$archived_date[ca$status == "current"] <- as.POSIXlt(Sys.time(), tz = cran_tz)
    ca

    # TODO match package version with dates of archival or removal
    cc <- save_state("cran_comments", cran_comments())
    if (is_not_data(cc)) {
        return(NA)
    }
    w <- which(cc$action %in% c("archived", "removed", "replaced", "renamed"))
    cc[w, ]
}


cran_packages <- function(packages = NULL) {
    current <- save_state("current", tools::CRAN_current_db(), FALSE)
    if (is_not_data(current)) {
        return(NA)
    }
    archive <- save_state("archive", tools::CRAN_archive_db(), FALSE)
    if (is_not_data(archive)) {
        return(NA)
    }
    s <- strsplit(rownames(current), "_", fixed = TRUE)
    current_packages <- vapply(s, "[", FUN.VALUE = character(1L), i = 1L)
    archive_packages <- names(archive)
    unique(current_packages, archive_packages)
}
