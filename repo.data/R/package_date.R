#' Find earliest date of compatibility
#'
#' Search the DESCRIPTION file for the release dates of dependencies and return the earliest date according to CRAN's archive.
#' This is the date at which the package could be installed.
#'
#' Currently this function assumes that packages only use ">=" and not other operators.
#' This might change on the future if other operators are more used.
#' @param packages Path to the package folder and/or name of packages published.
#' @inheritParams tools::package_dependencies
#'
#' @returns A vector with the datetimes of the published package (or current
#' date if not published) and the datetime when the requirements were met.
#' `NA` if not able to collect the data from CRAN.
#' @export
#' @family utilities
#' @examples
#' package_date("ABACUS")
#' package_date("paramtest")
#' package_date("Seurat") # Dependencies on packages not on CRAN
#' package_date("afmToolkit") # Dependency was removed from CRAN
package_date <- function(packages = ".", which = "strong") {
    fields <- check_which(which)
    is_local_pkg <- check_local(packages)

    # Get package dependencies.
    deps_df <- NULL
    # Dependencies of local packages
    if (any(is_local_pkg)) {
        desc <- get_from_local_pkg(packages[is_local_pkg],
                                   fields = c(PACKAGE_FIELDS, "Package"))
        deps <- desc[, intersect(fields, colnames(desc)), drop = FALSE]
        rownames(deps) <- desc[, "Package"]
        date_package <- Sys.Date()
        deps_df <- packages_dependencies(deps)
        deps_df <- repos_dependencies(deps_df$Name[deps_df$Type %in% fields, ],
                                      which = fields)
    }

    # Dependencies of remote packages
    if (any(!is_local_pkg)) {
        rd <- repos_dependencies(packages[!is_local_pkg], which = fields)
        if (is_not_data(rd)) {
            return(NA)
        }
        deps_df <- rbind(deps_df, rd)
    }

    # Clean packages required
    all_packages <- setdiff(unique(c(packages, deps_df$Name)), c(BASE, "R"))
    deps_df <- deps_df[!deps_df$Name %in% BASE, , drop = FALSE]

    which_r <- deps_df$Name == "R"
    # TODO replace by a repository independent function: cran_archive -> repos_archive(?)
    ca <- cran_archive(all_packages)
    if (is_not_data(ca)) {
        return(NA)
    }
    # Max release date across packages requested: minimal date when this was possible.
    date_package <- max(ca$Datetime[ca$Package %in% packages])
    r_versions <- check_installed("rversions")
    if (sum(which_r) && !r_versions) {
        warning("To take into consideration R versions too please install package rversions.")
    }
    r_ver_date <- NULL
    # Find release date of R version
    if (sum(which_r) && r_versions) {
        rver <- versions()
        if (is_not_data(rver)) {
            return(NA)
        }

        max_version <- max(deps_df$Version[which_r])
        r_ver_date <- rver$date[package_version(rver$version) >= package_version(max_version)][1L]
        ca <- filter_arch_date(ca, r_ver_date)
    }

    if (is_local_pkg && length(is_local_pkg) == 1L && NROW(deps_df) == 0L || NROW(deps_df) == 1L && r_versions) {
        return(c(Published = date_package, deps_available = NA))
    } else if (!is_local_pkg && NROW(ca) == 0L) {
        stop("Package ", sQuote(packages), " wasn't found on past or current CRAN archive or locally.")
    }

    # Use cran_archive, to get the release dates of packages.
    missing_packages <- setdiff(all_packages, ca$Package)

    # abn depends on INLA that is an Additional_repositories
    if (length(missing_packages)) {
        warning("Package publication date could not be obtained for: ",
                toString(missing_packages), ".\n",
                "This indicate packages outside CRAN repository.", call. = FALSE,
                immediate. = TRUE)
    }
    remaining_packages <- setdiff(all_packages, missing_packages)
    # TODO: Merge and filter the data
    out <- filter_arch_ver(deps_df[!which_r, , drop = FALSE],
                           ca[ca$Package %in% remaining_packages, , drop = FALSE])

    date_deps <- max(c(r_ver_date, as.Date(out$Datetime)))

    # Get the latest date.
    c(Published = date_package, deps_available = date_deps)
}


#' Package dates
#'
#' Same as package_date but using CRAN's actions instead of public archive.
#'
#' This provides information about when a package was removed or archived for a
#' more accurate estimation.
#' @param packages Name of the package on CRAN.
#' It accepts also local path to packages source directories but then the
#' function works as if the package is not released yet.
#' @inheritParams tools::package_dependencies
#' @keywords internal
# @examples
# # package_date_actions("afmToolkit")
package_date_actions <- function(packages = ".", which = "strong") {
    fields <- check_which(which)
    is_local_pkg <- check_local(packages)

    # Get package dependencies.
    deps_df <- NULL
    if (any(is_local_pkg)) {
        local_ap <- get_from_local_pkg(packages[is_local_pkg],
                                   fields = c(fields, "Package"))
        deps <- local_ap[, intersect(fields, colnames(local_ap)), drop = FALSE]
        date_package <- Sys.Date()
        deps_df <- rbind(deps_df, packages_dependencies(deps))
    }
    if (any(!is_local_pkg)) {
        rd <- repos_dependencies(which = fields)
        deps_df <- rbind(deps_df, rd[rd$package == packages, , drop = FALSE])
        p <- cran_archive(packages)
        date_package <- p$Datetime[NROW(p)]
    }


    # We don't need base packages
    deps_df <- deps_df[!deps_df$name %in% BASE, , drop = FALSE]
    which_r <- deps_df$name == "R"

    if (sum(which_r) && !check_installed("rversions")) {
        warning("To take into consideration R versions too please install package rversions.",
                call. = FALSE)
    }
    r_versions <- sum(which_r) && check_installed("rversions")

    if (!is_local_pkg && NROW(deps_df) == 0L || NROW(deps_df) == 1L && r_versions) {
        return(c(Published = date_package, deps_available = NA))
    } else if (!is_local_pkg && NROW(p) == 0L) {
        stop("Package ", sQuote(packages), " wasn't found on past or current CRAN archive or locally.",
             call.  = FALSE)
    }

    # Use cran_actions, to get the release dates of packages.
    actions <- cran_actions()
    actions$Package <- as.character(actions$Package)
    actions <- actions[actions$Package %in% deps_df$name[!which_r], , drop = FALSE]
    actions$Datetime <- datetime2POSIXct(actions$Date, actions$Time)
    missing_packages <- setdiff(deps_df$name[!which_r], as.character(actions$Package))

    # abn depends on INLA that is an Additional_repositories
    if (length(missing_packages)) {
        warning("Package publication date could not be obtained for: ",
                toString(missing_packages), ".\n",
                "This indicate packages outside CRAN repository.", call. = FALSE,
                immediate. = TRUE)
    }

    # Filter to those that were available at the time
    # browser()
    diff_time <- difftime(actions$Datetime, date_package)
    pkg_available <- actions[sign(diff_time) < 0, , drop = FALSE]
    not_on_actions <- setdiff(deps_df$name, c(pkg_available$Package, "R"))

    if (length(not_on_actions)) {
        warning("Package's not available on the version of actions used ",
                toString(sQuote(not_on_actions)),
                call. = FALSE, immediate. = FALSE)
    }

    # Get versions required or initial package release date
    pkg_available$Version <- as.character(pkg_available$Version)
    ver_match <- merge(pkg_available, deps_df[!which_r, , drop = FALSE], sort = FALSE,
                       by.x = c("Package", "Version"), by.y = c("name", "version"))
    m_vm <- match(ver_match$Package, deps_df$name)
    deps_df$Datetime <- as.POSIXct(NA, tz = cran_tz)
    if (length(m_vm)) {
        deps_df$Datetime[m_vm] <- ver_match$Datetime
    }

    # Add date to those not version specified
    pkg_no_ver_match <- deps_df$name[setdiff(seq_len(NROW(deps_df)), m_vm)]
    pkg_no_ver_match <- setdiff(pkg_no_ver_match, c(not_on_actions, "R"))
    if (length(pkg_no_ver_match)) {
        ver_no_match <- pkg_available[pkg_available$Package %in% pkg_no_ver_match, , drop = FALSE]
        ver_no_match <- ver_no_match[!duplicated(ver_no_match$Package, fromLast = TRUE), ,
                                     drop = FALSE]
        m_vnm <- match(ver_no_match$Package, deps_df$name)
        deps_df$Datetime[m_vnm] <- ver_no_match$Datetime
    }

    # Find release date of R version
    if (r_versions) {
        rver <- versions()
        if (is_not_data(rver)) {
            return(NA)
        }
        ver_position <- match(deps_df$version[which_r], package_version(rver$version))
        deps_df$Datetime[which_r] <- rver$date[ver_position]
    }

    # Get the latest date.
    c(Published = date_package, deps_available = max(deps_df$Datetime, na.rm = TRUE))
}
