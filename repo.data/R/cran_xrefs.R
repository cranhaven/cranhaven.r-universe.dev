#' CRAN's links
#'
#' Retrieve links on CRAN packages' R documentation files.
#' @inheritParams base_alias
#' @returns A data.frame with the links on CRAN's packages.
#' It has 4 columns: Package, Anchor, Target and Source.
#' `NA` if not able to collect the data from CRAN.
#' @family links from CRAN
#' @family meta info from CRAN
#' @seealso The raw source of the data is: \code{\link[tools:CRAN_rdxrefs_db]{CRAN_rdxrefs_db()}}.
#' @export
#' @examples
#' cl <- cran_links("CytoSimplex")
#' head(cl)
cran_links <- function(packages = NULL) {
    stopifnot("Requires at least R 4.5.0" = check_r_version())
    raw_xrefs <- save_state("cran_rdxrefs", tools::CRAN_rdxrefs_db())
    if (is_not_data(raw_xrefs)) {
        return(NA)
    }
    check_packages(packages, NA)
    env <- "full_cran_rdxrefs"
    # Check for random packages
    current_packages <- names(raw_xrefs)
    omit_pkg <- setdiff(packages, current_packages)
    if (length(omit_pkg)) {
        warning("Omitting packages ", toString(omit_pkg),
                ".\nMaybe they are currently not on CRAN?", immediate. = TRUE)
    }
    # Keep only packages that can be processed
    packages <- setdiff(packages, omit_pkg)
    if (!is.null(packages) && !length(packages)) {
        return(NULL)
    }

    # Check if there is already data
    first_xrefs <- empty_env(env)
    if (first_xrefs) {
        xrefs <- NULL
    } else {
        xrefs <- pkg_state[[env]]
    }

    # Decide which packages are to be added to the data
    if (!is.null(packages) && !first_xrefs) {
        new_packages <- setdiff(packages, xrefs[, "Package"])
    } else if (!is.null(packages) && first_xrefs) {
        new_packages <- intersect(packages, current_packages)
    } else if (is.null(packages) && first_xrefs) {
        new_packages <- current_packages
    } else if (is.null(packages) && !first_xrefs) {
        new_packages <- setdiff(current_packages, xrefs[, "Package"])
    }

    # Add new package's data
    if (length(new_packages)) {
        new_xrefs <- xrefs2df(raw_xrefs[new_packages])
        # warnings_links(new_xrefs)
        xrefs <- rbind(xrefs, new_xrefs)
        pkg_state[[env]] <- xrefs[, c("Package", "Source", "Anchor", "Target")]
    }
    if (is.null(packages)) {
        as.data.frame(xrefs)
    } else {
        as.data.frame(xrefs[xrefs[, "Package"] %in% packages, , drop = FALSE])
    }
}

#' Links between help pages by target
#'
#' Explore the relationship between CRAN packages and other help pages by the target they use.
#' @inheritParams base_alias
#' @family links from CRAN
#' @returns A data.frame with 6 columns: from_pkg, from_Rd, to_pkg, to_target, to_Rd, n (Number of links).
#' @export
#' @examples
#' ctl <- cran_targets_links("BaseSet")
#' head(ctl)
cran_targets_links <- function(packages = NULL) {
    out <- NULL
    env <- "cran_targets_links"
    first_call <- empty_env(env)
    check_packages(packages, NA)

    current_packages <- if (!first_call) {
        out <- pkg_state[[env]]
        unique(out$from_pkg)
    } else {
        NULL
    }

    if (!is.null(current_packages)) {

        omit_pkg <- setdiff(packages, current_packages)
        if (length(omit_pkg)) {
            warning("Omitting packages ", toString(omit_pkg),
                    ".\nMaybe they are currently not on CRAN?", immediate. = TRUE)
        }

        # Keep only packages that can be processed
        packages <- setdiff(packages, omit_pkg)
    }

    if (!is.null(packages) && !length(packages)) {
        return(NULL)
    }

    # Decide which packages are to be added to the data
    if (!is.null(packages) && !first_call) {
        new_packages <- setdiff(packages, current_packages)
    } else if (is.null(packages) && !first_call) {
        new_packages <- setdiff(cran_packages(), current_packages)
    } else if (first_call) {
        new_packages <- packages
    }

    # Search only the links from packages that are valid
    if (is.null(packages)) {
        deps <- c(cran_packages(), BASE)
    } else {
        ap <- tryCatch(available.packages(filters = c("CRAN", "duplicates")), warning = function(w){NA})
        if (is_not_data(ap)) {
            return(NA)
        }
        deps <- tools::package_dependencies(packages, db = ap)
    }
    deps <- unique(c(packages, funlist(deps)))


    # Get the packages
    bal <- base_alias(intersect(deps, c(BASE, "R")))
    if (is_not_data(bal)) {
        return(NA)
    }
    cran_pkgs <- setdiff(deps, c(BASE, "R"))
    cal <- cran_alias(cran_pkgs)
    if (is_not_data(cal)) {
        return(NA)
    }
    cl <- cran_links(cran_pkgs)
    if (is_not_data(cl)) {
        return(NA)
    }
    t2b2 <- targets2files(split_anchor(cl), rbind(bal, cal))

    if (length(new_packages)) {
        new_out <- packages_in_links(t2b2, new_packages)

        new_out <- add_uniq_count(t2b2)
        pkg_state[[env]] <- rbind(out, new_out)
    }

    out_fun <- t2b2[t2b2$from_pkg %in% packages, , drop = FALSE]
    return(add_uniq_count(out_fun))

    # Add new package's data
    if (length(new_packages)) {
        raw_xrefs <- save_state("cran_rdxrefs", tools::CRAN_rdxrefs_db())
        if (is_not_data(raw_xrefs)) {
            return(NA)
        }
        new_xrefs <- xrefs2df(raw_xrefs[new_packages])
        # warnings_links(new_xrefs)
        xrefs <- pkg_state[["full_cran_rdxrefs"]]
        xrefs <- rbind(xrefs, new_xrefs)
        pkg_state[[env]] <- xrefs[, c("Package", "Source", "Anchor", "Target")]
    }
    if (is.null(packages)) {
        as.data.frame(xrefs)
    } else {
        as.data.frame(xrefs[xrefs[, "Package"] %in% packages, , drop = FALSE])
    }

    new_out <- out[out$from_pkg %in% new_packages, , drop = FALSE]
    out <- save_state(env, out, verbose = FALSE)
    if (!is.data.frame(out) && !is.matrix(out)) {
        return(NA)
    } else if (!is.null(packages)) {
        out <- out[out$from_pkg %in% packages | out$to_pkg %in% packages, ]
        rownames(out) <- NULL
        out
    } else {
        out
    }
}

#' Links between help pages by page
#'
#' Explore the relationship between CRAN packages and other help pages.
#' If the target help page is ambiguous it is omitted.
#' @inheritParams base_alias
#' @family links from CRAN
#' @returns A data.frame with 6 columns: from_pkg, from_Rd, to_pkg, to_Rd, n (Number of links).
#' @export
#' @examples
#' cpl <- cran_pages_links("Matrix")
#' head(cpl)
cran_pages_links <- function(packages = NULL) {
    check_packages(packages, NA)

    target_links <- cran_targets_links(packages)
    if (is_not_data(target_links)) {
        return(NA)
    }

    w <- which(colnames(target_links) %in% "to_target")
    keep_rows <- nzchar(target_links$to_pkg)
    if (!is.null(packages)) {
        keep_rows <- keep_rows & (target_links$from_pkg %in% packages | target_links$to_pkg %in% packages)
    }
    add_uniq_count(target_links[keep_rows, -w])
}

#' Links between help pages by package
#'
#' Explore the relationship between CRAN packages and other packages.
#' If the target package is ambiguous it is omitted.
#' @inheritParams base_alias
#' @family links from CRAN
#' @returns A data.frame with 6 columns: from_pkg, to_pkg, n (Number of links).
#' `NA` if not able to collect the data from CRAN.
#' @export
#' @examples
#' \donttest{
#' cpkl <- cran_pkges_links()
#' head(cpkl)
#' }
cran_pkges_links <- function(packages = NULL) {
    target_links <- save_state("cran_pages_links", base_targets_links())
    if (is_not_data(target_links)) {
        return(NA)
    }
    check_packages(packages, NA)
    w <- which(!colnames(target_links) %in% c("from_pkg", "to_pkg"))
    keep_rows <- nzchar(target_links$to_pkg)
    if (!is.null(packages)) {
        keep_rows <- keep_rows & target_links %in% packages
    }
    add_uniq_count(target_links[keep_rows, -w])
}
