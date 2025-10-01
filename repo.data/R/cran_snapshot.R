#' Check CRAN package state on any given date
#'
#' Given the available information which packages were on CRAN on a given date?
#' @note Due to missing of CRAN comments some packages are not annotated when
#' were they archived and more packages than present might be returned for any
#' given date.
#' @param date The date you want to check.
#'
#' @returns The data.frame with the packages and versions at a given date.
#' `NA` if not able to collect the data from CRAN.
#' @export
#' @family utilities
#' @examples
#' \donttest{
#' cs <- cran_snapshot(Sys.Date() -2 )
#' head(cs)
#' }
cran_snapshot <- function(date) {

    stopifnot("Provide a date" = is(date, "Date"),
              "Accepted ranges is from the beginning of CRAN to today" = date <= Sys.Date() || date >= as.Date("1997-10-08"))

    ca <- cran_archive()
    if (date == Sys.Date()) {
        return(ca[ca$Status == "current", , drop = FALSE])
    }
    if (is_not_data(ca)) {
        return(NA)
    }
    ca_before_date <- filter_arch_date(ca, date)

    cc <- cran_comments(ca_before_date[, "Package"])
    if (is_not_data(cc)) {
        return(NA)
    }
    # If date is earlier than any comments return what it was.
    if (date < min(cc$date, na.rm = TRUE)) {
        return(ca_before_date)
    }

    cc_archive <- cc[cc$action %in% c("archived", "removed") & cc$date <= date, ]
    cc_archive <- sort_by(cc_archive, cc_archive[, c("package", "date")])
    dups <- duplicated(cc_archive$package, fromLast = TRUE)
    last_archival <- cc_archive[!dups & !is.na(cc_archive$package), ]

    missing <- setdiff(last_archival$package, ca_before_date$package)
    archived <- match(last_archival$package, ca_before_date$package, incomparables = missing)
    missing2 <- setdiff(ca_before_date$package, last_archival$package)
    # archived2 <- match(ca_before_date$package, last_archival$package, incomparables = missing2)

    on_cran <- rep_len(TRUE, NROW(ca_before_date))
    names(on_cran) <- ca_before_date$package
    on_cran[na.omit(archived)] <- as.Date(ca_before_date$Datetime[na.omit(archived)]) > last_archival$date[!is.na(archived)]
    out <- ca_before_date[on_cran, , drop = FALSE]
    rownames(out) <- NULL
    out
}


#' Estimate CRAN's date of packages
#'
#' Check which CRAN dates are possible for a given packages and versions.
#' @param versions A data.frame with the packages names and versions
#' @param session Session information.
#'
#' @returns Last installation date from CRAN.
#' @export
#' @family utilities
#' @rdname cran_date
#' @importFrom utils installed.packages
#' @examples
#' # ip <- installed.packages()
#' ip <- data.frame(Package = c("A3", "AER"), Version = c("1.0.0", "1.2-15"))
#' cran_date(ip)
cran_date <- function(versions) {
    if (is_not_data(versions) && !all(c("Package", "Version") %in% colnames(versions))) {
        stop("Versions should be a data.frame with 'Package' and 'Version' columns.")
    }
    if (any(versions[, "Package"] %in% BASE)) {
        versions <- versions[!versions[, "Package"] %in% c(BASE, "R"), , drop = FALSE]
    }
    if (is_not_data(versions)) {
        return(NA)
    }

    ca_packages <- cran_archive(versions[, "Package"])
    if (is_not_data(ca_packages)) {
        return(NA)
    }
    versions[, "Version"] <- as.character(versions[, "Version"])
    # match packages names and versions
    ca_v <- apply(as.matrix(ca_packages[, c("Package", "Version")]), 1L, paste, collapse = "_")
    # if version is NA match to whatever
    v_v <- apply(as.matrix(versions[, c("Package", "Version")]), 1L, paste, collapse = "_")
    missing_v <- anyNA(versions[, "Version"])
    if (missing_v) {
        any_v <- is.na(versions[, "Version"])
        ca_p <- ca_packages[ca_packages$Package %in% versions[any_v, "Package"],
                            c("Package", "Datetime")]
        d <- ca_p$Datetime[!duplicated(ca_p$Package)]
    }

    m <- match(v_v, ca_v)

    if (missing_v) {
        d <- c(ca_packages$Datetime[na.omit(m)], d)
    } else {
        d <- ca_packages$Datetime[na.omit(m)]

    }
    # Find range of dates where was last updated.
    max(d, na.rm = TRUE)
}

#' @rdname cran_date
#' @export
#' @importFrom utils sessionInfo
#' @examples
#' cran_session()
cran_session <- function(session = sessionInfo()) {
    if (is(session, "session_info")) {
        versions <- session$packages
        colnames(versions)[1:2] <- c("Package", "Version")
    } else {
        loaded <- lapply(session$loadedOnly, desc2version)
        other <- lapply(session$otherPkgs, desc2version)
        versions <- do.call(rbind, c(loaded, other))
    }
    cran_date(versions)
}


desc2version <- function(x){list2DF(x)[, c("Package", "Version")]}


