#' Check package history on CRAN
#'
#' It uses available information to provide the most accurate information of
#' CRAN at any given time.
#'
#' Several sources are used: CRAN's database to check packages files and versions,
#' CRAN's volunteers actions for when packages are archived or removed and
#' CRAN's comments to fill in the gaps.
#' @inheritParams base_alias
#' @inheritParams cran_alias
#' @returns A data.frame with the information to recreate CRAN at any point before today.
#' `NA` if not able to collect the data from CRAN.
#' @export
#' @seealso [cran_archive()], [cran_actions()], [cran_comments()].
#' @family meta info from CRAN
#' @examples
#' cran_history
cran_history <- function(packages = NULL) {
    history <- save_state("cran_history", cran_all_history())
    if (is_not_data(history)) {
        return(NA)
    }
    check_packages(packages, NA)
    get_package_subset("cran_history", packages)
}

cran_all_history <- function() {
    archive <- save_state("full_cran_archive", cran_archive())
    if (is_not_data(archive)) {
        return(NA)
    }
    actions <- save_state("full_cran_actions", cran_actions())
    if (is_not_data(actions)) {
        return(NA)
    }
    # comments <- save_state("cran_comments", cran_comments())

    archive$Date <- strftime(archive$Datetime, "%F")
    archive$Time <- strftime(archive$Datetime, "%T")
    dup_arch <- duplicated(archive[, c("Package", "Version")])
    arch <- archive[!dup_arch, ]

    # Package published twice with same version archive$Package[dup_arch]: BoundaryStats, discourseGT, lilikoi, pARccs, ScriptMapR, VSURF
    m0 <- merge(actions, arch,
                all = TRUE, by = c("Version", "Package"),
                sort = FALSE, suffixes = c("", ".archive"))
    m0$moment <- datetime2POSIXct(m0$Date, m0$Time)
    m0 <- sort_by(m0, m0[, c("Package", "moment", "Action")])

    # Published date is later than what the archive says
    k4 <- m0$Action == "publish" & m0$Date > m0$Date.archive & !is.na(m0$Date) & !is.na(m0$Date.archive)
    ## There are some packages that were published twice
    # Do not use the date, time of the duplicated archive info
    dup_a <- duplicated(m0[, c("Package", "Version", "Action")])
    k4[dup_a] <- FALSE

    ## others the actions file Date is incorrect.
    m0$Date[k4] <- m0$Date.archive[k4]
    m0$Time[k4] <- m0$Time.archive[k4]

    # Fill the gaps from previous action if possible
    m0$moment <- datetime2POSIXct(m0$Date, m0$Time)
    m0 <- sort_by(m0, m0[, c("Package", "moment", "Action")])

    # Use version of the previously published package
    wo_version <- which(is.na(m0$Version))
    diff_pkg <- m0$Package[wo_version] == m0$Package[(wo_version - 1)]
    k2 <- m0$Action[wo_version] == "archive" & m0$Action[(wo_version - 1)] == "publish"
    m0$Version[wo_version[diff_pkg & k2]] <- m0$Version[(wo_version[diff_pkg & k2] - 1)]



    # Some packages have an extra archive that adds a year: I assume this was a problem with the script
    wov <- wo_version[!k2]
    # p <- m0$Package[wov]
    # table(m0$Action[m0$Package %in% p] == "publish",
    #       is.na(m0$Version[m0$Package %in% p]))
    off_by_year <- abs(m0$Date[wov] - m0$Date[wov - 1]) == 365L
    m0 <- m0[-na.omit(wov[off_by_year]), ]


    published <- actions[actions$Action == "publish",  , drop = FALSE]
    published$Action <- NULL
    colnames(published)[1L:3L] <- paste0(colnames(published)[1L:3L], ".Pub")

    # TODO: Add packages with archived history but no publish entry
    # m0[!is.na(m0$Date.archive), ]

    archived <- m0[m0$Action == "archive", c("Date", "Time", "User", "Version", "Package") , drop = FALSE]
    colnames(archived)[1L:3L] <- paste0(colnames(archived)[1L:3L], ".Arch")

    m <- merge(published, archived, all = TRUE, sort = FALSE)
    m$Pub.Date <- datetime2POSIXct(m$Date.Pub, m$Time.Pub)
    m$Arch.Date <- datetime2POSIXct(m$Date.Arch, m$Time.Arch)
    m <- sort_by(m, m[, c("Package", "Pub.Date", "Arch.Date")])

    # TODO: Find the dates of previous publish actions to use as archive date
    # lapply(unique(m$package), function(i, data) {
    #     p <- data[data$Package == i, , drop = FALSE]
    #     if (NROW(p) <= 1L) {
    #         return(p)
    #     }
    #     arch_i <- which(!is.na(p$Date.Arch))
    #     p$Date.Arch[-NROW(p)] <- p$Date.Pub[-1L]
    # }, data = m)
    m$Date.Arch
    m$Date.Pub[-1L]

    removed <- actions[actions$Action == "remove",  , drop = FALSE]
    removed$Action <- NULL
    colnames(removed)[1:3] <- paste0(colnames(removed)[1:3], ".rm")
    m2 <- merge(m, removed, all = TRUE, sort = FALSE)
    m2$rm.Date <- datetime2POSIXct(m2$Date.rm, m2$Time.rm)

    # TODO: Apply the remove date to all archives
    pkg_state[["cran_history"]] <- m2
    m2
}

warnings_history <- function(x){
    first_package <- !duplicated(x$Package)
    w <- sum(first_package & (x$Action == "archive" | is.na(x$Action)))
    warning("There are ", w, " packages starting with an archive action!",
            call. = FALSE)
}
