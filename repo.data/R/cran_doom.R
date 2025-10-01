#' Calculate time till packages are archived
#'
#' Given the deadlines by the CRAN volunteers packages can be archived which can trigger some other packages to be archived.
#' This code calculates how much time the chain reaction will go on if maintainer don't fix/update the packages.
#'
#' Packages on Suggested: field should
#' @references Original code from: <https://github.com/schochastics/cran-doomsday/blob/main/index.qmd>
#' @inheritParams tools::package_dependencies
#' @param bioc Logical value if Bioconductor packages should be provided,
#' (Requires internet connection).
#' @returns A list with multiple elements:
#'  - time_till_last: Time till last package is affected.
#'  - last_archived: the date of the last package that would be affected.
#'  - npackages: Numeric vector with the number of packages used.
#'  - details: A data.frame with information for each individual package:
#'  Name, date affected, affected directly, repository, times it is affected
#'  (by archival causing issues.)
#' `NA` if not able to collect the data from CRAN.
#' @importFrom utils available.packages
#' @seealso The raw source of the data is: \code{\link[tools:CRAN_package_db]{tools::CRAN_package_db()}}
#' @family utilities
#' @export
#' @examples
#' \donttest{
#' cd <- cran_doom()
#' if (length(cd) > 1L) head(cd$details)
#' }
cran_doom <- function(which = "strong", bioc = FALSE) {
    fields_selected <- check_which(which)

    db <- save_state("CRAN_db", tools::CRAN_package_db())
    if (is_not_data(db)) {
        return(NA)
    }
    db$repo <- "CRAN"
    if (isTRUE(bioc)) {
        bioc <- bioc_available()
        bioc$repo <- "Bioconductor"
        columns <- intersect(colnames(bioc), colnames(db))
        db_all <- rbind(db[, columns], bioc[, columns])
    } else {
        db_all <- db
    }
    danger <- db[!is.na(db$Deadline), c("Package", "Deadline")]
    danger$Deadline <- charToDate(danger$Deadline, "%F")
    tp <- tools::package_dependencies(danger$Package, db = db_all,
                                      which = fields_selected,
                                      reverse = TRUE, recursive = TRUE)
    rev_dep <- names(tp)[lengths(tp) > 0L]
    # Time given by CRAN on the warnings
    # 14 for the first warning
    # 14 for the second (with dependencies added on the email)
    total_time_given <- 14L + 21L
    l <- lapply(rev_dep, function(pkg) {
        data.frame(
        Package = tp[[pkg]],
        Deadline = unique(danger$Deadline[danger$Package == pkg]) + total_time_given)
    })
    df2 <- do.call(rbind, l)
    affected <- table(df2$Package)
    multiple_affected <- names(affected)[affected > 1L]

    df3 <- df2[!df2$Package %in% multiple_affected, ]
    l2 <- lapply(multiple_affected, function(pkg) {
        data.frame(Package = pkg,
                   Deadline = min(df2$Deadline[df2$Package == pkg]))
    })
    df4 <- do.call(rbind, l2)
    # There are packages in direct and indirect danger!!
    indirect <- rbind(df3, df4)
    danger$type <- "direct"
    indirect$type <- "indirect"
    out <- rbind(danger, indirect)
    out$repo <- db_all$repo[match(out$Package, db_all$Package)]

    # Count times a packages is affected by a Deadline
    out$n_affected <- 0L
    out$n_affected[out$type == "direct"] <- 1L
    n_affected <- affected[match(out$Package, names(affected))]
    n_affected[is.na(n_affected)] <- 0L
    out$n_affected <- out$n_affected + as.numeric(n_affected)
    out <- sort_by(out, ~list(Deadline, type, -n_affected, Package))
    rownames(out) <- NULL

    list(time_till_last = max(out$Deadline) - Sys.Date(),
         last_archived = max(out$Deadline),
         npackages = c(CRAN = NROW(db), all = NROW(db_all)),
         details = out)

}
