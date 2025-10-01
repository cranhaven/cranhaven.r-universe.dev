#' Report duplicated alias
#'
#' @param alias The output of [cran_alias()] or [base_alias()]
#'
#' @returns A sorted data.frame with the Target, Package and Source of the duplicate alias.
#' @export
#' @family utilities
#' @examples
#' # Checking the overlap between to seemingly unrelated packages:
#' alias <- cran_alias(c("fect", "gsynth"))
#' if (length(alias) >= 1L) {
#'    dup_alias <- duplicated_alias(alias)
#'    head(dup_alias)
#' }
duplicated_alias <- function(alias) {
    if (is_not_data(alias)) {
        return(NA)
    }
    stopifnot(all(c("Package", "Source", "Target") %in% colnames(alias)))
    da <- dup_alias(alias)
    da[, c("Target", "Package", "Source"), drop = FALSE]
}


#' Create a matrix of alias
#'
#' Joins the matrices of each file
#' @param x A raw alias output as given by CRAN (and Bioconductor).
#' @returns A matrix with Package, Source (The name of the file), and the Target (or Alias)
#' @keywords internal
alias2df <- function(x) {
    if (!length(x)) {
        return(NULL)
    }

    l <- lapply(x, function(x) {
        cbind(Source = rep(names(x), lengths(x)), Target = funlist(x))
    })
    aliasesDF <- do.call(rbind, l)
    aliasesDF <- cbind(aliasesDF, Package = rep(names(l), vapply(l, NROW, numeric(1L))))
    aliasesDF[, c("Package", "Source", "Target"), drop = FALSE]
}

warnings_alias <- function(alias) {

    if (length(unique(alias[, "Package"])) <= 1L) {
        paths <- grepl("/", alias[, "Source"], fixed = TRUE)
        dup_targets <- duplicated(alias[, "Target"])
        more_alias <- sum(paths) > sum(dup_targets) * 2L
        return(more_alias)
    }

    s <- split(as.data.frame(alias), alias[, "Package"])
    more_alias <- vapply(s, warnings_alias, logical(1L))
    names(more_alias) <- names(s)
    # Recursive call
    if (sum(more_alias) > 1L) {
        warning("Packages ", toString(sQuote(names(more_alias)[more_alias])),
                " have targets not present in a OS.", call. = FALSE)
        return(FALSE)
    } else if (sum(more_alias) == 1L) {
        warning("Package ", unique(alias[, "Package"]),
                " has targets not present in a OS.", call. = FALSE)
        return(FALSE)
    }
    TRUE
}

# Add alias using the data from the R source code to fix/clean it
r_os_alias <- function(alias) {
    current_os <- .Platform$OS.type
    missing_alias <- os_alias[, "os"] != current_os
    rbind(alias, os_alias[missing_alias, colnames(alias)])
}

# Internal function to find duplicate alias (and which packages they are in)
dup_alias <- function(alias) {
    df <- duplicated(alias[, "Target"])
    dup_targets <- unique(alias[df, "Target"])
    pkg_df <- alias[alias[, "Target"] %in% dup_targets, ]
    pkg_df <- sort_by(pkg_df, pkg_df[, c("Target", "Package", "Source")])
    rownames(pkg_df) <- NULL
    pkg_df
}

# Join duplicate alias
count_dups <- function(dups) {
    s <- split(dups$Package, dups$Target)
    v <- vapply(s, paste, collapse = ",", FUN.VALUE = character(1L))
    df <- data.frame(Alias = names(s), Packages = v, Times = lengths(s))
    rownames(df) <- NULL
    df
}
