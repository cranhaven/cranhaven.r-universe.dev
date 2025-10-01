#' Tidy information about maintainers
#'
#' Make more accessible information about maintainers. Extracts and makes
#' comparable some dates. It also provides the user name used and cleans the
#' names of the maintainer of extra quotes.
#'
#' @details
#' User is what the machine building the package reported. This might indicate
#' some collaboration, repackaging or simply nothing as it can be hidden/modified.
#' The name, email and user might help identify similar named people (or confuse between maintainers)
#'
#' @returns A data.frame with one row per package and 11 columns.
#' The package name, Maintainer field, user maintainer manual date, packaged
#' date, published date, name of maintainer used, email used, direction and domain.
#' `NA` if not able to collect the data from CRAN.
#' @export
#' @seealso The raw source of the data is: \code{\link[tools:CRAN_authors_db]{CRAN_authors_db()}}
#' @examples
#' maintainers <- cran_maintainers()
#' head(maintainers)
cran_maintainers <- function() {
    db <- save_state("CRAN_db", tools::CRAN_package_db())
    if (is_not_data(db)) {
        return(NA)
    }
    # https://mastodon.social/@eddelbuettel/114217492195207107
    sm <- strcapture(pattern = "['\"]?(.+)['\"]?<((.+)@(.+))>",
                         x = db$Maintainer,
                         proto = data.frame(Name = character(),
                                            email = character(),
                                            direction = character(),
                                            domain = character()))

    sm$direction <- gsub("\\+.+$", "", sm$direction)
    sm$direction <- tolower(sm$direction)
    sm$domain <- tolower(sm$domain)
    sm$Name <- trimws(sm$Name)

    # Packages using Maintainer keep the quotes on the field
    sm$Name <- sub(', PhD"', "", sm$Name)
    modify <- endsWith(sm$Name, '"') & !is.na(sm$Name)
    sm$Name[modify] <- gsub(',.*"', "", sm$Name[modify])

    s <- strsplit(db$Packaged, "; ")

    cbind(db[, c("Package", "Maintainer")],
          user = sapply(s, `[`, 2),
          maintainer_date = charToDate(db$Date, c("%F", "%D", "%m.%d.%y", "%Y-%m-%d")),
          packaged_date = charToDate(sapply(s, `[`, 1), c("%F %T", "%c", "%F", "%a %b %e %T %Y")),
          published_date = charToDate(db$Published, "%F"),
          sm)
}

# Like as.Date.character but tryFormat until they don't work or there are no format to try.
charToDate <- function(x, tryFormat) {
    # To convert units correctly
    lc_time <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", lc_time), add = TRUE)
    Sys.setlocale("LC_TIME", "C")

    stopifnot(is.character(x))
    stopifnot(as.logical(length(tryFormat)))
    # Preallocate while preserving Date class
    y <- rep_len(Sys.Date(), length(x))
    y[] <- NA
    # Avoid trying with NAs
    x_no_na <- !is.na(x)
    y[x_no_na] <- as.Date(x[x_no_na], format = tryFormat[1])
    # Try further methods.
    for (format in tryFormat[-1]) {
        is_relevant_na <- is.na(y) & x_no_na
        y[is_relevant_na] <- as.Date(x[is_relevant_na], format)

        if (!anyNA(y[is_relevant_na])) {
            # Everything converted
            break
        }
    }
    y
}
