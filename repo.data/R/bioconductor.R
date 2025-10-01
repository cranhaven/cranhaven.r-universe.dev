#' Bioconductor packages using CRAN archived packages
#'
#' Checks on the 4 Bioconductor repositories which packages depend on a
#' archived package.
#' @inheritParams repos_dependencies
#' @returns A data.frame with the name of the Bioconductor packages
#' depending on archived packages (on Archived column) and the
#' number of missing packages (n).
#' `NA` if not able to collect the data.
#' @export
#' @seealso For CRAN's data source: \code{\link[tools:CRAN_package_db]{tools::CRAN_package_db()}}
#' @examples
#' bca <- bioc_cran_archived()
#' head(bca)
bioc_cran_archived <- function(which = "strong") {
    fields_selected <- check_which(which)
    bioc <- bioc_available()
    db <- save_state("CRAN_db", tools::CRAN_package_db())
    if (is_not_data(db)) {
        return(NA)
    }
    columns <- intersect(colnames(bioc), colnames(db))
    db_all <- rbind(db[, columns], bioc[, columns])
    bioc_deps <- packages_dependencies(as.matrix(bioc[, fields_selected]))
    base_r <- tools::standard_package_names()$base

    present <- !bioc_deps$Name %in% c(db_all$Package, base_r, "R")
    missing_deps <- bioc_deps[present, ]
    # TODO: Check any dependency to these packages.
    miss_p_pkg <- split(missing_deps$Name, missing_deps$Package)
    lmissing_dep <- lengths(miss_p_pkg)
    p_missing <- vapply(miss_p_pkg, toString, character(1L))
    df <- data.frame(Package = names(miss_p_pkg), Archived = p_missing, n = lmissing_dep)
    rownames(df) <- NULL
    df
}

#' @importFrom utils read.csv
bioc_version <- function(type = "release") {
    bioc_config <- "https://bioconductor.org/config.yaml"
    rl <- tryCatch(readLines(con = url(bioc_config)), warning = function(w){NA}, error = function(e){NA})
    if (is_not_data(rl)) {
        return(NA)
    }
    type <- match.arg(type, c("release", "devel"))
    if (identical(type, "release")) {
        version <- which(startsWith(rl, "release_version"))
    } else {
        version <- which(startsWith(rl, "devel_version"))
    }
    # Quick and dirty way to read and split the data
    rv <- read.csv(text = rl[version], sep = ":", header = FALSE, colClasses = c("character", "character"))
    trimws(rv$V2)
}


bioc_repos <- function(version = "release",
                       repos = c("/bioc", "/data/annotation", "/data/experiment", "/workflows", "/books")) {
    name_repos <- basename(repos)
    name_repos[1] <- "software"

    urls <- paste0("https://bioconductor.org/packages/", bioc_version(version), repos)

    url_repos <- urls
    names(url_repos) <- name_repos
    url_repos
}

bioc_available <- function(version = "release",
                           repos = c("/bioc", "/data/annotation", "/data/experiment", "/workflows", "/books")) {
    url_repos <- bioc_repos(version, repos)
    opts <- options(available_packages_filters = c("CRAN", "duplicates"))
    on.exit(options(opts), add = TRUE)
    bioc <- save_state(paste0("bioc_available_", version),
        available.packages(repos = url_repos))
    if (is_not_data(bioc)) {
        return(NA)
    }
    bioc <- as.data.frame(bioc)
    bioc
}

bioc_views <- function(version = bioc_version()) {
    if (is.na(version)) {
        return(NA)
    }
    url <- paste0("https://bioconductor.org/packages/", version, "/bioc/VIEWS")
    read.dcf(url(url))
}

bioc_archive <- function() {
    # TODO convert this to extract the dates of the latest publication of the package.
    # As no new packages are added until the next release we can assume they were
    # on the date most packages were updated
    v <- paste0(3, ".", 1:21)
    bv <- lapply(v, bioc_views)

    versions <- rep(v, vapply(bv, NROW, numeric(1L)))
    # m1 <- do.call(merge, bv, all = TRUE)

    v2 <- paste0(2, ".", 1:14)
    bv2 <- lapply(v2, bioc_views)
    versions2 <- rep(v2, vapply(bv2, NROW, numeric(1L)))
    # m2 <- do.call(merge, bv2, all = TRUE)
    # m <- merge(m1, m2, all = TRUE, sort = FALSE)
}


