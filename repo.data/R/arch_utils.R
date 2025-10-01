str2mat <- function(pattern, x, columns, perl = FALSE, useBytes = FALSE){
    m <- regexec(pattern, x, perl = perl, useBytes = useBytes)
    str <- regmatches(x, m)
    ntokens <- length(columns) + 1L
    nomatch <- lengths(str) == 0L
    str[nomatch] <- list(rep.int(NA_character_, ntokens))
    if (length(str) > 0L && length(str[[1L]]) != ntokens) {
        stop("The number of captures in 'pattern' != 'length(proto)'")
    }
    mat <- matrix(as.character(unlist(str)), ncol = ntokens,
        byrow = TRUE)[, -1L, drop = FALSE]
    colnames(mat) <- columns
    mat
}

curr2m <- function(pkges) {
    curr <- as.matrix(pkges[, c("mtime", "size", "uname")])
    # dse packages has unorthodox version number
    pkg_v <- str2mat(pattern = "(.+)_(.+)\\.tar\\.gz",
                     x = rownames(pkges),
                     columns = c("package", "version"))
    x <- cbind(curr, pkg_v, status = "current")

    keep_columns <- c("package", "mtime", "version", "uname", "size", "status")
    x[, keep_columns]
    rownames(x) <- NULL
    x
}

arch2m <- function(arch){
    if (!length(arch)) {
        return(NULL)
    }

    l <- lapply(arch, function(x){
        as.matrix(x[, c("mtime", "size", "uname")])
    })
    mat <- do.call(rbind, l)
    # FIXME .tar.gz is not the only way to compress build packages
    pkg_v <- str2mat(pattern = "(.+)_(.+)\\.tar\\.gz",
                     x = rownames(mat),
                     columns = c("package", "version"))
    # cleaning captures
    pkg_v[, "package"] <- basename(pkg_v[, "package"])

    x <- cbind(mat, pkg_v, status = "archived")

    keep_columns <- c("package", "mtime", "version", "uname", "size", "status")
    x[, keep_columns]
    rownames(x) <- NULL
    x
}

arch2df <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }
    x <- as.data.frame(x)
    x$size <- as.numeric(x$size)
    x$mtime <- as.POSIXct(x$mtime, tz = cran_tz)

    # Arrange dates and data
    keep_columns <- c("package", "mtime", "version", "uname", "size", "status")
    x <- sort_by(x[, keep_columns, drop = FALSE], x[, c("package", "status", "mtime")])
    colnames(x) <- c("Package", "Datetime", "Version", "User", "Size", "Status")
    rownames(x) <- NULL
    x
}

warnings_archive <- function(all_packages) {
    # Rely on order of all_packages by date
    dup_arch <- duplicated(all_packages[, c("package", "version")])
    if (any(dup_arch)) {
        warning("There are ", sum(dup_arch, na.rm = TRUE),
                " packages both archived and published\n",
                "This indicate manual CRAN intervention.",
                call. = FALSE, immediate. = TRUE)
    }
    all_packages
}


filter_arch_date <- function(arch, date, type = "[") {
    type <- match.arg(type, c("[", "("))
    bd <- as.Date(arch$Datetime) < as.Date(date)
    if (identical(type, "[")) {
        before_date <- arch[bd, , drop = FALSE]
        before_date <- before_date[!duplicated(before_date$Package, fromLast = TRUE), , drop = FALSE]
    } else {
        before_date <- NULL
    }
    after_date <- arch[!bd, , drop = FALSE]
    out <- rbind(before_date, after_date)
    out <- sort_by(out, out[, c("Package", "Datetime")])
    rownames(out) <- NULL
    out
}


filter_arch_ver <- function(req, arch, req_column = "Name") {
    stopifnot(c(req_column, "Version") %in% colnames(req))
    stopifnot(c("Package", "Version") %in% colnames(arch))

    no_version <- is.na(req[, "Version"])
    # Return the first version of the packages available
    if (all(no_version)) {
        return(arch[!duplicated(arch[, "Package"]), , drop = FALSE])
    }


    arch_ver <- arch[arch[, "Package"] %in% req[!no_version, req_column], , drop = FALSE]

    m <- merge(req[!no_version, , drop = FALSE], arch_ver,
          by.x = c("Name", "Version"), by.y = c("Package", "Version"),
          all.x = TRUE, all.y = FALSE, sort = FALSE)
    # If no version the release dates of those packages is earlier than R or other requirements
    m <- m[!is.na(m$Datetime), , drop = FALSE]
    m$Package <- m$Name
    #
    if (any(no_version)) {
        pkg_wo_ver_req <- req[no_version, req_column]
        arch_no_ver <- arch[arch[, "Package"] %in% pkg_wo_ver_req, , drop = FALSE]
        arch_no_ver <- arch_no_ver[!duplicated(arch_no_ver[, "Package"]), , drop = FALSE]
    } else  {
        arch_no_ver <- NULL
    }

    rbind(m[, c("Package", "Datetime")], arch_no_ver[, c("Package", "Datetime")])
}
