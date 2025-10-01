empty_env <- function(name) {
    is.null(pkg_state[[name]])
}

save_state <- function(name, out, verbose = TRUE) {
    # Use CRAN mirror if not set a default
    CRAN_baseurl()
    if (empty_env(name)) {
        if (verbose) {
        message("Retrieving ", name, ", this might take a bit.\n",
                "Caching results to be faster next call in this session.")
        }
        m <- tryCatch(out, warning = function(w) {NA}, error = function(e) {NA})
        if (is_not_data(m)) {
            return(NA)
        }
        pkg_state[[name]] <- m
    }
    pkg_state[[name]]
}

funlist <- function(x) {unlist(x, FALSE, FALSE)}


get_package_subset <- function(name, pkges) {
    stopifnot(is.character(name) && length(name) == 1L,
              "NULL or character vector" = is.null(pkges) || (is.character(pkges) && length(pkges)))

    if (empty_env(name)) {
        return(NULL)
    }

    df <- pkg_state[[name]]

    if (is.null(pkges)) {
        return(df)
    }

    df[pkg_in_x(df, pkges), , drop = FALSE]
}

pkg_in_x <- function(x, packages) {
    if ("package" %in% colnames(x)) {
        x[, "package"] %in% packages
    } else {
        x[, "Package"] %in% packages
    }
}
check_subset <- function(obj, pkges) {
    if ("package" %in% colnames(obj)) {
        all(pkges %in% obj[, "package"])
    } else {
        all(pkges %in% obj[, "Package"])
    }
}

check_installed <- function(x) {
    requireNamespace(x, quietly = TRUE)
}

check_local <- function(x) {
    desc_pkg <- file.path(x, "DESCRIPTION")
    vapply(desc_pkg, file.exists, FUN.VALUE = logical(1L))
}

get_from_local_pkg <- function(x, fields = "Package") {
    if (!length(x)) {
        return(NULL)
    }
    desc_pkg <- file.path(x, "DESCRIPTION")
    desc <- lapply(desc_pkg, read.dcf, fields = fields)
    names(desc) <- if (is.null(names(x))) x else names(x)
    do.call(rbind, desc)
}

# tools:::CRAN_baseurl_for_src_area but with fixed mirror
CRAN_baseurl <- function() {
    url <- "https://CRAN.R-project.org"
    out <- Sys.setenv(R_CRAN_SRC = Sys.getenv("R_CRAN_SRC", url))
    if (isTRUE(out)) {
        url
    } else {
        NULL
    }
}

# tools:::read_CRAN_object but for several types
read_CRAN <- function(path, cran = CRAN_baseurl()) {
    con <- gzcon(url(sprintf("%s/%s", cran, path), open = "rb"))
    on.exit(close(con))
    if (endsWith(path, "rds") || endsWith(path, "RDS")) {
        tryCatch(readRDS(con), error = function(e){
            if (grepl("cannot read from connection", e, fixed = TRUE)) {
                NULL
            } else {e}
        })
    } else {
        tryCatch(read.dcf(con), error = function(e){
            if (grepl("cannot read from connection", e, fixed = TRUE)) {
                NULL
            } else {e}
        })
    }

}

check_r_version <- function() {
    ver <- paste(R.Version()[c("major","minor")], collapse = ".")
    r_ver <- package_version(ver)
    target <- package_version("4.5.0")
    r_ver >= target
}


.cran_archive <- function() {
    if (check_r_version()) {
        return(tools::CRAN_archive_db())
    }
    read_CRAN(CRAN_baseurl(), "src/contrib/Meta/archive.rds")
}

datetime2POSIXct <- function(date, time, tz = cran_tz) {
    moment <- paste(date, time)
    moment[is.na(date) & is.na(time)] <- NA
    moment <- as.POSIXct(moment, tz = cran_tz)
    moment
}


uniq_count <- function(x, name = "n") {
    id <- apply(as.matrix(x), 1L, paste0, collapse = "")

    # Return if no duplicates
    if (!anyDuplicated(id)) {
        if (!NROW(x)) {
            return(cbind(x, n = numeric(0L)))
        }
        n <- matrix(1L, nrow = NROW(x),
                    dimnames = list(seq_len(NROW(x)), name))
        return(cbind(x, n))
    }
    ids <- table(factor(id, levels = unique(id)))
    names(ids) <- NULL
    uid <- unique(x)
    rownames(uid) <- NULL
    uid[, name] <- as.numeric(ids)
    uid
}

add_uniq_count <- function(x, name = "n", old_name = "n") {
    w <- which(colnames(x) %in% old_name)
    # Nothing to add up:
    if (!length(w)) {
        return(x)
    }
    id <- apply(as.matrix(x[, -w, drop = FALSE]), 1, paste0, collapse = ";")
    dup_f <- duplicated(id)
    dup_r <- duplicated(id, fromLast = TRUE)
    dup <- dup_f | dup_r

    # Return if no duplicates
    if (!any(dup)) {
        if (!NROW(x)) {
            return(cbind(x[, -w, drop = FALSE], n = numeric(0L)))
        }
        n <- matrix(1L, nrow = NROW(x),
                    dimnames = list(seq_len(NROW(x)), name))
        return(cbind(x[, -w, drop = FALSE], n))
    }

    y <- x[!dup, ]
    df <- tapply(x[dup, , drop = FALSE], id[dup], function(xy, column_to_add) {
        y <- unique(as.matrix(xy)[, -column_to_add, drop = FALSE])
        y <- cbind(y, name = sum(xy[, column_to_add, drop = TRUE], na.rm = TRUE))
        colnames(y)[ncol(y)] <- name
        y
    }, column_to_add = w)
    dff <- do.call(rbind, df)
    out <- rbind(y, dff)
    out <- as.data.frame(out)
    out$n <- as.numeric(out$n)
    rownames(out) <- NULL
    out
}

check_packages <- function(packages, length = 1L) {
    char_packages <- is.character(packages) && length(na.omit(packages))

    if (isFALSE(char_packages) && !is.na(length)) {
        if (length <= length(packages)) {
            msg <- "Use NULL or a character vector with some packages."
        } else {
            msg <- sprintf("Use NULL or a character vector (without NA) of length %d.", length)
        }
        stop(msg, call. = FALSE)
    }

    # If length = NA it can be NULL
    if (is.null(packages)) {
        return(TRUE)
    }
    local_packages <- dir.exists(packages)

    # is a directory (local package) or :
    #  - least two characters
    #  - start with a letter
    #  - not end in a dot
    valid_names <- nchar(packages) >= 2L & grepl("^[[:alpha:]]", packages) & !endsWith(packages, ".")

    # Don't trigger error on local packages
    if (!any(local_packages) && !any(valid_names[!local_packages])) {
        stop("Packages names should have at least two characters and start",
             " with a letter and not end in a dot.", call. = FALSE)
    }

    TRUE
}

is_logical <- function(x) {
    isTRUE(x) || isFALSE(x)
}


is_not_data <- function(x) {
    !as.logical(NROW(x)) || (length(x) == 1L && is.na(x))
}
