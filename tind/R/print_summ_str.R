#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ################################################## #
# print and summary methods for tind package classes #
# ################################################## #


# print
# ###################################################################

#' @rdname tind-methods
#' @export
print.tind <- function(x, ...) .print.internal(x, ...)


#' @rdname tdiff
#' @export
print.tdiff <- function(x, ...) .print.internal(x, ...)


#' @rdname tinterval
#' @export
print.tinterval <- function(x, ...) .print.internal(x, ...)



# print implementation
# ###################################################################

.print.internal <- function(x, ...)
{
    # prep
    if (is.tind(x)) {
        class <- "tind"
        type <- .get.type(x)
        tz <- .get.tz(x)
        info <- .ti_type(type, long = TRUE, translate = TRUE)
    } else if (is.tdiff(x)) {
        class <- "tdiff"
        type <- .get_tdiff_type(x)
        tz <- NULL
        info <- gettextf("time difference")
        if (type != "t") {
            info <- paste0(info, ", ",  gettextf("unit"), ": ",
                           .t_unit(type, long = TRUE, translate = TRUE))
        }
    } else { # tinterval
        class <- "tinterval"
        type <- .get.type(x$start)
        tz <- .get.tz(x$start)
        info <- switch(type,
                       t = gettextf("time interval"),
                       h = gettextf("time interval (time of day)"),
                       y = gettextf("time interval in years"),
                       q = gettextf("time interval in quarters"),
                       m = gettextf("time interval in months"),
                       w = gettextf("time interval in weeks"),
                       d = gettextf("time interval in days"),
                       i = gettextf("integer interval"),
                       n = gettextf("numeric interval"))
    }
    # info
    if (!is.null(tz)) info <- paste0(info, ", ", gettextf("time zone: %s", tz))
    info <- paste0(info, " [", class, "]\n")
    cat(info)
    # fast exit
    if (!(n <- length(x))) {
        cat(if (class == "tinterval") gettextf("empty") else gettextf("length 0"),
            "\n", sep = "")
        return (invisible(x))
    }
    # data
    mp <- getOption("max.print", 1000L)
    clip <- mp < n
    fx <- if (clip) format(x[1L:mp], ...) else format(x, ...)
    nms <- names(fx)
    fx <- format(fx, justify = "centre")
    names(fx) <- nms
    # print
    gap <- 1L + isTRUE(any(grepl("[^ ]+[ ]+[^ ]+", fx)))
    print(fx, quote = FALSE, print.gap = gap)
    if (clip) {
        mes <- gettextf("reached %s -- omitted %d entries",
                        sQuote("getOption(\"max.print\")"), n - mp)
        cat(" [ ", mes, " ]\n", sep = "")
    }

    return (invisible(x))
}



# summary
# ###################################################################

#' @rdname tind-methods
#' @export
summary.tind <- function(object, ...)
{
    sumstats <- list(`Type`         = ti_type,
                     `Time zone`    = tzone,
                     `Length`       = length,
                     `First`        = function(x) x[1L],
                     `Last`         = function(x) x[max(1L, length(x))],
                     `Min.`         = function(x) min(x, na.rm = TRUE),
                     `Max.`         = function(x) max(x, na.rm = TRUE),
                     `Median`       = function(x) stats::median(x, na.rm = TRUE),
                     `Mean`         = function(x) mean(x, na.rm = TRUE),
                     `Span`         = function(x) tspan(x, na.rm = TRUE),
                     `Resolution`   = resolution_t,
                     `NA`           = function(x) sum(is.na(x)))
    if (!length(object)) {
        sumstats <- sumstats[1L:3L]
    } else if (is.ordered_t(object)) {
        sumstats$`Min.` <- sumstats$`Max.` <- NULL
    } else {
        sumstats$`First` <- sumstats$`Last` <- NULL
    }
    if (ti_type(object, long = FALSE) != "t") sumstats$`Time zone` <- NULL
    res <- suppressWarnings(lapply(sumstats, function(f) do.call(f, list(object))))
    return (structure(sapply(res, format), class = c("summaryDefault", "table")))
}


#' @rdname tdiff
#' @export
summary.tdiff <- function(object, ...)
{
    sumstats <- list(`Unit`      = t_unit,
                     `Length`    = length,
                     `Min.`      = function(x) min(x, na.rm = TRUE),
                     `Max.`      = function(x) max(x, na.rm = TRUE),
                     `Median`    = function(x) stats::median(x, na.rm = TRUE),
                     `Mean`      = function(x) mean(x, na.rm = TRUE),
                     `NAs`       = function(x) sum(is.na(x)))
    if (!length(object)) sumstats <- sumstats[1L:2L]
    res <- suppressWarnings(lapply(sumstats, function(f) do.call(f, list(object))))
    return (structure(sapply(res, format), class = c("summaryDefault", "table")))
}


#' @rdname tinterval
#' @export
summary.tinterval <- function(object, ...)
{
    sumstats1 <- list(`Type`                = ti_type,
                      `Time zone`           = tzone,
                      `No. of intervals`    = length)
    if (ti_type(object, long = FALSE) != "t") sumstats1 <- sumstats1[-2L]
    res <- lapply(sumstats1, function(f) do.call(f, list(object)))

    if (length(object)) {
        dx <- sort(tspan(object), na.last = TRUE)
        sumstats2 <- list(`Min. span`   = function(x) x[1L],
                          `Max. span`   = function(x) x[length(x)],
                          `Mean span`   = mean)
        res2 <- lapply(sumstats2, function(f) do.call(f, list(dx)))
        res <- c(res, res2)
        ux <- unique(object)
        sumstats3 <- list(`Unique intervals`    = length,
                          `Total span`          = function(x) sum(tspan(ux)))
        res3 <- lapply(sumstats3, function(f) do.call(f, list(ux)))
        res <- c(res, res3)
    }

    return (structure(sapply(res, format), class = c("summaryDefault", "table")))
}



# str
# ###################################################################

#' @keywords internal
#' @exportS3Method utils::str
str.tind <- function(object, ...)
{
    if (!(n <- length(object))) {
        md <- substr(typeof(object), 1L, 1L)
        cat(sprintf(" 'tind' %s(0)\n", if (md == "i") "int" else "num"))
        return (invisible(NULL))
    }

    n <- length(object)
    inf <- sprintf(" tind [1:%d]", n)
    fmt <- format(utils::head(object, 10L))
    fmt[is.na(fmt)] <- "NA"
    gap <- 1L + isTRUE(any(grepl("[^ ]+[ ]+[^ ]+", fmt)))
    sel <- cumsum(nchar(fmt) + gap) < 65 - nchar(inf)
    sel[1L] <- TRUE
    ns <- sum(sel)
    fmt <- fmt[sel]
    if (ns < n) fmt <- c(fmt, "...")
    fmt <- paste0(paste0(paste0(rep(" ", gap), collapse = ""), fmt), collapse = "")
    cat(inf, fmt, "\n", sep = "")

    return (invisible(NULL))
}


#' @keywords internal
#' @exportS3Method utils::str
str.tdiff <- function(object, ...)
{
    if (!(n <- length(object))) {
        md <- substr(typeof(object), 1L, 1L)
        cat(sprintf(" 'tdiff' %s(0)\n", if (md == "i") "int" else "num"))
        return (invisible(NULL))
    }

    n <- length(object)
    inf <- sprintf(" tdiff [1:%d]", n)
    fmt <- format(utils::head(object, 10L))
    fmt[is.na(fmt)] <- "NA"
    sel <- cumsum(nchar(fmt) + 1L) < 65 - nchar(inf)
    sel[1L] <- TRUE
    ns <- sum(sel)
    fmt <- fmt[sel]
    if (ns < n) fmt <- c(fmt, "...")
    fmt <- paste0(paste0(" ", fmt), collapse = "")
    cat(inf, fmt, "\n", sep = "")

    return (invisible(NULL))
}


#' @keywords internal
#' @exportS3Method utils::str
str.tinterval <- function(object, ...)
{
    cat(" 'tinterval'\n")
    cat("   $ start:", utils::capture.output(utils::str(object$start)), "\n", sep = "")
    cat("   $ end  :", utils::capture.output(utils::str(object$end)), "\n", sep = "")
    return (invisible(NULL))
}

