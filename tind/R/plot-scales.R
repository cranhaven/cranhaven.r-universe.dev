#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ############################################ #
# scales / axes for plotting time-indexed data #
# ############################################ #



# time axis label formatting
# ###################################################################

.label_tind <- function(x, format = NULL, locale = NULL)
{
    if (is.function(format)) return (format)
    if (!is.null(format))
        return (function(x) base::format(x, format = format, locale = locale))

    if (.get.type(x) == "t")
        return (function(x) {
            n <- length(x)
            if (!n) return (character())
            dt <- date_time_split(x)
            lab <- base::format(dt$time)
            mdnght <- x == as.date_time(dt$date, .get.tz(x))
            lab[mdnght] <- base::format(dt$date[mdnght])
            return (lab)
        })

    return (function(x) base::format(x, format = format, locale = locale))
}


# tind -> Cartesian, Cartesian -> tind
# ###################################################################

.t2coords <- function(x) as.double(x)

.coords2t <- function(x, type, tz)
    .tind(do.call(paste0(".validate_", type), list(x)), type, tz)


# breaks
# ###################################################################

.breaks_t <- function(x, n, limits = NULL)
{
    al <- .apply_limits(x, limits)
    rx <- resolution_t(x)
    x1 <- if (is.na(rx)) c(al[[1L]], al[[2L]])
          else c(floor_t(al[[2L]][1L], rx), al[[1L]], ceiling_t(al[[2L]][2L], rx))

    res <- pretty(x1, n = n, min.n = min(n %/% 2L, 2L))
    return (res)
}


.minor_breaks_t <- function(b, limits, n, resx, type, tz)
{
    if (isTRUE(n == 0)) return (numeric())
    nb <- length(b)
    br <- .coords2t(b, type, tz)
    resb <- .tdiff2nunit(resolution_t(br))
    rbu <- resb$unit
    rbn <- resb$n
    if (is.na(n)) { # default settings
        n <- 2
        if (!(type %in% c("i", "n"))) {
            if (rbn == 3) n <- 3.3
            if ((rbu == "q") && (rbn == 1L) || rbn == 3) n <- 4
            else if ((rbu == "d") || (rbn == 1L)) n <- 4
            else if ((rbu == "m") && (rbn == 4L)) n <- 4
            else if ((rbu == "w") && (rbn == 1L)) n <- 7
        }
    }

    # a trick to make sure resolution of argument to pretty is resx
    if (!(type %in% c("i", "n"))) {
        br2 <- c(br, ceiling_t(br[c(1L, nb)] + resx, resx),
                     floor_t(br[c(1L, nb)] - resx, resx),
                     round_t(.coords2t(limits, type, tz), resx))
    } else br2 <- br
    mb <- pretty(br2, nb * n, nb - 1L)

    # consistency checks
    if (type %in% c("i", "n")) {
        ok <- !(diff(br[1L:2L]) %% diff(mb[1L:2L]))
    } else {
        resm <- .tdiff2nunit(resolution_t(mb))
        rmu <- resm$unit
        rmn <- resm$n
        if (rbu == rmu) {
            if ((rbu == "d") && (rbn == 15)) ok <- FALSE
            else ok <- !(rbn %% rmn)
        } else if (rbu == "y") {
            ok <- TRUE
        } else if ((rbu == "q") && (rmu == "m")) {
            ok <- (rmn == 1L)
        } else if (rmu == "d") {
            ok <- (rmn == 1L) || (rmn == 15L) && (rbu == "m")
        } else if (rmu %in% c("h", "min", "s")) {
            ok <- TRUE
        } else ok <- FALSE
    }
    if (!ok) return (numeric())

    mb <- .tind(setdiff(as.vector(mb), as.vector(br)), type, tz)
    res <- .t2coords(mb)
    res <- res[(res >= limits[1L]) & (res <= limits[2L])]
    if (length(b) + length(res) > 20L) return (numeric())

    return (res)
}


# transformations
# ###################################################################

.tind_trans <- function(x, format = NULL, locale = NULL, ...)
{
    x <- as.tind(x)
    force(format); force(locale)
    type <- .get.type(x)
    tz <- .get.tz(x)
    resx <- resolution_t(x)
    # name
    name <- paste0("tind_", .ti_type(type, valid = TRUE))
    if (type == "t") {
        tzname <- gsub("-(?=[0-9])", "m", tz, perl = TRUE)
        tzname <- gsub("\\+(?=[0-9])", "p", tzname, perl = TRUE)
        tzname <- gsub("[-+/]", "_", tzname)
        name <- paste0(name, "_", tzname)
    }
    # tind -> Cartesian
    transform <- .t2coords
    # Cartesian -> tind
    inverse <- function(x) .coords2t(x, type = type, tz = tz)
    # breaks
    breaks <- .breaks_t
    # minor breaks
    minor_breaks <- function(b, limits, n) .minor_breaks_t(b, limits, n, resx = resx,
                                                           type = type, tz = tz)
    # format
    format <- .label_tind(x, format = format, locale = locale)
    # domain
    domain <- if (!(type %in% c("i", "n"))) .tind(.limits(type), type, tz)
              else suppressWarnings(as.tind(.limits(type), type))

    return (list(name = name,
                 transform = transform, inverse = inverse,
                 breaks = breaks, minor_breaks = minor_breaks,
                 format = format,
                 domain = domain))
}


# time axis resolution in Cartesian coordinates
# ###################################################################

.resolution_t <- function(x)
{
    type <- .get.type(x)
    res <- resolution_t(x)
    if (type == "i") return (res)
    if (type == "n") {
        if (!is.na(res)) return (res)
        if (length(x) <= 1L) return (1.)
        return (min(diff(sort(unique(x)))))
    }
    rtype <- .get_tdiff_type(res)
    if ((type == "h") || (type == rtype)) return (.unclass(res))

    widths <- list(q = c(y = 4),
                   m = c(y = 12),
                   w = c(y = 52.18),
                   d = c(w = 7, m = 30.44, y = 365.24))
    widths$t <- 86400 * c(d = 1, widths$d)
    return (.unclass(res) * widths[[type]][[rtype]])
}


# time axis limits
# ###################################################################

.apply_limits <- function(x, limits)
{
    if (is.null(limits)) return (list(x, x[c(NA_integer_, NA_integer_)]))
    if (!is.tind(limits) && !is.tinterval(limits)) {
        limits1 <- tryCatch(as.tinterval(limits), error = function(e) NULL)
        if (is.null(limits1)) limits1 <- tryCatch(as.tind(limits),
                                                  error = function(e) NULL)
        limits <- limits1
    }
    if (is.tind(limits) && (length(limits) == 2L)) {
        limits <- tinterval(limits[1L], limits[2L])
    } else if (!is.tinterval(limits) || length(limits) != 1L) {
        limits <- NULL
    }
    if (is.null(limits)) {
        mes0 <- gettextf("invalid time axis limits")
        mes1 <- gettextf("expected %s of length 1 or %s of length 2",
                            dQuote("tinterval"), dQuote("tind"))
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    tsplim <- tspan(limits)
    if (is.finite(tsplim) && !tsplim) {
        mes <- gettextf("empty time time interval")
        stop(mes, call. = FALSE, domain = NA)
    }
    if ((.get.type(x) == "t") && (.get.type(limits$start) == "h")) {
        mes <- gettextf("limits of type %s cannot be used with indices of type %",
                        .ti_type2char("h"), .ti_type2char("t"))
        stop(mes, call. = FALSE, domain = NA)
    }
    x1 <- x[x %in_t% limits]
    if (!length(x1)) {
        mes <- gettextf("no data within time interval")
        warning(mes, call. = FALSE, domain = NA)
    }

    limits <- as.tinterval(limits, .get.type(x), tz = .get.tz(x))
    limits <- c(limits$start, limits$end)

    return (list(x1, limits))
}


.limits_t <- function(x, n.breaks, n.minor_breaks, limits = NULL)
{
    tp <- .get.type(x)
    trans <- .tind_trans(x)
    pt <- trans$breaks(x, n.breaks, limits)
    at <- trans$transform(pt)
    minor <- trans$minor_breaks(at, range(at, na.rm = TRUE), n.minor_breaks)
    mpt <- trans$inverse(minor)

    # auxiliary functions
    .maxrange <- function(...)
    {
        argl <- list(...)
        r0 <- do.call(min, c(lapply(argl, function(x) x[1L]), list(na.rm = TRUE)))
        r1 <- do.call(max, c(lapply(argl, function(x) x[2L]), list(na.rm = TRUE)))
        c(r0, r1)
    }
    .minrange <- function(...)
    {
        argl <- list(...)
        r0 <- do.call(max, c(lapply(argl, function(x) x[1L]), list(na.rm = TRUE)))
        r1 <- do.call(min, c(lapply(argl, function(x) x[2L]), list(na.rm = TRUE)))
        c(r0, r1)
    }

    lims <- .apply_limits(x, limits)
    lim <- trans$transform(lims[[2L]])
    rngx <- range(lims[[1L]])
    lim0 <- trans$transform(rngx)
    res1 <- resolution_t(pt)
    res2 <- resolution_t(c(pt, mpt))
    lim1 <- trans$transform(round_t(rngx, res1))
    lim2 <- trans$transform(round_t(rngx, res2))

    return (list(lim = .maxrange(lim0, .minrange(lim1, lim2), lim), limits = lims[[2L]]))
}



# general purpose
# ###################################################################

#' Compute Time Axis Parameters for Plotting
#'
#' @description
#' Auxiliary function \code{axis_t} returns a six-element list with axis
#' limits (in Cartesian coordinates), tick-mark positions (in Cartesian
#' coordinates), tick-mark labels (character vector), positioning of minor
#' tick-marks (in Cartesian coordinates), resolution of indices (in Cartesian
#' coordinates), and \code{limits} argument converted to a \code{tinterval}
#' of the same index type as \code{x}. The results can be used for manual creation
#' of axes in plots.
#'
#' @param x time indices for which an axis is to be created.
#' @param limits \code{NULL} for automatic limits, \code{tinterval} of length 1
#'               or \code{tind} of length 2.
#' @param format (optional) a character string determining label format
#'               (see \code{\link{format}}) or a custom formatting function.
#' @param locale (optional) a character string determining locale to be used
#'               for formatting labels, see \link{calendar-names} for information
#'               on locale settings.
#' @param expand a logical value. If \code{TRUE}, limits are expanded by 3\%
#'               on both sides.
#' @param n.breaks an integer value, desired number of breaks.
#'
#' @return A six-element list with scale limits (\code{lim}), vector of tick-mark
#' positions (\code{at}), character vector with tick-mark labels (\code{labels}),
#' vector of minor tick-mark positions (\code{minor}), resolution (in Cartesian
#' coordinates) of time indices (\code{resolution}), and \code{limits} argument
#' converted to a \code{tinterval} of the same index type as \code{x} (\code{limits}).
#'
#' @seealso \code{\link{pretty}} for computing pretty breakpoints,
#' \code{\link{axis.tind}} for creating axes with \pkg{graphics} package,
#' \code{\link{scale_tind}} for creating axes with \pkg{ggplot2}.
#'
#' @examples
#' \donttest{
#' # load graphics
#' library(graphics)
#' # artificial data
#' N <- 180
#' df <- data.frame(d = today() + (-N + 1):0, y = cumsum(rnorm(N)))
#' (axt <- axis_t(df$d, format = "%m/%d/%y", n.breaks = 6L))
#' # custom time axis with minor breaks
#' plot(df$d, df$y, xlim = axt$lim, type = "l", xaxt = "n", xaxs = "i")
#' axis(1, at = axt$lim, labels = FALSE, lwd = 1, lwd.ticks = 0)
#' axis(1, at = axt$at, labels = axt$labels, lwd = 0, lwd.ticks = .7)
#' axis(1, at = axt$minor, labels = FALSE, lwd = 0, lwd.ticks = .4)
#' }
#'
#' @export
#'
axis_t <- function(x, limits = NULL, format = NULL, locale = NULL,
                   expand = FALSE, n.breaks = 5L)
{
    x <- as.tind(x)
    .checkTRUEFALSE(expand)

    trans <- .tind_trans(x, format, locale)
    lims <- .limits_t(x, n.breaks, NA, limits)
    lim <- lims[[1L]]
    b <- trans$breaks(x, n.breaks, limits)
    b <- trans$transform(b)
    mb <- trans$minor_breaks(b, lim, NA)

    res <- .resolution_t(x)
    lim <- lim + .5 * c(-1, 1) * res
    if (expand) lim <- lim + 0.03 * c(-1, 1) * diff(lim)
    b <- b[b >= lim[1L] & b <= lim[2L]]
    mb <- mb[mb >= lim[1L] & mb <= lim[2L]]
    lab <- trans$format(trans$inverse(b))
    lim <- lim

    return (list(lim = lim, at = b, labels = lab, minor = mb, resolution = res,
                 limits = tinterval(lims[[2L]][1L], lims[[2L]][2L])))
}


# graphics
# ###################################################################

#' Add Time Axis --- Plotting with \code{graphics} Package
#'
#' \code{axis.tind} adds time axis to a plot. Axes will be added automatically
#' by \pkg{graphics}, but the default behaviour can be overridden by plotting
#' without axis and then calling \code{axis.tind}, see Examples.
#'
#' @param side see \code{\link[graphics]{axis}}.
#' @param x time indices for which an axis is to be created.
#' @param at (optional) time indices at which manual tick-marks and labels
#'           should be placed.
#' @param format (optional) a character string determining label format
#'               or a formatting function, see \code{\link{format}}.
#' @param locale (optional) a character string determining locale to be used
#'               for formatting labels, see \link{calendar-names} for information
#'               on locale settings.
#' @param labels a logical value determining whether automatic labels should
#'               be placed at tick-marks or a character vector of labels.
#' @param n.breaks an integer value, desired number of breaks.
#' @param ... further arguments passed to \code{\link[graphics]{axis}}.
#'
#' @return Same as for \code{\link[graphics]{axis}}, used for its side effect,
#' which is to add time axis to an existing plot.
#'
#' @seealso \code{\link{pretty}} for computing pretty breakpoints,
#' \code{\link{axis_t}} for calculating axis parameters,
#' \code{\link{scale_tind}} for creating axes with \pkg{ggplot2}.
#'
#' @examples
#' \donttest{
#' # load graphics
#' library(graphics)
#' # artificial data
#' N <- 100
#' df <- data.frame(d = today() + (-N + 1):0, y = cumsum(rnorm(N)))
#' # default axis
#' plot(df$d, df$y, type = "l")
#' # custom date format with potentially more breaks and a smaller font
#' plot(df$d, df$y, type = "l", xaxt = "n")
#' axis.tind(1, df$d, format = "%m/%d/%y", n.breaks = 7L, cex.axis = .9)
#' }
#'
#' @export
#'
axis.tind <- function(side, x, at, format = NULL, locale = NULL, labels = TRUE,
                      n.breaks = 5L, ...)
{
    x <- as.tind(x)
    if (!missing(at) && !is.null(at)) {
        at <- as.tind(at)
        xtype <- .get.type(x)
        attype <- .get.type(at)
        if (xtype != attype) {
            if (!(xtype %in% .hi_res_cast(attype))) .stop_cast(attype, xtype)
            at <- .cast(at, xtype, .get.tz(x))
        } else if ((xtype == "t") && (.get.tz(x) != .get.tz(at)))
            .warn_diff_tz(.get.tz(x), .get.tz(at))
        if (isTRUE(labels))
            labels <- .label_tind(at, format = format, locale = locale)(at)
        at <- .t2coords(at)
    } else {
        axt <- axis_t(x, format = format, locale = locale, expand = TRUE,
                      n.breaks = n.breaks)
        at <- axt$at
        if (isTRUE(labels)) labels <- axt$labels
    }
    graphics::axis(side, at = at, labels = labels, ...)
}


#' @keywords internal
#' @exportS3Method graphics::Axis
Axis.tind <- function(x = NULL, at = NULL, ..., side, labels = TRUE)
    axis.tind(side = side, x = x, at = at, labels = labels, ...)



# ggplot2
# ###################################################################

#' @keywords internal
#' @exportS3Method ggplot2::scale_type
scale_type.tind <- function(x) c("tind", "continuous")


#' Time Scales for Plotting with \pkg{ggplot2}
#'
#' @description
#' These functions provide \pkg{ggplot2} scales for \code{tind}. Scales will
#' be added automatically by \pkg{ggplot2}, but the default behaviour
#' can be overridden by adding \code{scale_*_tind} to the plot.
#'
#' @details
#' The algorithm determining positioning of breaks and minor breaks always takes
#' the resolution of time indices (see \code{\link{resolution_t}}) into account.
#' For example, for monthly data with breaks placed every three months (January,
#' April, July, October) minor breaks will never be placed in the middle (mid of
#' February, May, August, November) but rather every month. The algorithm
#' overrides the default approach of \pkg{ggplot2} --- axis limits are determined
#' based on breaks and breaks on time indices and their resolution, whereas
#' \pkg{ggplot2} starts with limits based on data and determines breaks based
#' on limits only (ignoring the resolution of time indices).
#'
#' Argument list is a bit different from that of to \code{scale_*_date} and
#' \code{scale_*_datetime}. Firstly, \code{n.breaks} argument is supported,
#' allowing users to set the expected number of breaks on time axis. Secondly,
#' \code{labels} cannot be a function. Formatting functions can be provided via
#' \code{format} argument, which also supports character string with format
#' specification (see \code{\link{format}}) rendering \code{date_labels}
#' argument redundant. \code{locale} argument controls the language used
#' for month and weekday names, see \link{calendar-names}.
#' \code{breaks} cannot be a function, but can be a \code{tdiff} / character string
#' determining distance between breaks rendering \code{date_breaks} argument
#' redundant. \code{limits} argument is expected to determine time interval
#' and not limits in Cartesian coordinates. Open-ended intervals are supported.
#'
#' \code{breaks}, \code{minor_breaks}, and \code{limits} cannot be functions
#' as \pkg{ggplot2} assumes that breaks and limits can be properly set based
#' on (automatic) limits only without taking into account the resolution of time
#' indices, which is not true.
#'
#' Secondary axes are not supported.
#'
#' @note
#' Due to the fact that \code{\link[ggplot2]{limits}} method is currently
#' (as of version 4.0.0) not exported, users cannot use \code{xlim} and
#' \code{ylim} with \code{tind} scales. Limits on time-indexed axes can be set
#' using \code{limits} argument to \code{scale_*_tind}.
#'
#' @param name a character string with axis name, \code{waiver()} for default
#'             name, or \code{NULL} for none.
#' @param breaks \code{waiver()} for default tick-marks, \code{NULL} for none,
#'               time indices at which tick-marks should be placed (\code{tind}),
#'               or a \code{tdiff} / character string determining distance between
#'               breaks.
#' @param minor_breaks \code{waiver()} for default minor tick-marks,
#'                     \code{NULL} for none, or time indices at which minor
#'                     tick-marks should be placed.
#' @param n.breaks an integer value, desired number of breaks.
#' @param labels \code{waiver()} for default labels, \code{NULL} for none,
#'               or a character vector of labels.
#' @param limits \code{NULL} for automatic limits, \code{tinterval} of length 1
#'               or \code{tind} of length 2.
#' @param expand,guide see \link[ggplot2]{scale_continuous}.
#' @param position a character string determining axis position,
#'                 \code{"bottom"} or \code{"top"} for \code{scale_x_tind},
#'                 \code{"left"} or \code{"right"} for \code{scale_y_tind}.
#' @param format (optional) a character string determining label format
#'               (see \code{\link{format}}) or a formatting function.
#' @param locale (optional) a character string determining locale to be used
#'               for formatting labels, see \link{calendar-names} for information
#'               on locale settings.
#'
#' @return A continuous scale as returned by \code{\link[ggplot2]{continuous_scale}}.
#'
#' @seealso \code{\link{pretty}} for computing pretty breakpoints,
#' \code{\link{axis_t}} for computing time axis parameters,
#' \code{\link{axis.tind}} for creating axes with \pkg{graphics} package.
#'
#' @examples
#' \donttest{
#' # load ggplot2
#' library(ggplot2)
#' # artificial data
#' d <- seq(floor_t(today(), "y"), ceiling_t(today(), "y", ceiling = "last"))
#' df <- data.frame(d = d, D = as.Date(d), y = cumsum(rnorm(length(d))),
#'                  q = paste0("Q", quarter(d)))
#' # default scale
#' ggplot(df) + geom_line(aes(x = d, y = y)) + theme_bw()
#' # compare with the default scale for Date
#' ggplot(df) + geom_line(aes(x = D, y = y)) + theme_bw()
#' # change format
#' ggplot(df) + geom_line(aes(x = d, y = y)) + theme_bw() +
#'     scale_x_tind(format = "%b '%y")
#' # set breaks every 4 months
#' ggplot(df) + geom_line(aes(x = d, y = y)) + theme_bw() +
#'     scale_x_tind(breaks = "4m")
#' # set limits
#' ggplot(df) + geom_line(aes(x = d, y = y), na.rm = TRUE) + theme_bw() +
#'     scale_x_tind(limits = c(today() %-m% 4, today()))
#' # facets with custom formatting and reduced number of breaks
#' ggplot(df) + geom_line(aes(x = d, y = y)) + theme_bw() +
#'     scale_x_tind(n.breaks = 4, format = "%b '%y") +
#'     facet_wrap(~q, scales = "free_x")
#' }
#'
#' @name scale_tind
NULL


#' @rdname scale_tind
#' @export
scale_x_tind <- function(name = ggplot2::waiver(),
                         breaks = ggplot2::waiver(),
                         minor_breaks = ggplot2::waiver(),
                         n.breaks = 7L,
                         labels = ggplot2::waiver(),
                         limits = NULL,
                         expand = ggplot2::waiver(),
                         guide = ggplot2::waiver(),
                         position = "bottom",
                         format = NULL, locale = NULL)
{
    .tind_scale("x", name = name,
                breaks = breaks,
                minor_breaks = minor_breaks,
                n.breaks = n.breaks,
                labels = labels,
                limits = limits,
                expand = expand,
                guide = guide,
                position = position,
                format = format, locale = locale)
}


#' @rdname scale_tind
#' @export
scale_y_tind <- function(name = ggplot2::waiver(),
                         breaks = ggplot2::waiver(),
                         minor_breaks = ggplot2::waiver(),
                         n.breaks = 7L,
                         labels = ggplot2::waiver(),
                         limits = NULL,
                         expand = ggplot2::waiver(),
                         guide = ggplot2::waiver(),
                         position = "left",
                         format = NULL, locale = NULL)
{
    .tind_scale("y", name = name,
                breaks = breaks,
                minor_breaks = minor_breaks,
                n.breaks = n.breaks,
                labels = labels,
                limits = limits,
                expand = expand,
                guide = guide,
                position = position,
                format = format, locale = locale)
}



.tind_scale <- function(axis, name, breaks, minor_breaks, n.breaks, labels,
                        limits, expand, guide, position, format, locale)
{
    # is_waiver was not exported by ggplot2 before 4.0.0
    .is.waive <- function(x) inherits(x, "waiver")

    ## NOTE: yes, we are overriding ggplot2's way doing things and, in a way,
    ## running in circles here. ggplot2 requires limits to compute breaks
    ## and the approach of tind is to determine limits based on breaks and
    ## breaks on time indices only (always taking resolution of time indices
    ## into account).
    sc <- ggplot2::ggproto("ScaleContinuousTind", ggplot2::ScaleContinuous,
        secondary.axis = ggplot2::waiver(),
        transform = function(self, x)
        {
            ## NOTE: this is actually a hack, similar to the one found
            ## in the implementation of ggplot2::scale_*_datetime.
            # set up everything on the 1st call
            if (is.null(self$tind)) {
                self$tind <- x
                self$trans <- .tind_trans(x, self$format, self$locale)
                self$format <- self$locale <- NULL
                self$limits0 <- .apply_limits(x, self$limits0)[[2L]]
                self$limits0 <- tinterval(self$limits0[1L], self$limits0[2L])
            }
            tx <- ggplot2::ggproto_parent(ggplot2::ScaleContinuous, self)$transform(x)
            return (tx)
        },
        get_limits = function(self)
        {
            # set up everything on the 1st call
            if (is.null(self$limits)) {
                self$limits <- .limits_t(self$tind, self$n.breaks, NA, self$limits0)[[1L]]
            }
            return (self$limits)
        },
        get_breaks = function(self, limits = self$get_limits())
        {
            b <- self$breaks
            if (is.null(b)) return (numeric())
            if (!.is.waive(b) && !is.tind(b)) {
                rx <- resolution_t(self$tind)
                mM <- range(self$tind, na.rm = TRUE)
                m <- floor_t(mM[1L], b)
                M <- ceiling_t(mM[2L], b)
                b <- sort(unique(floor_t(c(seq(m, M, rx), M), b)))
                b <- self$trans$transform(b)
            } else if (is.tind(b)) {
                xtype <- .get.type(self$tind)
                brtype <- .get.type(b)
                if (xtype != brtype) {
                    if (!(xtype %in% .hi_res_cast(brtype)))
                        .stop_cast(brtype, xtype, "scale_*_tind")
                    b <- .cast(b, xtype, .get.tz(self$tind))
                } else if ((xtype == "t") && (.get.tz(self$tind) != .get.tz(b))) {
                    .warn_diff_tz(.get.tz(self$tind), .get.tz(b))
                    b <- .cast(b, xtype, .get.tz(self$tind))
                }
                b <- self$trans$transform(b)
            } else { # default
                b <- self$trans$breaks(self$tind, self$n.breaks, self$limits0)
            }
            b <- self$trans$transform(b)
            limits <- self$get_limits()
            b <- b[b >= limits[1L] & b <= limits[2L]]
            return (b)
        },
        get_breaks_minor = function(self, n = NA, b = self$break_positions(),
                                    limits = self$get_limits())
        {
            mb <- self$minor_breaks
            if (is.null(mb)) return (numeric())
            if (.is.waive(mb) && is.tind(self$breaks)) return (numeric())
            if (!.is.waive(mb) && !is.tind(mb)) {
                mes <- gettextf("invalid %s argument", sQuote("minor_breaks"))
                stop(mes, call. = FALSE, domain = NA)
            }
            if (is.tind(mb)) {
                xtype <- .get.type(self$tind)
                brtype <- .get.type(mb)
                if (xtype != brtype) {
                    if (!(xtype %in% .hi_res_cast(brtype)))
                        .stop_cast(brtype, xtype, "scale_*_tind")
                    mb <- .cast(mb, xtype, .get.tz(self$tind))
                } else if ((xtype == "t") && (.get.tz(self$tind) != .get.tz(b))) {
                    .warn_diff_tz(.get.tz(self$tind), .get.tz(b))
                    mb <- .cast(mb, xtype, .get.tz(self$tind))
                }
                mb <- self$trans$transform(mb)
            } else {
                mb <- self$trans$minor_breaks(b, limits, n)
            }
            mb <- mb[mb >= limits[1L] & mb <= limits[2L]]
            return (mb)
        },
        get_labels = function(self, breaks = self$get_breaks())
        {
            labels <- self$labels
            if (is.null(breaks) || is.null(labels)) return(NULL)
            if (.is.waive(self$labels) || .is.waive(self$breaks))
                return (self$trans$format(self$trans$inverse(breaks)))
            if (!is.character(labels)) {
                mes <- gettextf("invalid %s argument", sQuote("labels"))
                stop(mes, call. = FALSE, domain = NA)
            }
            if (length(labels) != length(breaks)) {
                mes <- gettextf("%s and %s are of different lengths",
                                sQuote("labels"), sQuote("breaks"))
                stop(mes, call. = FALSE, domain = NA)
            }
            return (labels)
        },
        rescale = function(self, x, limits = self$get_limits(), range = limits)
        {
            # ggplot2 is very aggressive in expanding limits, so we sort of
            # try and contain its behaviour
            lambda <- .5
            from <- lambda * self$get_limits() + (1 - lambda) * range
            self$rescaler(x, from = from)
        },
        map = function(self, x, limits = self$get_limits())
        {
            # map resets time indices and limits (but not transformation)
            self$tind <- self$trans$inverse(x)
            self$limits <- NULL
            # do censoring here
            x[!(self$tind %in_t% self$limits0)] <- NA
            return (x)
        },
        ## NOTE: ggplot2 before 4.0.0 requires this
        sec_name = function(self) ggplot2::waiver()
    )

    if (.is.waive(expand)) {
        ## NOTE: yet another override to make some space for labels at the ends
        expand <- c(.1, .01)
    }
    cs <- ggplot2::continuous_scale(aesthetics = paste0(axis, c("", "min", "max", "end")),
                                    name = name,
                                    palette = identity,
                                    position = position,
                                    guide = guide,
                                    breaks = breaks,
                                    labels = labels,
                                    minor_breaks = minor_breaks,
                                    n.breaks = max(0L, n.breaks - 1L),
                                    expand = expand,
                                    super = sc)
    # init
    cs$format <- format
    cs$locale <- locale
    cs$limits0 <- limits

    return (cs)
}

