#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ###################################################### #
# working with different time index types and time units #
# ###################################################### #


# list of index types, type check
# ###################################################################

.ti_type <- function(type = NULL, long = TRUE, valid = FALSE, rm.names = FALSE,
                     translate = FALSE)
{
    types <- c(y = "year", q = "quarter", m = "month", w = "week", d = "date",
               t = "date-time", h = "time of day",
               i = "integer index", n = "numeric index")
    if (!long) {
        if (is.null(type)) return (names(types))
        return (type)
    }
    # translate
    if (translate) types <- c(y = gettextf("year"),
                              q = gettextf("quarter"),
                              m = gettextf("month"),
                              w = gettextf("week"),
                              d = gettextf("date"),
                              t = gettextf("date-time"),
                              h = gettextf("time of day"),
                              i = gettextf("integer index"),
                              n = gettextf("numeric index"))
    if (is.null(type)) type <- 1:length(types)
    res <- types[type]
    if (rm.names) names(res) <- NULL
    if (valid) res <- gsub("[- ]", "_", res)
    return (res)
}


.ti_type2char <- function(type = NULL, dash = FALSE)
{
    res <- .ti_type(type, long = TRUE, valid = FALSE, rm.names = FALSE,
                    translate = TRUE)
    res <- if (dash) paste0(dQuote(names(res)), " - ", res) else
                     paste0(dQuote(names(res)), " (", res, ")")
    return (toString(res))
}


.check_type <- function(type, types = NULL)
    .match.arg(type, as.list(.ti_type(types, long = TRUE)))


.check_type_tz <- function(type, tz, types = NULL)
{
    if (!is.null(type)) {
        type <- .check_type(type, types)
        if (!is.null(tz)) {
            if (type != "t") {
                mes <- gettextf("time zone provided for type other than %s",
                                .ti_type2char("t"))
                stop(mes, call. = FALSE, domain = NA)
            }
        }
        if (type == "t") tz <- .check_tz(tz)
    } else if (!is.null(tz)) {
        type <- "t"
        tz <- .check_tz(tz)
    }
    return (list(type = type, tz = tz))
}


.expect_type <- function(xtp, types, caller = 0L)
{
    if (!(xtp %in% types)) {
        if (caller) {
            caller <- deparse(sys.calls()[[sys.nframe() - caller]][1L])
            caller <- gsub("\\(\\)$", "", caller)
            quote <- !grepl("^`.*`$", caller)
            mes0 <- gettextf("invalid time index type in %s: %s",
                             if (quote) sQuote(caller) else caller,
                             .ti_type2char(xtp))
        } else {
            mes0 <- gettextf("invalid time index type: %s", .ti_type2char(xtp))
        }
        mes1 <- if (length(types) == 1L) gettextf("expected: %s",
                                                 .ti_type2char(types))
                else gettextf("expected one of the following: %s",
                              .ti_type2char(types))
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
}

# instant types and storage modes
# ###################################################################

.is.instant <- function(x) (x %in% c("t", "h", "n"))

.mode <- function(x) if (.is.instant(x)) "double" else "integer"


# limits
# ###################################################################

.limits <- function(tp)
{
    if (tp %in% c("i", "n")) return (c(-Inf, Inf))
    return (.Call(C_tlimits, tp))
}


# time index components
# ###################################################################

.ti_components <- function()
{
    compnts <- list(y = list(c("y")),
                    q = list(c("y", "q")),
                    m = list(c("y", "m")),
                    w = list(c("y", "w")),
                    d = list(c("y", "m", "d"), c("y", "j"), c("y", "w", "u")),
                    h = list(c("H", "M", "S"), c("H", "M"), c("H"),
                             c("I", "M", "S", "p"), c("I", "M", "p"), c("I", "p")),
                    t = list())
    for (i in seq_along(compnts[["h"]]))
        compnts[["t"]] <- c(compnts[["t"]], lapply(compnts[["d"]],
                                        function(x) c(x, compnts[["h"]][[i]])))
    compnts[["t"]] <- c(lapply(compnts[["t"]], function(x) c(x, "z")),
                        compnts[["t"]])
    return (compnts)
}


.ti_comp2char <- function(comp = NULL)
{
    compts <- c(y = gettextf("year"),
                q = gettextf("quarter"),
                m = gettextf("month"),
                w = gettextf("week"),
                d = gettextf("day"),
                j = gettextf("day of year"),
                u = gettextf("day of week"),
                H = gettextf("hour"),
                M = gettextf("minute"),
                S = gettextf("second"),
                I = gettextf("hour, 12-hour clock"),
                p = gettextf("AM/PM indicator"),
                z = gettextf("time zone / UTC offset"))
    if (!is.null(comp)) compts <- compts[comp]
    res <- toString(paste0(dQuote(names(compts)), " (", compts, ")"))
    return (res)
}


# this returns two-element list with inferred type and ignored components
.infer_type <- function(comp, numonly = FALSE)
{
    compnts <- .ti_components()
    ucompnts <- unique(unlist(compnts))
    if (numonly) ucompnts <- setdiff(ucompnts, c("I", "p", "z"))
    if (!all(comp %in% ucompnts)) {
        mes1 <- if (numonly) gettextf("unrecognised / unsupported time index components: %s",
                                      toString(dQuote(setdiff(comp, ucompnts))))
                else gettextf("unrecognised time index components: %s",
                              toString(dQuote(setdiff(comp, ucompnts))))
        mes2 <- gettextf("Hint: recognised components are: %s",
                        .ti_comp2char(ucompnts))
        stop(paste0(mes1, "\n", mes2), call. = FALSE, domain = NA)
    }
    for (tp in rev(names(compnts))) {
        cp <- compnts[[tp]]
        for (j in seq_along(cp)) {
            if (all(cp[[j]] %in% comp))
                return (list(type = tp, ignored = setdiff(comp, cp[[j]])))
        }
    }
    mes <- gettextf("cannot infer type / construct time index based on the following components: %s",
                    .ti_comp2char(comp))
    stop(mes, call. = FALSE, domain = NA)
}


# possible type casts, highest / lowest resolution
# ###################################################################

.hi_res_cast <- function(x)
{
    hi <- list(y = c("q", "m", "w", "d", "t"),
               q = c("m", "d", "t"),
               m = c("d", "t"),
               w = c("d", "t"),
               d = c("t"),
               t = character(),
               h = character(),
               i = c("n"),
               n = character())
    return (hi[[x]])
}


.lo_res_cast <- function(x)
{
    lo <- list(y = character(),
               q = c("y"),
               m = c("q", "y"),
               w = c("y"),
               d = c("w", "m", "q", "y"),
               t = c("h", "d", "w", "m", "q", "y"),
               h = character(),
               i = character(),
               n = character())
    return (lo[[x]])
}


.stop_cast <- function(t1, t2, caller = 0L)
{
    if (is.integer(caller)) {
        caller <- deparse(sys.calls()[[sys.nframe() - caller - 1L]][1L])
        caller <- sub("\\(\\)$", "", caller)
        caller <- sub("\\.(tind|tinterval)$", "", caller)
    }
    mes <- gettextf("cast from time index type %s to type %s in %s not possible",
                    .ti_type2char(t1), .ti_type2char(t2), sQuote(caller))
    stop(mes, call. = FALSE, domain = NA)
}


.max_res <- function(x, caller = 0L, weak = FALSE)
{
    x <- unique(x)
    if (length(x) == 1L) return (x)
    types1 <- c("y", "q", "m", "w", "d", "h", "t")
    types2 <- c("i", "n")
    if (any(x %in% types1) && any(x %in% types2))
        .stop_cast(types2[min(match(x, types2), na.rm = TRUE)],
                   types1[max(match(x, types1), na.rm = TRUE)],
                   if (is.numeric(caller)) caller + 1L else caller)
    if (all(x %in% types2)) return ("n")
    i <- match(x, types1)
    mi <- max(i)
    i <- sort(i)
    i <- i[i != mi]
    # weak means that not everything can be cast up but the max resolution
    # can be cast down to everything
    ok <- if (weak) all(types1[i] %in% .lo_res_cast(types1[mi]))
          else sapply(i, function(i) (types1[mi] %in% .hi_res_cast(types1[i])))
    if (all(ok)) return (types1[mi])
    caller <- if (is.numeric(caller)) caller + 1L else caller
    if (weak) .stop_cast(types1[mi], types1[i[which.min(ok)]], caller)
    .stop_cast(types1[i[which.min(ok)]], types1[mi], caller)
}



# list of time units
# ###################################################################

.t_unit <- function(unit = NULL, long = TRUE, valid = FALSE, rm.names = FALSE,
                    translate = FALSE)
{
    units <- c(y = "year", q = "quarter", m = "month", w = "week", d = "day",
               h = "hour", min = "minute", s = "second")
    if (!long) {
        if (is.null(unit)) return (names(units))
        return (unit)
    }
    # translate
    if (translate) units <- c(y =   gettextf("year"),
                              q =   gettextf("quarter"),
                              m =   gettextf("month"),
                              w =   gettextf("week"),
                              d =   gettextf("day"),
                              h =   gettextf("hour"),
                              min = gettextf("minute"),
                              s =   gettextf("second"))
    if (is.null(unit)) unit <- 1:length(units)
    res <- units[unit]
    if (rm.names) names(res) <- NULL
    if (valid) res <- gsub("[- ]", "_", res)
    return (res)
}


.t_unit2char <- function(unit = NULL, dash = FALSE)
{
    res <- .t_unit(unit, long = TRUE, rm.names = FALSE, translate = TRUE)
    res <- if (dash) paste0(dQuote(names(res)), " - ", res) else
                     paste0(dQuote(names(res)), " (", res, ")")
    return (toString(res))
}


.check_unit <- function(unit, units = NULL)
    .match.arg(unit, as.list(.t_unit(units, long = TRUE)))



# check increment unit
# ###################################################################

.check_inc_x_by_y <- function(x, n, y)
{
    if (is.null(y)) return (list(n = n, unit = NULL))

    if (x %in% c("i", "n")) {
        mes <- gettextf("time units not supported with type %s", .ti_type2char(x))
        stop(mes, call. = FALSE, domain = NA)
    }

    tuples <- list(y = list(y = 1L),
                   q = list(q = 1L, y = 4L),
                   m = list(m = 1L, q = 3L, y = 12L),
                   w = list(w = 1L, y = list(1L, "y")),
                   d = list(d = 1L, w = 7L, m = list(1L, "m"),
                            q = list(3L, "m"), y = list(12L, "m")),
                   h = list(s = 1, min = 60, h = 3600),
                   t = list(s = 1, min = 60, h = 3600,
                            d = list(1L, "d"), w = list(7L, "d"),
                            m = list(1L, "m"), q = list(3L, "m"),
                            y = list(12L, "m")))

    if (!(y %in% names(tuples[[x]]))) {
        mes0 <- gettextf("invalid time unit for type %s", .ti_type2char(x))
        mes1 <- gettextf("admissible units: %s",
                         .t_unit2char(names(tuples[[x]])))
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
    mlt <- tuples[[x]][[y]]
    if (!is.list(mlt)) return (list(n = n * mlt, unit = NULL))
    return (list(n = n * mlt[[1]], unit = mlt[[2]]))
}


# admissible units for floor_t, ceiling_t, cut
# ###################################################################

.check_fl_ceil_type_unit <- function(tp, un)
{
    if (tp %in% c("i", "n")) {
        if (!is.null(un$unit)) {
            mes <- gettextf("time unit provided for type %s", .ti_type2char(tp))
            stop(mes, call. = FALSE, domain = NA)
        }
        un$type <- un$unit <- tp
    }
    if (tp == "i") {
        uni <- suppressWarnings(as.integer(un$n))
        if ((length(uni) != 1L) || !is.finite(uni) || (uni <= 0L) || (uni != un$n)) {
            mes <- gettextf("positive integer expected for type %s", .ti_type2char("i"))
            stop(mes, call. = FALSE, domain = NA)
        }
        un$n <- uni
        return (un)
    }
    if (tp == "n") {
        uni <- suppressWarnings(as.double(un$n))
        if ((length(uni) != 1L) || !is.finite(un$n) || (un$n <= 0L)) {
            mes <- gettextf("positive number expected for type %s", .ti_type2char("n"))
            stop(mes, call. = FALSE, domain = NA)
        }
        return (un)
    }

    if (is.null(un$unit)) {
        un$unit <- if (tp %in% c("h", "t")) "s" else tp
        un$type <- tp
    } else {
        un$type <- if (un$unit %in% c("h", "min", "s")) tp else un$unit
    }

    units <- list(y = c("y"),
                  q = c("q", "y"),
                  m = c("m", "q", "y"),
                  w = c("w", "y"),
                  d = c("d", "w", "m", "q", "y"),
                  t = c("s", "min", "h", "d", "w", "m", "q", "y"),
                  h = c("s", "min", "h"))
    if (!(un$unit %in% units[[tp]])) {
        mes0 <- gettextf("invalid time unit for type %s", .ti_type2char(tp))
        mes1 <- gettextf("admissible units: %s", .t_unit2char(units[[tp]]))
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }

    .check_mult(un$n, un$unit)
    if (un$unit != "s") un$n <- as.integer(un$n)
    return (un)
}

