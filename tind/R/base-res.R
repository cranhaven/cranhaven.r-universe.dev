#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ########################## #
# resolution of time indices #
# ########################## #


.mults <- function(unit, full = FALSE)
{
    ml <- list(y = c(1L, 2L, 5L, 10L, 20L, 25L, 50L, 100L, 200L, 250L, 500L, 1000L),
               q = c(1L, 2L, 4L),
               m = c(1L, 2L, 3L, 4L, 6L, 12L),
               w = c(1L, 2L, 4L, 13L, 26L, 52L),
               d = c(1L:3L, 7L, 15L, 30L),
               h = c(1L, 2L, 3L, 4L, 6L, 8L, 12L, 24L),
               min = c(1L, 2L, 3L, 4L, 5L, 6L, 10L, 12L, 15L, 20L, 30L, 60L),
               s = c(0.000001, 0.000002, 0.000005, 0.00001, 0.00002, 0.00005,
                       0.0001,   0.0002,   0.0005,   0.001,   0.002,   0.005,
                         0.01,     0.02,     0.05,     0.1,     0.2,     0.5,
                     1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 60))
    m <- ml[[unit]]
    return (if (full || (unit == "y")) m
            else if (unit == "d") c(1L, 15L)
            else m[-length(m)])
}


.check_mult <- function(n, unit)
{
    if (!(n %in% .mults(unit))) {
        mes0 <- gettextf("invalid multiplier for time unit %s", .t_unit2char(unit))
        mes1 <- gettextf("admissible values: %s", toString(.mults(unit)))
        stop(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
    }
}


.res_y <- function(y) list(n = .Call(C_res_y, y), unit = "y")


.res_q <- function(q)
{
    n <- .Call(C_res_q, q)
    if (n != 4L) return (list(n = n, unit = "q"))
    return (list(n = .Call(C_res_y, .q2y(q)), unit = "y"))
}


.res_m <- function(m)
{
    n <- .Call(C_res_m, m)
    if (n != 12L) return (if (n %% 3L) list(n = n, unit = "m")
                          else list(n = n %/% 3L, unit = "q"))
    return (list(n = .Call(C_res_y, .m2y(m)), unit = "y"))
}


.res_w <- function(w)
{
    n <- .Call(C_res_w, w)
    if (n != 52L) return (list(n = n, unit = "w"))
    return (list(n = .Call(C_res_y, .w2y(w)), unit = "y"))
}


.res_d <- function(d)
{
    n <- .Call(C_res_d, d)
    if (n == 30L) return (.res_m(.d2m(d)))
    if (n == 7L) return (.res_w(.d2w(d)))
    return (list(n = n, unit = "d"))
}


.res_t <- function(t, tz)
{
    n <- .Call(C_res_s, t)
    if (n && (n != 60)) return (list(n = n, unit = "s"))
    if (!n) return (list(n = .Call(C_res_subs, t), unit = "s"))
    ## FIXME: The code below might not work correctly for time zones in which
    ## some days do not start at 00:00 or with double midnights (second occurrence
    ## of 00:00 might be misundestood as the start of the day).
    dh <- .t2dhz(t, tz = tz)
    n <- .Call(C_res_min, dh$h)
    if (n != 60) return (list(n = n, unit = "min"))
    n <- .Call(C_res_h, dh$h)
    if (n != 24) return (list(n = n, unit = "h"))
    return (.res_d(dh$d))
}


.res_h <- function(h)
{
    n <- .Call(C_res_s, h)
    if (n && (n != 60)) return (list(n = n, unit = "s"))
    if (!n) return (list(n = .Call(C_res_subs, h), unit = "s"))
    n <- .Call(C_res_min, h)
    if (n != 60) return (list(n = n, unit = "min"))
    n <- .Call(C_res_h, h)
    if (n == 24L) n <- 1L
    return (list(n = n, unit = "h"))
}


.res_i <- function(i) .Call(C_res_i, i)


.res_n <- function(x)
{
    x <- as.vector(x)
    if (!.Call(C_is_ordered, x, TRUE)) {
        if (anyNA(x)) x <- x[!is.na(x)]
        x <- sort(unique(x))
    }
    n <- length(x)

    if (n == 0L) return (NA_real_)
    if (n == 1L) {
        if (x == 0.) return (NA_real_)
        return (x)
    }

    mindx <- min(diff(x))
    rax <- range(abs(x))
    minax <- rax[1L]
    maxax <- rax[2L]
    if (minax > 0.) mindx <- min(mindx, minax)
    x <- x / mindx
    if (max(abs(x - round(x))) > maxax * .Machine$double.eps^.5) return (NA_real_)
    return (mindx)
}

