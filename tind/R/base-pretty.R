#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ###################### #
# pretty time axis ticks #
# ###################### #


# auxiliary function
# ###################################################################

.min_incs <- function(n, min.n)
{
    st <- 0L + (1L + n - min.n) %/% 2L
    return (st + (0L:min.n))
}


# years
# ###################################################################

.pretty_y <- function(x, n = 5L, min.n = 2L, r = NULL)
{
    if (is.null(r)) r <- .res_y(x)[[1L]]
    rx <- range(x)
    dx <- diff(rx)

    if (!n) { if (!dx) return (rx[1L]) else n <- 1L }
    if (dx < min.n * r) return (rx[1L] + .min_incs(dx %/% r, min.n) * r)

    rs <- .mults("y")
    rs <- rs[!(rs %% r)]
    alts <- lapply(rs, function(r) as.integer(seq(floor(rx[1L] / r),
                                                  ceiling(rx[2L] / r)) * r))
#     alts <- lapply(rs, function(r) seq.int(.floor_y(rx[1L], r),
#                                            .ceiling_y(rx[2L], r), r))
    lnghts <- sapply(alts, length) - 1L
    lnghts[lnghts < min.n] <- Inf
    ii <- which.min(abs(lnghts - n))
    return (alts[[ii]])
}



# quarters
# ###################################################################

.pretty_q <- function(x, n = 5L, min.n = 2L, r = NULL)
{
    if (is.null(r)) {
        r <- .res_q(x)
        if (r$unit == "y") return (.y2q(.pretty_y(.q2y(x), n, min.n, r = r$n)))
        r <- r$n
    }

    rx <- range(x)
    dx <- diff(rx)

    if (!n) { if (!dx) return (rx[1L]) else n <- 1L }
    if (dx < min.n * r) return (rx[1L] + .min_incs(dx %/% r, min.n) * r)

    rs <- .mults("q", TRUE)
    rs <- rs[!(rs %% r)]
    alts <- lapply(rs, function(r) seq.int(.floor_q(rx[1L], r),
                                           .ceiling_q(rx[2L], r), r))
    lnghts <- sapply(alts, length) - 1L
    lnghts[lnghts < min.n] <- Inf
    ii <- which.min(abs(lnghts - n))
    if (rs[ii] == 4L) return (.y2q(.pretty_y(.q2y(alts[[ii]]), n, min.n, 1L)))
    return (alts[[ii]])
}



# months
# ###################################################################

.pretty_m <- function(x, n = 5L, min.n = 2L, r = NULL)
{
    if (is.null(r)) {
        r <- .res_m(x)
        if (r$unit == "y") return (.y2m(.pretty_y(.m2y(x), n, min.n, r = r$n)))
        if (r$unit == "q") return (.q2m(.pretty_q(.m2q(x), n, min.n, r = r$n)))
        r <- r$n
    }

    rx <- range(x)
    dx <- diff(rx)

    if (!n) { if (!dx) return (rx[1L]) else n <- 1L }
    if (dx < min.n * r) return (rx[1L] + .min_incs(dx %/% r, min.n) * r)

    rs <- .mults("m", TRUE)
    rs <- rs[!(rs %% r)]
    if (!(r %in% c(2L, 4L))) rs <- setdiff(rs, c(2L, 4L))
    alts <- lapply(rs, function(r) seq.int(.floor_q(rx[1L], r),
                                           .ceiling_q(rx[2L], r), r))
    lnghts <- sapply(alts, length) - 1L
    lnghts[lnghts < min.n] <- Inf
    ii <- which.min(abs(lnghts - n))
    if (rs[ii] == 12L) return (.y2m(.pretty_y(.m2y(alts[[ii]]), n, min.n, 1L)))
    return (alts[[ii]])
}


# weeks
# ###################################################################

.pretty_w <- function(x, n = 5L, min.n = 2L, r = NULL)
{
    if (is.null(r)) {
        r <- .res_w(x)
        if (r$unit == "y") return (.y2w(.pretty_y(.w2y(x), n, min.n, r = r$n)))
        r <- r$n
    }

    rx <- range(x)
    dx <- diff(rx)

    if (!n) { if (!dx) return (rx[1L]) else n <- 1L }
    if (dx < min.n * r) {
        incs <- .min_incs(as.integer(ceiling(dx / r)), min.n)
        if (r == 1L) return (rx[1L] + incs)
        x0 <- .floor_w(rx[1L] + incs[1L] * r, r)
        x1 <- .ceiling_w(rx[1L] + incs[length(incs)] * r, r)
        return (unique(.floor_w(x0:x1, r)))
    }

    rs <- .mults("w", TRUE)
    rs <- rs[!(rs %% r)]
    alts <- lapply(rs, function(r) unique(.floor_w(seq.int(.floor_w(rx[1L], r),
                                                           .ceiling_w(rx[2L], r)), r)))
    lnghts <- sapply(alts, length) - 1L
    lnghts[lnghts < min.n] <- Inf
    ii <- which.min(abs(lnghts - n))
    if (rs[ii] == 52L) return (.y2w(.pretty_y(.w2y(alts[[ii]]), n, min.n, 1L)))
    return (alts[[ii]])
}



# days
# ###################################################################

.pretty_d <- function(x, n = 5L, min.n = 2L, r = NULL)
{
    if (is.null(r)) {
        r <- .res_d(x)
        if (r$unit == "y") return (.y2d(.pretty_y(.d2y(x), n, min.n, r = r$n)))
        if (r$unit == "q") return (.q2d(.pretty_q(.d2q(x), n, min.n, r = r$n)))
        if (r$unit == "m") return (.m2d(.pretty_m(.d2m(x), n, min.n, r = r$n)))
        if (r$unit == "w") return (.w2d(.pretty_w(.d2w(x), n, min.n, r = r$n)))
        r <- r$n
    }

    rx <- range(x)
    dx <- diff(rx)

    if (!n) { if (!dx) return (rx[1L]) else n <- 1L }
    if (dx < min.n * r) {
        incs <- .min_incs(as.integer(ceiling(dx / r)), min.n)
        if (r == 1L) return (rx[1L] + incs)
        x0 <- .floor_d(rx[1L] + incs[1L] * r, r)
        x1 <- .ceiling_d(rx[1L] + incs[length(incs)] * r, r)
        return (unique(.floor_d(x0:x1, r)))
    }

    rs <- rev(.mults("d", TRUE))
    rs <- rs[!(rs %% r)]
    alts <- lapply(rs, function(r) unique(.floor_d(seq.int(.floor_d(rx[1L], r),
                                                           .ceiling_d(rx[2L], r)), r)))
    lnghts <- sapply(alts, length) - 1L
    lnghts[lnghts < min.n] <- Inf
    ii <- which.min(abs(lnghts - n))
    if (rs[ii] == 30L) return (.m2d(.pretty_m(.d2m(alts[[ii]]), n, min.n, 1L)))
    return (alts[[ii]])
}



# date-time
# ###################################################################

.pretty_t <- function(tt, tz, n = 5L, min.n = 2L)
{
    r <- .res_t(tt, tz)
    if (r$unit == "y") return (.y2t(.pretty_y(.t2y(tt, tz), n, min.n, r$n), tz))
    if (r$unit == "q") return (.q2t(.pretty_q(.t2q(tt, tz), n, min.n, r$n), tz))
    if (r$unit == "m") return (.m2t(.pretty_m(.t2m(tt, tz), n, min.n, r$n), tz))
    if (r$unit == "w") return (.w2t(.pretty_w(.t2w(tt, tz), n, min.n, r$n), tz))
    if (r$unit == "d") return (.d2t(.pretty_d(.t2d(tt, tz), n, min.n, r$n), tz))

    rt <- range(tt); t0 <- rt[1L]; t1 <- rt[2L]; dt <- diff(rt)

    if (!n) { if (dt == 0) return (rt[1L]) else n <- 1L }
    if (dt >= 86400 * n) {
        d0 <- .t2d(t0, tz)
        d1 <- .t2d(t1, tz)
        if (t1 > .d2t(d1, tz)) d1 <- d1 + 1L
        return (.d2t(.pretty_d(c(d0, d1), n, min.n, 1L), tz))
    }
    if (r$unit == "h") return (.pretty_hms("h", t0, t1, tz, n, min.n, r$n))
    if (dt >= 3600 * n) return (.pretty_hms("h", t0, t1, tz, n, min.n, 1L))
    if (r$unit == "min") return (.pretty_hms("min", t0, t1, tz, n, min.n, r$n))
    if (dt >= 60 * n) return (.pretty_hms("min", t0, t1, tz, n, min.n, 1L))
    if (r$n >= 1) return (.pretty_hms("s", t0, t1, tz, n, min.n, r$n))
    if (dt >= n) return (.pretty_hms("s", t0, t1, tz, n, min.n, 1))
    return (.pretty_n(tt, n = n, min.n = min.n))
}


.pretty_hms <- function(u, t0, t1, tz, n, min.n, r)
{
    unit <- switch(u, h = 3600, min = 60, s = 1)
    t0 <- .floorceil_t(t0, r, u, tz, TRUE)
    t1 <- .floorceil_t(t1, r, u, tz, FALSE)
    dx <- ceiling((t1 - t0) / (unit * r))
    if (dx <= n) {
        incs <- if (dx < min.n) .min_incs(dx, min.n) else 0:dx
        pt <- c(t0 + unit * r * incs, t1)
        pt <- unique(.floorceil_t(pt, r, u, tz, TRUE))
        return (pt)
    }

    rs <- rev(.mults(u, TRUE))
    if (u == "h") {
        if (!(r %in% c(4L, 8L))) rs <- setdiff(rs, c(4L, 8L))
    } else {
        if (!(r %in% c(3, 6, 12))) rs <- setdiff(rs, c(6, 12))
        if (!(r %in% c(10, 20))) rs <- setdiff(rs, c(10, 20))
    }
    rs <- rs[!(rs %% r)]
    dx <- ceiling((t1 - t0) / unit)
    r <- rs[which.min(abs(n - dx / rs))]
    t0 <- .floorceil_t(t0, r, u, tz, TRUE)
    t1 <- .floorceil_t(t1, r, u, tz, FALSE)
    dx <- ceiling((t1 - t0) / (unit * r))
    if (dx < min.n) {
        incs <- .min_incs(dx, min.n)
        pt <- c(t0 + unit * r * incs, t1)
        pt <- unique(.floorceil_t(pt, r, u, tz, TRUE))
        return (pt)
    }
    return (unique(.floorceil_t(c(seq(t0, t1, by = unit), t1), r, u, tz, TRUE)))
}


# time of day
# ###################################################################

.pretty_h <- function(hh, n = 5L, min.n = 2L)
{
    r <- .res_h(hh)
    rh <- range(hh); h0 <- rh[1L]; h1 <- rh[2L]; dh <- diff(rh)
    un <- switch(r$unit, h = 3600, min = 60, s = 1) * r$n
    dx <- round(dh / un)
    if (!n) { if (dh == 0) return (rh[1L]) else n <- 1L }
    if (dx <= n) {
        incs <- if (dx < min.n) .min_incs(dx, min.n) else 0:dx
        return (round(h0 + un * incs, 6L))
    }

    mltsh <- .mults("h")
    if ((r$unit != "h") || !(r$n %in% c(4L, 8L))) mltsh <- setdiff(mltsh, c(4L, 8L))
    mltsm <- .mults("min")
    if ((r$unit != "min") || !(r$n %in% c(3, 6, 12))) mltsm <- setdiff(mltsm, c(6, 12))
    if ((r$unit != "min") || !(r$n %in% c(10, 20))) mltsm <- setdiff(mltsm, c(10, 20))
    mltss <- .mults("s")
    if (!(r$n %in% c(3, 6, 12))) mltss <- setdiff(mltss, c(6, 12))
    if (!(r$n %in% c(10, 20))) mltss <- setdiff(mltss, c(10, 20))

    if (r$unit == "s") {
        if (r$n < 1) {
            f0 <- as.integer(round(r$n * 1e6))
            f0s <- as.integer(round(mltss[mltss < 1] * 1e6))
            rs <- mltss[!(mltss %% r$n)]
            rs <- c(rs, mltss[mltss >= 1])
        } else {
            rs <- mltss[mltss >= 1 & !(mltss %% r$n)]
        }
        rs <- c(rs, mltsm * 60, mltsh * 3600)
    } else if (r$unit == "min") {
        rs <- c(mltsm[!(mltsm %% r$n)] * 60, mltsh * 3600)
    } else {
        rs <- mltsh[!(mltsh %% r$n)] * 3600
    }
    rs <- rev(rs)
    rs <- rs[dh / rs >= min.n]
    r <- rs[which.min(abs(n - dh / rs))]
    return (round(floor(h0 / r):ceiling(h1 / r) * r, digits = 6L))
}


# integer and numeric indices
# ###################################################################

.pretty_i <- function(i, n = 5L, min.n = 2L)
{
    ri <- range(i)
    i0 <- ri[1L]
    i1 <- ri[2L]
    di <- i1 - i0
    if (!n) { if (!di) return (i0) else n <- 1L }
    if (di <= min.n) {
        pt <- i0 + .min_incs(di, min.n)
        if ((i0 >= 0L) && (pt[1L] < 0L)) pt <- pt - pt[1L]
        else if ((i1 <= 0L) && (pt[length(pt)] > 0L)) pt <- pt - pt[length(pt)]
        return (pt)
    }
    if (di <= n) return (i0:i1)
    return (as.integer(.pretty_n(ri, n = n, min.n = min.n)))
}


.pretty_n <- function(x, n = 5L, min.n = 2L)
{
    ## NOTE: a simple alternative to base::pretty
    rx <- range(x); x0 <- rx[1L]; x1 <- rx[2L]; dx <- diff(rx)
    if (!n) { if (dx == 0.) return (x0) else n <- 1L }
    if (dx == 0) {
        if (!n) return (x0)
        if (x0 == 0) { x1 <- dx <- 1 }
        else { x0 <- x0 - .1 * abs(x0); x1 <- x1 + .1 * abs(x1); dx <- x1 - x0 }
    } else if (!n) n <- 1
    p10 <- 10^floor(log10(dx / n))
    mults <- c(1, 2, 5, 10, 20, 50, 100)
    alts <- ceiling(x1 / p10 / mults) - floor(x0 / p10 / mults)
    imin <- (alts >= min.n); mults <- mults[imin]; alts <- alts[imin]
    m <- mults[which.min(abs(alts - n))]
    return (floor(x0 / p10 / m):ceiling(x1 / p10 / m) * p10 * m)
}

