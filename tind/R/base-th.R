#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ######### #
# date-time #
# ######### #


## NOTE: valid range is from 0000-01-01 15:00:00 UTC to 9999-12-31 09:00:00 UTC
## i.e. from -62167165200 to 253402246800

.validate_t <- function(t) .Call(C_validate_t, .require_mode(t, "double"))


## NOTE: valid range is from 00:00:00 to 24:00:00

.validate_h <- function(h) .Call(C_validate_h, .require_mode(h, "double"))


.validate_hms <- function(h, m, s)
    .Call_3("validate_hms", .require_mode(h, "integer"),
                            .require_mode(m, "integer"),
                            .require_mode(s, "double"))


## NOTE: The five functions below rely on as.POSIXlt.numeric.

.t2d <- function(t, tz)
{
    # fast exit for UTC and time zones with fixed offset
    if (tz %in% c(.tz_utc(), .tz_fixed())) {
        if (tz %in% .tz_fixed()) t <- t + .tz_fixed_offset(tz)
        return (.validate_d(floor(t / 86400)))
    }
    plt <- as.POSIXlt(t, origin = "1970-01-01 00:00:00", tz = tz)
    return (.Call(C_plt_ymd2d, plt$year, plt$mon, plt$mday, names(t)))
}

.t2h <- function(t, tz)
{
    # fast exit for UTC and time zones with fixed offset
    if (tz %in% c(.tz_utc(), .tz_fixed())) {
        if (tz %in% .tz_fixed()) t <- t + .tz_fixed_offset(tz)
        return (round(t %% 86400, 6L))
    }
    plt <- as.POSIXlt(t, origin = "1970-01-01 00:00:00", tz = tz)
    return (.Call(C_plt_hms2h, plt$hour, plt$min, plt$sec, names(t)))
}

.t2dhz <- function(t, tz, gmtoff = FALSE, zone = FALSE)
{
    plt <- as.POSIXlt(t, origin = "1970-01-01 00:00:00", tz = tz)
    res <- list(d = .Call(C_plt_ymd2d, plt$year, plt$mon, plt$mday, names(t)),
                h = .Call(C_plt_hms2h, plt$hour, plt$min, plt$sec, names(t)))
    if (gmtoff) {
        # not NULL but 0-length for UTC
        gmtoff <- if (tz %in% .tz_utc()) integer() else plt$gmtoff
        res <- c(res, list(gmtoff = gmtoff))
    }
    if (zone) {
        # not NULL but 0-length for UTC
        zone <- if (tz %in% .tz_utc()) character() else plt$zone
        if (!is.null(zone) && any(grepl("^[-+0-9]", zone))) zone <- NULL
        res <- c(res, list(zone = zone))
    }
    return (res)
}


# This is an auxiliary function and does not support NAs
.midnightdiff <- function(t, d, tz)
{
    plt <- as.POSIXlt(t, origin = "1970-01-01 00:00:00", tz = tz)
    return (.Call(C_plt_midnightdiff, d, plt$year, plt$mon, plt$mday,
                  plt$hour, plt$min, plt$sec))
}


.isdst_t <- function(t, tz)
{
    if (tz %in% c(.tz_utc(), .tz_fixed())) return (is.na(t) & NA)
    plt <- as.POSIXlt(t, origin = "1970-01-01 00:00:00", tz = tz)
    isdst <- rep(FALSE, length(t))
    names(isdst) <- names(t)
    isdst[plt$isdst > 0L] <- TRUE
    isdst[plt$isdst < 0L] <- NA
    return (isdst)
}


.t2hour <- function(t, tz) .h2hour(.t2h(t, tz))

.t2min <- function(t, tz)
{
    mint <- suppressWarnings(min(t, na.rm = TRUE))
    if (!is.finite(mint)) return (rep(NA_integer_, length(t)))
    if ((mint >= 567993600) && !(tz %in% .tz_nfh88()))
        return (.h2min(t))
    return (.h2min(.t2h(t, tz)))
}

.t2sec <- function(t, tz)
{
    mint <- suppressWarnings(min(t, na.rm = TRUE))
    if (!is.finite(mint)) return (t)
    if ((mint >= -283996800) && !(tz %in% .tz_n30m61()))
        return (.h2sec(t))
    return (.h2sec(.t2h(t, tz)))
}


.t2char <- function(t, tz, gmtoff = TRUE, zone = FALSE)
{
    nms <- names(t)
    n <- length(t)
    if (!n) return (character())

    dhz <- .t2dhz(t, tz, gmtoff, zone)

    if (zone) {
        zone <- dhz$zone
        if (!is.null(zone)) gmtoff <- FALSE
    } else zone <- NULL
    if (gmtoff) {
        gmtoff <- dhz$gmtoff
    } else gmtoff <- NULL

    return (.Call(C_t2char, dhz$d, dhz$h, gmtoff, zone))
}


.h2char <- function(h) .Call(C_h2char, h)

.h2hour <- function(h) .Call(C_h2hour, h)

.h2min <- function(h) .Call(C_h2min, h)

.h2sec <- function(h) .Call(C_h2sec, h)


.t2w <- function(t, tz) .d2w(.t2d(t, tz))

.t2m <- function(t, tz) .d2m(.t2d(t, tz))

.t2q <- function(t, tz) .d2q(.t2d(t, tz))

.t2y <- function(t, tz) .d2y(.t2d(t, tz))


.d2t0 <- function(d, tz)
{
    ## NOTE: Tested to work correctly for all dates since 1923-01-01 in 598 time
    ## zones supported by R 4.3.1 on Linux. For most time zones should also work
    ## correctly before that date.
    ## The are three main reasons behind complexity of this code:
    ## - There are over 100 time zones in which midnight was missing (a day did
    ##   not start at 00:00) at least once due to UTC offset or DST changes.
    ## - There are over 30 time zones with at least one day with double midnight
    ##   (e.g. DST change by going back to 00:00 at 01:00).
    ## - There are a few time zones in which a day was missing

    # 0-length inputs
    if (!(n <- length(d))) return (numeric())
    # it's simple in UTC
    if (tz %in% .tz_utc())
        return (d * 86400)
    # ...and in time zones with fixed offset
    if (tz %in% .tz_fixed())
        return (d * 86400 - .tz_fixed_offset(tz))
    # handle missing days
    missingd <- .tz_missing_days()
    if (tz %in% names(missingd)) {
        md <- d %in% missingd[[tz]]
        if (any(md)) {
            mes0 <- gettextf("NAs introduced")
            mes1 <- gettextf("invalid date (%s) for time zone %s",
                             toString(.d2char(d[md])), tz)
            warning(paste0(mes0, "; ", mes1), call. = FALSE, domain = NA)
            d[md] <- NA_integer_
        }
    }
    # handle NAs and minimise the use of date-time -> date conversions
    rng <- .Call(C_range, d, TRUE)
    if (is.na(rng[1L])) return (rep(NA_real_, n))
    drng <- diff(rng)
    if (length(d) > drng) {
        d1 <- rng[1L] + as.integer(0L:drng)
    } else {
        d1 <- unique(d)
        d1 <- d1[!is.na(d1)]
    }
    if (rng[1L] < -17167L) {
        mes <- gettextf("results for dates before %s might be incorrect",
                        "1923-01-01")
        warning (mes, call. = FALSE, domain = NA)
    }
    map <- match(d, d1)

    # handle double midnights
    dblmidnight <- tz %in% .tz_dblmidnight(.d2y(rng[1L]))
    # start with midgnith in UTC and adjust towards 00:00:00
    t1 <- d1 * 86400
    md1 <- .midnightdiff(t1, d1, tz)
    t1 <- t1 - md1
    # 1st check
    md1 <- .midnightdiff(t1, d1, tz)
    nok <- as.logical(md1)
    if (!any(nok) && !dblmidnight) return (t1[map])
    # 2nd check
    t1[nok] <- t1[nok] - md1[nok]
    nok <- d1 != .t2d(t1, tz)
    if (!any(nok) && !dblmidnight) return (t1[map])
    # 3rd check
    t1[nok] <- t1[nok] - .midnightdiff(t1[nok], d1[nok], tz)
    if (!dblmidnight) return (t1[map])
    # in some time zones we might have gone too far into the day
    for (back in c(10800, 7200, 3600, 1800)) {
        okback <- !as.logical(.midnightdiff(t1 - back, d1, tz))
        if (any(okback)) t1[okback] <- t1[okback] - back
    }
    # finally...
    return (t1[map])
}


.d2t <- function(d, tz)
{
    t <- .d2t0(d, tz)
    names(t) <- names(d)
    return (t)
}

.w2t <- function(w, tz) .d2t(.w2d(w), tz)

.m2t <- function(m, tz) .d2t(.m2d(m), tz)

.q2t <- function(q, tz) .d2t(.q2d(q), tz)

.y2t <- function(y, tz) .d2t(.y2d(y), tz)


.t2yf <- function(t, tz)
{
    n <- length(t)
    if (!n) return (numeric())
    rt <- .Call(C_range, t, TRUE)
    if (!is.finite(rt[1L])) return (rep(NA_real_, n))
    ry <- .t2y(rt, tz) + 0L:1L
    ys <- as.integer(seq.int(ry[1L], ry[2L]))
    ts <- .y2t(ys, tz)
    if (requireNamespace("stats", quietly = TRUE)) {
        res <- stats::approx(ts, ys, t)$y
    } else {
        ii <- findInterval(t, ts)
        ii[(ii < 1L) | (ii > length(ts))] <- NA
        dts <- diff(ts)
        dys <- diff(ys)
        res <- ys[ii] + dys[ii] * (t - ts[ii]) / dts[ii]
    }
    names(res) <- names(t)
    return (res)
}


.t2jdn <- function(t) 2440587.5 + t / 86400


.jdn2t <- function(d) round((d - 2440587.5) * 86400, digits = 3L)


.hours_in_day <- function(d, tz)
{
    n <- length(d)
    if (!n) return (numeric())
    d1 <- d + 1L

    missingd <- .tz_missing_days()
    if (tz %in% names(missingd)) {
        d1m <- d1 %in% missingd[[tz]]
        d1[d1m] <- d1[d1m] + 1L
    }

    return ((.d2t(d1, tz) - .d2t(d, tz)) / 3600)
}


.dhz2t <- function(d, h, z, tz, warn)
{
    # warn: 0 - no warning, 1 - warning w/o position (e.g. for use in recursive calls)
    # 2 - full warning
    # this is always invoked with args of length > 0
    # z is always of the same length as d or of 0 length
    if (length(d) < length(h)) {
        d <- rep_len(d, length(h))
    } else if (length(d) > length(h)) {
        h <- rep_len(h, length(d))
    }

    # fast exit
    if ((tz %in% c(.tz_utc(), .tz_fixed())) && !length(z)) {
        res <- d * 86400
        if (tz %in% .tz_fixed()) res <- res - .tz_fixed_offset(tz)
        res <- res + h
        names(res) <- names(d)
        return (round(res, 6L))
    }

    # we have UTC offsets
    if (length(z)) {
        res <- double(length(d))
        names(res) <- names(d)
        znok <- is.na(z)
        zok <- !znok
        res[zok] <- round(d[zok] * 86400 + h[zok] - z[zok], 6L)
        # recursive call
        if (any(znok)) res[znok] <- .dhz2t(d[znok], h[znok], integer(), tz,
                                           as.integer(as.logical(warn)))
        return (res)
    }

    # generic code
    d1 <- d + 1L
    missingd <- .tz_missing_days()
    if (tz %in% names(missingd)) {
        md <- d %in% missingd[[tz]]
        d[md] <- NA_integer_
        d1m <- d1 %in% missingd[[tz]]
        d1[d1m] <-  d1[d1m] + 1L
    }
    t0 <- .d2t(d, tz)
    t1 <- .d2t(d1, tz)
    res_chck <- .Call(C_dh2t_aux, t0, t1, h)
    res <- res_chck[[1L]]
    dn24h <- res_chck[[2L]]

    if (any(dn24h)) {
        dt1 <- res[dn24h]
        dt2 <- dt1 + (t1[dn24h] - t0[dn24h]) - 86400
        h1 <- .t2h(dt1, tz = tz)
        h2 <- .t2h(dt2, tz = tz)
        hh <- h[dn24h]
        ok1 <- h1 == hh
        ok2 <- h2 == hh

        if (warn) {
            ambg <- ok1 & ok2
            if (any(ambg)) {
                mes0 <- gettextf("ambiguous date-time indices")
                ambg <- which.max(ambg)
                if (warn > 1L) pos <- seq_along(d)[dn24h][ambg]
                ambg <- c(.t2char(dt1[ambg], tz, FALSE, FALSE),
                          .t2char(dt1[ambg], tz, TRUE, TRUE),
                          .t2char(dt2[ambg], tz, TRUE, TRUE))
                mes1 <- if (warn == 1L) gettextf("first occurrence: %s", ambg[1L])
                        else gettextf("first position %s: %s", pos, ambg[1L])
                mes2 <- gettextf("time zone: %s", tz)
                mes <- paste0(mes0, "; ", mes1, "; ", mes2)
                if (ambg[2L] != ambg[3L]) {
                    mes2 <- gettextf("assuming: %s", ambg[3L])
                    mes3 <- gettextf("not %s", ambg[2L])
                    mes <- paste0(mes, "\n", mes2, " (", mes3, ")")
                }
                warning(mes, call. = FALSE, domain = NA)
            }
        }
        dt1[ok2] <- dt2[ok2]
        nok <- !ok1 & !ok2
        nok <- !is.na(nok) & nok
        if (any(nok)) dt1[nok] <- NA_real_
        res[dn24h] <- dt1
    }

    names(res) <- names(d)
    return (round(res, 6L))
}


.astz <- function(t, tz0, tz1)
{
    nn <- length(t)
    if (!nn) return (t)
    rng <- .Call(C_range, t, TRUE)
    if (!is.finite(rng[1L])) return (t)

    # a bit faster for longer vectors
    if ((nn >= 1e5) && (diff(rng) / 86400 < nn) &&
        !(tz1 %in% names(.tz_missing_days()))) {
        dd <- as.integer(seq.int(floor(rng[1L] / 86400) - 1,
                                 ceiling(rng[2L] / 86400) + 2))
        dt0 <- .d2t(dd, tz0)
        ii <- .match_left(t, dt0)
        t00 <- dt0[ii]
        t01 <- dt0[ii + 1L]
        dt1 <- .d2t(dd, tz1)
        t10 <- dt1[ii]
        t11 <- dt1[ii + 1L]
        nl24 <- (t01 - t00 != 86400) | (t11 - t10 != 86400)
        nl24 <- !is.na(nl24) & nl24
        res <- t + (t10 - t00)
        dh0 <- .t2dhz(t[nl24], tz0)
        res[nl24] <- .dhz2t(dh0$d, dh0$h, integer(), tz1, 1L)
        return (res)
    }

    dh0 <- .t2dhz(t, tz0)
    return (.dhz2t(dh0$d, dh0$h, integer(), tz1, 2L))
}


.floorceil_t <- function(t, n, u, tz, fl)
{
    nms <- names(t)
    nn <- length(t)
    if (!nn) return (t)

    rng <- .Call(C_range, t, TRUE)
    if (!is.finite(rng[1L])) return (t)
    miny <- .t2y(rng[1L], tz)
    simple <- (tz %in% .tz_utc()) ||
              (u == "h") && (n == 1) && (miny >= 1988L) && !(tz %in% .tz_nfh88()) ||
              (u == "min") && (miny >= 1961L) && !(tz %in% .tz_n30m61()) ||
              (u == "s") && (miny >= 1948L) || (u == "s") && (n < 1)

    un <- c(3600, 60, 1)[match(u, c("h", "min", "s"))]
    nun <- un * n

    if (simple) {
        tt <- if (fl) .Call(C_floor_h, t, nun) else .Call(C_ceiling_h, t, nun)
        names(tt) <- nms
        return (tt)
    }

    if ((nn >= 1e5) && (diff(rng) / 86400 < nn) && !(tz %in% names(.tz_missing_days()))) {
        # a bit faster for longer vectors
        dd <- as.integer(seq.int(floor(rng[1L] / 86400) - 1,
                                 ceiling(rng[2L] / 86400) + 1))
        t01 <- .d2t(dd, tz)
        i01 <- .match_left(t, t01)
        t0 <- t01[i01]
        t1 <- t01[i01 + 1L]
    } else {
        d0 <- .t2d(t, tz)
        d1 <- d0 + 1L
        missingd <- .tz_missing_days()
        if (tz %in% names(missingd)) {
            d1m <- d1 %in% missingd[[tz]]
            d1[d1m] <- d1[d1m] + 1L
        }
        t0 <- .d2t(d0, tz)
        t1 <- .d2t(d1, tz)
    }
    dl <- t1 - t0
    dt <- t - t0
    tt <- t0 + if (fl) .Call(C_floor_h, dt, nun) else .Call(C_ceiling_h, dt, nun)

    nl24 <- dl != 86400
    nl24 <- nl24 & !is.na(nl24)
    if (any(nl24)) {
        t <- t[nl24]
        dh <- .t2dhz(t, tz)
        nh <- if (fl) .Call(C_floor_h, dh$h, nun) else .Call(C_ceiling_h, dh$h, nun)
        nt <- .dhz2t(rep(dh$d, 2L), if (fl) c(nh, pmax(nh - nun, 0)) else
                                c(nh, pmin(nh + nun, 86400)), integer(), tz, 0L)
        nt <- sort(unique(nt[!is.na(nt)]))
        ii <- if (fl) .match_left(t, nt) else .match_right(t, nt)
        tt[nl24] <- nt[ii]
    }

    names(tt) <- nms
    return (round(tt, digits = 6L))
}

.floor_t_h <- function(t, n, tz) .floorceil_t(t, n, "h", tz, TRUE)

.ceiling_t_h <- function(t, n, tz) .floorceil_t(t, n, "h", tz, FALSE)

.floor_t_min <- function(t, n, tz) .floorceil_t(t, n, "min", tz, TRUE)

.ceiling_t_min <- function(t, n, tz) .floorceil_t(t, n, "min", tz, FALSE)

.floor_t_s <- function(t, n, tz) .floorceil_t(t, n, "s", tz, TRUE)

.ceiling_t_s <- function(t, n, tz) .floorceil_t(t, n, "s", tz, FALSE)


.floorceil_h <- function(h, n, u, fl)
{
    un <- c(3600, 60, 1)[match(u, c("h", "min", "s"))]
    nun <- un * n
    hh <- if (fl) .Call(C_floor_h, h, nun) else .Call(C_ceiling_h, h, nun)
    return (round(hh, digits = 6L))
}

.floor_h_h <- function(h, n) .floorceil_h(h, n, "h", TRUE)

.ceiling_h_h <- function(h, n) .floorceil_h(h, n, "h", FALSE)

.floor_h_min <- function(h, n) .floorceil_h(h, n, "min", TRUE)

.ceiling_h_min <- function(h, n) .floorceil_h(h, n, "min", FALSE)

.floor_h_s <- function(h, n) .floorceil_h(h, n, "s", TRUE)

.ceiling_h_s <- function(h, n) .floorceil_h(h, n, "s", FALSE)


.inc_t_by_d <- function(t, n, tz)
{
    d0 <- .t2d(t, tz)
    d1 <- d0 + 1L
    dd0 <- d0 + n
    dd1 <- dd0 + 1L

    missingd <- .tz_missing_days()
    if (tz %in% names(missingd)) {
        d1m <- d1 %in% missingd[[tz]]
        d1[d1m] <- d1[d1m] + 1L
        dd0m <- dd0 %in% missingd[[tz]]
        dd0[dd0m] <- dd0[dd0m] + 1L
        dd1[dd0m] <- dd1[dd0m] + 1L
        dd1m <- dd1 %in% missingd[[tz]]
        dd1[dd1m] <- dd1[dd1m] + 1L
    }

    d1 <- .validate_d(d1)
    dd0 <- .validate_d(dd0)
    dd1 <- .validate_d(dd1)

    t0 <- .d2t(d0, tz)
    t1 <- .d2t(d1, tz)
    tt0 <- .d2t(dd0, tz)
    tt1 <- .d2t(dd1, tz)
    diff0 <- t - t0
    tt <- tt0 + diff0

    # starting and resulting days' lengths
    l0 <- t1 - t0
    l1 <- tt1 - tt0
    # starting and/or resulting days shorter or longer than 24h
    in24h <- (l0 != 86400) | (l1 != 86400)
    in24h <- in24h & !is.na(in24h)
    if (any(in24h)) {
        diff0_ <- diff0[in24h]
        t_ <- t[in24h]
        tt_ <- tt[in24h]
        diff1_ <- t1[in24h] - t_
        tt0_ <- tt0[in24h]
        tt1_ <- tt1[in24h]
        h_ <- .t2h(t_, tz = tz)
        h0 <- .t2h(tt_, tz = tz)
        h1 <- .t2h(tt1_ - diff1_, tz = tz)
        ok0 <- h0 == h_
        ok1 <- h1 == h_
        tt_[ok1] <- tt1_[ok1] - diff1_[ok1]
        nok <- !ok0 & !ok1
        tt_[nok] <- tt0_[nok] + h_[nok]
        tt[in24h] <- tt_
    }

    return (.validate_t(tt))
}


.inc_t_by_m <- function(t, n, tz)
{
    d0 <- .t2d(t, tz)
    d1 <- .inc_d_by_m(d0, n)
    n <- d1 - d0
    return (.inc_t_by_d(t, n, tz))
}

