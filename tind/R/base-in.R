#
# This file is a part of tind.
#
# Copyright (c) Grzegorz Klima 2025
#
# ##################################### #
# arbitrary integer and numeric indices #
# ##################################### #



# integer indices
# ###################################################################

.validate_i <- function(i) suppressWarnings(as.integer(i))

.i2char <- function(i)
{
    n <- length(i)
    if (!n) return (character())
    res <- rep(NA_character_, n)
    names(res) <- names(i)

    if (anyna <- anyNA(i)) {
        nna <- !is.na(i)
        if (!any(nna)) return (res)
        ii <- i[nna]
    } else ii <- i

    res0 <- format(ii)

    if (anyna) res[nna] <- res0 else res[] <- res0
    return (res)
}

.i2n <- function(i) as.double(i)

.floor_i <- function(i, u) ((i %/% u) * u)

.ceiling_i <- function(i, u) ((i %/% u + (i %% u != 0L)) * u)



# numeric indices
# ###################################################################

.validate_n <- function(n)
{
    n <- suppressWarnings(as.double(n))
    n[!is.finite(n)] <- NA_real_
    return (n)
}

.n2char <- function(n)
{
    N <- length(n)
    if (!N) return (character())
    res <- rep(NA_character_, N)
    names(res) <- names(n)

    if (anyna <- anyNA(n)) {
        nna <- !is.na(n)
        if (!any(nna)) return (res)
        nn <- n[nna]
    } else nn <- n

    if (all(nn == 0)) {
        res0 <- rep("0", length(nn))
    } else {
        mina <- min(abs(nn[nn != 0]))
        maxa <- max(abs(nn))
        res0 <- if ((mina < 1e-3) || (maxa >= 1e4)) format(nn, digits = 16)
                else format(nn, scientific = FALSE, digits = 16)
    }

    if (anyna) res[nna] <- res0 else res[] <- res0
    return (res)
}

.floor_n <- function(n, u) (floor(n / u) * u)

.ceiling_n <- function(n, u) (ceiling(n / u) * u)

