#' @title
#' Get index positions from a Hilbert Curve
#' @param h
#' One of: Integer vector, `data.frame`, or `matrix`.
#' @param ... Unused.
#' @param n
#' Exponent to the dimensions of the underlying grid. The Hilbert
#' Curve indices are based on a `2^n x 2^n` grid. This number
#' must be less than 15 due to the 32-bit implementation of R. This *must*
#' be the same as the `n` used in `index`.
#' @param idx
#' Column name or index containing the Hilbert Curve indices.
#' @param attach
#' If `TRUE`, adds the position as new columns to the given
#' `data.frame`/`matrix`. If `h` is a `data.frame`, then the
#' columns are named `x` and `y`; otherwise, it is two unnamed columns at
#' the end of the matrix.
#' @return A `data.frame` containing the positions as `integer`
#'         columns `x` and `y`, or the original object
#'         (`data.frame` or `matrix`) with the columns attached.
#'         When `n` is greater than 15, the positions are of
#'         type `bit64::integer64`.
#' @rdname position
#' @export
position <- function(h, ..., n = 10L) {
    if (n < 16L) {
        UseMethod("position")
    } else {
        if (!requireNamespace("bit64", quietly = TRUE)) {
            stop("`bit64` is required to use exponents greater than 15.")
        }

        UseMethod("position64")
    }
}

#' @rdname position
#' @export
position.data.frame <- function(h, ..., n, idx = 1, attach = TRUE) {
    .Class    <- class(h[[idx[1]]])
    positions <- NextMethod("position", h = h[[idx[1]]], ..., n = n)

    if (attach) {
        h[["x"]] <- positions[[1]]
        h[["y"]] <- positions[[2]]
        return(h)
    }

    positions
}

#' @rdname position
#' @export
position.matrix <- function(h, ..., n, idx = 1, attach = TRUE) {
    .Class    <- class(h[, idx[1]])
    positions <- NextMethod("position", h = h[, idx[1]], ..., n = n)

    if (attach) {
        h[, ncol(h) + 1] <- positions[[1]]
        h[, ncol(h) + 1] <- positions[[2]]
        return(h)
    }

    positions
}

#' @rdname position
#' @export
position.numeric <- function(h, ..., n) {
    .Class <- "integer"
    NextMethod("position", h = as.integer(h), ..., n = n)
}


#' @rdname position
#' @export
position.integer <- function(h, ..., n) {
    HILBERT_position_(n, h)
}

#' @rdname position
#' @export
position64 <- function(h, ..., n = 10L) {
    UseMethod("position64")
}

#' @rdname position
#' @export
position64.data.frame <- function(h, ..., n, idx = 1, attach = TRUE) {
    .Class    <- class(h[[idx[1]]])
    positions <- NextMethod("position64", h = h[[idx[1]]], ..., n = n)

    if (attach) {
        h[["x"]] <- positions[[1]]
        h[["y"]] <- positions[[2]]
        return(h)
    }

    positions
}

#' @rdname position
#' @export
position64.matrix <- function(h, ..., n, idx = 1, attach = TRUE) {
    .Class    <- class(h[, idx[1]])
    positions <- NextMethod("position64", h = h[[idx[1]]], ..., n = n)

    if (attach) {
        h[, ncol(h) + 1] <- positions[[1]]
        h[, ncol(h) + 1] <- positions[[2]]
        return(h)
    }

    positions
}

#' @rdname position
#' @export
position64.double <- function(h, ..., n) {
    h      <- bit64::as.integer64(h)
    .Class <- "integer64"
    NextMethod("position64", h = h, ..., n = n)
}

#' @rdname position
#' @export
position64.integer <- function(h, ..., n) {
    h      <- bit64::as.integer64(h)
    .Class <- "integer64"
    NextMethod("position64", h = h, ..., n = n)
}

#' @rdname position
#' @export
position64.numeric <- function(h, ..., n) {
    h      <- bit64::as.integer64(h)
    .Class <- "integer64"
    NextMethod("position64", h = h, ..., n = n)
}

#' @rdname position
#' @export
position64.integer64 <- function(h, ..., n) {
    h      <- bit64::as.bitstring(h)
    .Class <- "bitstring"
    NextMethod("position64", h = h, ..., n = n)
}

#' @rdname position
#' @export
position64.character <- function(h, ..., n) {
    .Class <- class(h) <- "bitstring"
    NextMethod("position64", h = h, ..., n = n)
}

#' @rdname position
#' @export
position64.bitstring <- function(h, ..., n) {
    pos      <- HILBERT_position64_(n, h)
    pos[[1]] <- bit64::as.integer64(pos[[1]])
    pos[[2]] <- bit64::as.integer64(pos[[2]])
    pos
}