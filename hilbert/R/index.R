#' @title
#' Index positions to a Hilbert Curve
#' @param x
#' One of: Numeric vector, `data.frame`, or `matrix`.
#' If a numeric vector, then it corresponds to the rows
#' of a position.
#' @param ... Unused.
#' @param n
#' Exponent to the dimensions of the underlying grid. The Hilbert
#' Curve indices are based on a `2^n x 2^n` grid. This number
#' must be less than 15 due to the 32-bit implementation of R.
#' @param y
#' Numeric vector.
#' Corresponds to the columns of a position.
#' @param coords
#' Column names or indices of a `data.frame`/`matrix` that
#' contain the position coordinates.
#' @param attach
#' If `TRUE`, adds the indices as a new column to the given
#' `data.frame`/`matrix`. If `x` is a `data.frame`, then the
#' column is named `h`; otherwise, it is an unnamed column at
#' the end of the matrix.
#' @return An `integer` vector of Hilbert indices, or when `attach` is `TRUE`,
#'         the original object (`data.frame` or `matrix`) with a new `integer`
#'         column (`h` for `data.frame`) containing the Hilbert indices. When
#'         `n` is greater than 15, the vector is of type `bit64::integer64`.
#' @rdname index
#' @export
index <- function(x, ..., n = 10L) {
    if (n < 16L) {
        UseMethod("index")
    } else {
        if (!requireNamespace("bit64", quietly = TRUE)) {
            stop("`bit64` is required to use exponents greater than 15.")
        }

        UseMethod("index64")
    }
}

#' @rdname index
#' @export
index.data.frame <- function(x, ..., n, coords = c(1, 2), attach = TRUE) {
    xx      <- x[[coords[1]]]
    yy      <- x[[coords[2]]]
    .Class  <- class(xx)
    indices <- NextMethod("index", x = xx, y = yy, ..., n = n)

    if (attach) {
        x[["h"]] <- indices
        return(x)
    }

    indices
}

#' @rdname index
#' @export
index.matrix <- function(x, ..., n, coords = c(1, 2), attach = TRUE) {
    xx      <- x[, coords[1]]
    yy      <- x[, coords[2]]
    .Class  <- class(xx)
    indices <- NextMethod("index", x = xx, y = yy, ..., n = n)

    if (attach) {
        x[, ncol(x) + 1] <- indices
        return(x)
    }

    indices
}

#' @rdname index
#' @export
index.double <- function(x, y, ..., n) {
    .Class <- "integer"
    NextMethod("index", x = as.integer(x), y = as.integer(y), ..., n = n)
}

#' @rdname index
#' @export
index.numeric <- function(x, y, ..., n) {
    .Class <- "integer"
    NextMethod("index", x = as.integer(x), y = as.integer(y), ..., n = n)
}

#' @rdname index
#' @export
index.integer <- function(x, y, ..., n) {
    HILBERT_index_(n, x, y)
}

#' @rdname index
#' @export
index64 <- function(x, ..., n = 10L) {
    UseMethod("index64")
}

#' @rdname index
#' @export
index64.data.frame <- function(x, ..., n, coords = c(1, 2), attach = TRUE) {
    xx      <- x[[coords[1]]]
    yy      <- x[[coords[2]]]
    .Class  <- class(xx)
    indices <- NextMethod("index64", x = xx, y = yy, ..., n = n)

    if (attach) {
        x[["h"]] <- indices
        return(x)
    }

    indices
}

#' @rdname index
#' @export
index64.matrix <- function(x, ..., n, coords = c(1, 2), attach = TRUE) {
    xx      <- x[[coords[1]]]
    yy      <- x[[coords[2]]]
    .Class  <- class(xx)
    indices <- NextMethod("index64", x = xx, y = yy, ..., n = n)

    if (attach) {
        x[[ncol(x) + 1]] <- indices
        return(x)
    }

    indices
}

#' @rdname index
#' @export
index64.double <- function(x, y, ..., n) {
    x      <- bit64::as.integer64(x)
    y      <- bit64::as.integer64(y)
    .Class <- "integer64"
    NextMethod("index64", x = x, y = y, ..., n = n)
}

#' @rdname index
#' @export
index64.integer <- function(x, y, ..., n) {
    x      <- bit64::as.integer64(x)
    y      <- bit64::as.integer64(y)
    .Class <- "integer64"
    NextMethod("index64", x = x, y = y, ..., n = n)
}

#' @rdname index
#' @export
index64.numeric <- function(x, y, ..., n) {
    x      <- bit64::as.integer64(x)
    y      <- bit64::as.integer64(y)
    .Class <- "integer64"
    NextMethod("index64", x = x, y = y, ..., n = n)
}

#' @rdname index
#' @export
index64.integer64 <- function(x, y, ..., n) {
    x      <- bit64::as.bitstring(x)
    y      <- bit64::as.bitstring(y)
    .Class <- "bitstring"
    NextMethod("index64", x = x, y = y, ..., n = n)
}

#' @rdname index
#' @export
index64.character <- function(x, y, ..., n) {
    .Class <- class(x) <- class(y) <- "bitstring"
    NextMethod("index64", x = x, y = y, n = n)
}

#' @rdname index
#' @export
index64.bitstring <- function(x, y, ..., n) {
    bit <- HILBERT_index64_(n, x, y)
    bit64::as.integer64(bit)
}