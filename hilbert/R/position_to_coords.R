#' @title
#' Convert Grid Positions to Coordinates
#' @param x
#' One of: Integer vector, `data.frame`, or `matrix`.
#' If a numeric vector, then it corresponds to Row positions.
#' @param y
#' Integer vector corresponding to Column positions.
#' @param ... Unused.
#' @param extent
#' Named vector with names `xmax`, `xmin`, `ymax`, `ymin`.
#' Corresponds to the bounding box of the given coordinates.
#' If `extent` is `NULL`, then the function will throw an exception.
#' @param n
#' Exponent to the dimensions of the underlying grid. The Hilbert
#' Curve indices are based on a `2^n x 2^n` grid. This number
#' must be less than 15 due to the 32-bit implementation of R.
#' @param coords
#' Column names or indices of a `data.frame`/`matrix` that
#' contain the positions.
#' @param attach
#' If `TRUE`, adds the coordinates as new columns to the given
#' `data.frame`/`matrix`. This will *replace* the position columns.
#' @return A `data.frame` containing the coordinates as `numeric`
#'         columns `x` and `y`, or the original object
#'         (`data.frame` or `matrix`) with the positions
#'         replaced with the coordinates.
#' @rdname position_to_coords
#' @export
position_to_coords <- function(x, ..., n = 10L, extent = NULL) {
    if (is.null(extent)) {
        stop("`extent` is required.")
    }

    if (n < 16L) {
        UseMethod("position_to_coords")
    } else {
        if (!requireNamespace("bit64", quietly = TRUE)) {
            stop("`bit64` is required to use exponents greater than 15.")
        }

        UseMethod("position_to_coords64")
    }
}

#' @rdname position_to_coords
#' @export
position_to_coords.data.frame <- function(
    x, ..., n, extent, coords = c(1, 2), attach = TRUE
) {
    .Class <- class(x[[coords[1]]])
    coordinates <- NextMethod(
        "position_to_coords",
        x = x[[coords[1]]],
        y = x[[coords[2]]],
        ...,
        n = n,
        extent = extent
    )

    if (attach) {
        x[[coords[1]]] <- coordinates$x
        x[[coords[2]]] <- coordinates$y
        return(x)
    }

    coordinates
}

#' @rdname position_to_coords
#' @export
position_to_coords.matrix <- function(
    x, ..., n, extent, coords = c(1, 2), attach = TRUE
) {
    .Class <- class(x[, coords[1]])
    coordinates <- NextMethod(
        "position_to_coords",
        x = x[, coords[1]],
        y = x[, coords[2]],
        ...,
        n = n,
        extent = extent
    )

    if (attach) {
        x[, coords[1]] <- coordinates$x
        x[, coords[2]] <- coordinates$y
        return(x)
    }

    coordinates
}

#' @rdname position_to_coords
#' @export
position_to_coords.numeric <- function(x, y, ..., n, extent) {
    .Class <- "integer"
    NextMethod(
        "position_to_coords",
        x = as.integer(x),
        y = as.integer(y),
        ...,
        n = n,
        extent = extent
    )
}

#' @rdname position_to_coords
#' @export
position_to_coords.double <- function(x, y, ..., n, extent) {
    .Class <- "integer"
    NextMethod(
        "position_to_coords",
        x = as.integer(x),
        y = as.integer(y),
        ...,
        n = n,
        extent = extent
    )
}

#' @rdname position_to_coords
#' @export
position_to_coords.integer <- function(x, y, ..., n, extent) {
    HILBERT_xy_to_coords_(n, x, y, extent)
}

#' @rdname position_to_coords
#' @export
position_to_coords64 <- function(x, ..., n = 10L, extent = NULL) {
    UseMethod("position_to_coords64")
}

#' @rdname position_to_coords
#' @export
position_to_coords64.data.frame <- function(
    x, ..., n, extent, coords = c(1, 2), attach = TRUE
) {
    .Class <- class(x[[coords[1]]])
    coordinates <- NextMethod(
        "position_to_coords64",
        x = x[[coords[1]]],
        y = x[[coords[2]]],
        ...,
        n = n,
        extent = extent
    )

    if (attach) {
        x[[coords[1]]] <- coordinates$x
        x[[coords[2]]] <- coordinates$y
        return(x)
    }

    coordinates
}

#' @rdname position_to_coords
#' @export
position_to_coords64.matrix <- function(
    x, ..., n, extent, coords = c(1, 2), attach = TRUE
) {
    .Class <- class(x[, coords[1]])
    coordinates <- NextMethod(
        "position_to_coords64",
        x = x[, coords[1]],
        y = x[, coords[2]],
        ...,
        n = n,
        extent = extent
    )

    if (attach) {
        x[, coords[1]] <- coordinates$x
        x[, coords[2]] <- coordinates$y
        return(x)
    }

    coordinates
}

#' @rdname position_to_coords
#' @export
position_to_coords64.numeric <- function(x, y, ..., n, extent) {
    .Class <- "integer64"
    NextMethod(
        "position_to_coords",
        x = bit64::as.integer64(x),
        y = bit64::as.integer64(y),
        ...,
        n = n,
        extent = extent
    )
}

#' @rdname position_to_coords
#' @export
position_to_coords64.double <- function(x, y, ..., n, extent) {
    .Class <- "integer64"
    NextMethod(
        "position_to_coords64",
        x = bit64::as.integer64(x),
        y = bit64::as.integer64(y),
        ...,
        n = n,
        extent = extent
    )
}

#' @rdname position_to_coords
#' @export
position_to_coords64.integer64 <- function(x, y, ..., n, extent) {
    .Class <- "bitstring"
    NextMethod(
        "position_to_coords64",
        x = bit64::as.bitstring(x),
        y = bit64::as.bitstring(y),
        ...,
        n = n,
        extent = extent
    )
}

#' @rdname position_to_coords
#' @export
position_to_coords64.bitstring <- function(x, y, ..., n, extent) {
    HILBERT_xy_to_coords_64_(n, x, y, extent)
}