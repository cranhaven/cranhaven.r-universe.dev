#' @title
#' Convert Coordinates to Grid Positions
#' @param x
#' One of: Numeric vector, `data.frame`, or `matrix`.
#' If a numeric vector, then it corresponds to X coordinates.
#' @param y
#' Numeric vector corresponding to Y coordinates.
#' @param ... Unused.
#' @param extent
#' Named vector with names `xmax`, `xmin`, `ymax`, `ymin`.
#' Corresponds to the bounding box of the given coordinates.
#' If `extent` is `NULL`, then the bounding box is found from the
#' given coordinates.
#' @param n
#' Exponent to the dimensions of the underlying grid. The Hilbert
#' Curve indices are based on a `2^n x 2^n` grid. This number
#' must be less than 15 due to the 32-bit implementation of R.
#' @param coords
#' Column names or indices of a `data.frame`/`matrix` that
#' contain the coordinates.
#' @param attach
#' If `TRUE`, adds the position as new columns to the given
#' `data.frame`/`matrix`. This will *replace* the coordinate columns.
#' @return A `data.frame` containing the positions as `integer`
#'         columns `x` and `y`, or the original object
#'         (`data.frame` or `matrix`) with the coordinates
#'         replaced with the grid positions. When `n` is greater than 15,
#'         the positions are of type `bit64::integer64`.
#' @rdname coords_to_position
#' @export
coords_to_position <- function(x, ..., n = 10L, extent = NULL) {
    if (n < 16L) {
        UseMethod("coords_to_position")
    } else {
        if (!requireNamespace("bit64", quietly = TRUE)) {
            stop("`bit64` is required to use exponents greater than 15.")
        }

        UseMethod("coords_to_position64")
    }
}

#' @rdname coords_to_position
#' @export
coords_to_position.data.frame <- function(
    x, ..., n, extent, coords = c(1, 2), attach = TRUE
) {
    if (is.null(extent)) extent <- .extent(x[[coords[1]]], x[[coords[2]]])

    .Class <- class(x[[coords[1]]])
    positions <- NextMethod(
        "coords_to_position",
        x = x[[coords[1]]],
        y = x[[coords[2]]],
        ...,
        n = n,
        extent = extent
    )

    if (attach) {
        x[[coords[1]]] <- positions[[1]]
        x[[coords[2]]] <- positions[[2]]
        return(x)
    }

    positions
}

#' @rdname coords_to_position
#' @export
coords_to_position.matrix <- function(
    x, ..., n, extent, coords = c(1, 2), attach = TRUE
) {
    if (is.null(extent)) extent <- .extent(x[, coords[1]], x[, coords[2]])

    .Class <- class(x[, coords[1]])
    positions <- NextMethod(
        "coords_to_position",
        x = x[, coords[1]],
        y = x[, coords[2]],
        ...,
        n = n,
        extent = extent
    )

    if (attach) {
        x[, coords[1]] <- positions[, 1]
        x[, coords[2]] <- positions[, 2]
        return(x)
    }

    positions
}

#' @rdname coords_to_position
#' @export
coords_to_position.numeric <- function(x, y, ..., n, extent) {
    if (is.null(extent)) extent <- .extent(x, y)
    HILBERT_coords_to_xy_(n, x, y, extent)
}

#' @rdname coords_to_position
#' @export
coords_to_position.double <- function(x, y, ..., n, extent) {
    if (is.null(extent)) extent <- .extent(x, y)
    HILBERT_coords_to_xy_(n, x, y, extent)
}

#' @rdname coords_to_position
#' @export
coords_to_position.integer <- function(x, y, ..., n, extent) {
    if (is.null(extent)) extent <- .extent(x, y)
    HILBERT_coords_to_xy_(n, x, y, extent)
}

#' @rdname coords_to_position
#' @export
coords_to_position64 <- function(x, ..., n = 10L, extent = NULL) {
    UseMethod("coords_to_position64")
}

#' @rdname coords_to_position
#' @export
coords_to_position64.data.frame <- function(
    x, ..., n, extent, coords = c(1, 2), attach = TRUE
) {
    if (is.null(extent)) extent <- .extent(x[[coords[1]]], x[[coords[2]]])

    .Class <- class(x[[coords[1]]])
    positions <- NextMethod(
        "coords_to_position64",
        x = x[[coords[1]]],
        y = x[[coords[2]]],
        ...,
        n = n,
        extent = extent
    )

    if (attach) {
        x[[coords[1]]] <- positions[[1]]
        x[[coords[2]]] <- positions[[2]]
        return(x)
    }

    positions
}

#' @rdname coords_to_position
#' @export
coords_to_position64.matrix <- function(
    x, ..., n, extent, coords = c(1, 2), attach = TRUE
) {
    if (is.null(extent)) extent <- .extent(x[, coords[1]], x[, coords[2]])

    .Class <- class(x[, coords[1]])
    positions <- NextMethod(
        "coords_to_position64",
        x = x[, coords[1]],
        y = x[, coords[2]],
        ...,
        n = n,
        extent = extent
    )

    if (attach) {
        x[, coords[1]] <- positions[, 1]
        x[, coords[2]] <- positions[, 2]
        return(x)
    }

    positions
}

#' @rdname coords_to_position
#' @export
coords_to_position64.numeric <- function(x, y, ..., n, extent) {
    if (is.null(extent)) extent <- .extent(x, y)
    pos      <- HILBERT_coords_to_xy_64_(n, x, y, extent)
    pos[[1]] <- bit64::as.integer64(pos[[1]])
    pos[[2]] <- bit64::as.integer64(pos[[2]])
    pos
}

#' @rdname coords_to_position
#' @export
coords_to_position64.double <- function(x, y, ..., n, extent) {
    if (is.null(extent)) extent <- .extent(x, y)
    pos      <- HILBERT_coords_to_xy_64_(n, x, y, extent)
    pos[[1]] <- bit64::as.integer64(pos[[1]])
    pos[[2]] <- bit64::as.integer64(pos[[2]])
    pos
}

#' @rdname coords_to_position
#' @export
coords_to_position64.integer <- function(x, y, ..., n, extent) {
    if (is.null(extent)) extent <- .extent(x, y)
    pos      <- HILBERT_coords_to_xy_64_(n, x, y, extent)
    pos[[1]] <- bit64::as.integer64(pos[[1]])
    pos[[2]] <- bit64::as.integer64(pos[[2]])
    pos
}