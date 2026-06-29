#' Generates distance matrices
#' Distance matrices are generated between the cells of multiple animals
#' (Caenorhabditis elegans)  from time-series matrices.
#' @param data Time-series matrices
#' @param distance "mSBD" or "SBD" or "Euclid" can be specified. mSBD means
#' modified Shape-based distance.
#' @return A list containing distance matrices
#' @examples
#' # Toy data
#' n_cell_x <- 13
#' n_cell_y <- 24
#' n_cell_z <- 29
#' n_cells <- 30
#' n_time_frames <- 100
#'
#' # 13 cells, 100 time frames
#' animal_x <- matrix(runif(n_cell_x * n_time_frames),
#'     nrow = n_cell_x, ncol = n_time_frames
#' )
#' rownames(animal_x) <- sample(seq(n_cells), n_cell_x)
#' colnames(animal_x) <- seq(n_time_frames)
#'
#' # 24 cells, 100 time frames
#' animal_y <- matrix(runif(n_cell_y * n_time_frames),
#'     nrow = n_cell_y, ncol = n_time_frames
#' )
#' rownames(animal_y) <- sample(seq(n_cells), n_cell_y)
#' colnames(animal_y) <- seq(n_time_frames)
#'
#' # 29 cells, 100 time frames
#' animal_z <- matrix(runif(n_cell_z * n_time_frames),
#'     nrow = n_cell_z, ncol = n_time_frames
#' )
#' rownames(animal_z) <- sample(seq(n_cells), n_cell_z)
#' colnames(animal_z) <- seq(n_time_frames)
#'
#' # Positive Control of Difference between SBD and mSBD
#' animal_z[2, ] <- -animal_x[1, ]
#' X <- list(
#'     animal_x = animal_x,
#'     animal_y = animal_y,
#'     animal_z = animal_z
#' )
#' Ds_mSBD <- worm_distance(X, "mSBD")
#' @importFrom dtwclust SBD
#' @importFrom dtwclust NCCc
#' @importFrom dtwclust zscore
#' @importFrom stats as.dist
#' @importFrom utils combn
#' @export
worm_distance <- function(data, distance = c("mSBD", "SBD", "Euclid")) {
    # Argument Check
    .check_worm_distance(data)
    distance <- match.arg(distance)
    # Calculate Distance
    Ds <- lapply(data, .distFunc[[distance]])
    # Output
    Ds
}

.check_worm_distance <- function(data) {
    stopifnot(is.list(data))
    if (length(data) <= 2) {
        stop("The number of matrices are too small!")
    }
    data |>
        lapply(is.numeric) |>
        unlist() |>
        all() |>
        stopifnot()
}

.SBDMatrix <- function(X) {
    indices <- t(combn(seq(nrow(X)), 2))
    indices <- indices[, 2:1]
    distances <- apply(indices, 1, function(xx) {
        SBD(X[xx[1], ], X[xx[2], ])$dist
    })
    out <- matrix(0, nrow = nrow(X), ncol = nrow(X))
    out[indices] <- distances
    out <- as.dist(out)
    attr(out, "Labels") <- rownames(X)
    out
}

.mSBDMatrix <- function(X) {
    indices <- t(combn(seq(nrow(X)), 2))
    indices <- indices[, 2:1]
    distances <- apply(indices, 1, function(xx) {
        .mSBD(X[xx[1], ], X[xx[2], ])$dist
    })
    out <- matrix(0, nrow = nrow(X), ncol = nrow(X))
    out[indices] <- distances
    out <- as.dist(out)
    attr(out, "Labels") <- rownames(X)
    out
}

.mSBD <- function(x, y, znorm = FALSE, error.check = TRUE,
                  return.shifted = TRUE) {
    nx <- length(x)
    ny <- length(y)
    if (nx > ny) {
        swap <- x
        x <- y
        y <- swap
    } else {
        swap <- NULL
    }
    if (znorm) {
        CCseq <- NCCc(zscore(x, error.check = FALSE),
            zscore(y, error.check = FALSE),
            error.check = FALSE
        )
    } else {
        CCseq <- NCCc(x, y, error.check = FALSE)
    }
    m <- max(abs(CCseq))
    if (!return.shifted) {
        return(1 - m) # nocov
    }
    shift <- which.max(abs(CCseq)) - max(nx, ny)
    if (is.null(swap)) {
        if (shift < 0L) {
            yshift <- y[(-shift + 1L):ny]
        } else {
            yshift <- c(rep(0, shift), y)
        }
    } else {
        if (shift < 0L) {
            yshift <- c(rep(0, -shift), x)
        } else {
            yshift <- x[(shift + 1L):ny]
        }
    }
    nys <- length(yshift)
    if (nys < nx) {
        yshift <- c(yshift, rep(0, nx - nys))
    } else {
        yshift <- yshift[1L:nx]
    }
    # return
    list(dist = 1 - m, yshift = yshift)
}

.distFunc <- list(
    "Euclid" = dist,
    "SBD" = .SBDMatrix,
    "mSBD" = .mSBDMatrix
)
