#' Gaussian Blobs
#' @details
#' This function generates a matrix of features creating multiclass datasets
#' by allocating each class one or more normally-distributed clusters of points.
#' It can control both centers and standard deviations of each cluster.
#' For example, we want to generate a dataset of weight and height (two features) of 500 people (data length),
#' including three groups, baby, children, and adult. Centers are the average weight and height for each group,
#' assuming both weight and height are normally distributed (i.e. follow Gaussian distribution).
#' The standard deviation (sd) is the sd of the Gaussian distribution
#' while the bounding box (bbox) is the range for each generated cluster center when only the number of centers is given.
#'
#' @param nobs The data length to be generated.
#' @param features Features of dataset.
#' @param centers  Either the number of centers, or a matrix of the chosen centers.
#' @param sd   The level of Gaussian noise, default 1.
#' @param bbox The bounding box of the dataset.
#' @param do.plot  Logical value. If TRUE (default value), a plot of the generated Blobs is shown.
#'
#' @return A list of two variables, x and classes.
#' @export
#'
#' @references Amos Elberg (2018). clusteringdatasets: Datasets useful for testing clustering algorithms. R package version 0.1.1. https://github.com/elbamos/clusteringdatasets
#'
#' @examples
#' Blobs=data.gen.blobs(nobs=1000, features=2, centers=3, sd=1, bbox=c(-10,10), do.plot=TRUE)

data.gen.blobs <- function(nobs = 100, features = 2, centers = 3, sd = 1, bbox = c(-10,
    10), do.plot = TRUE) {
    if (is.matrix(centers)) {
        if (ncol(centers) != features)
            stop("Dimensionality of centers must equal number of features.")
    } else {
        centers <- runif(n = features * centers, min = bbox[1], max = bbox[2])
        centers <- matrix(centers, ncol = features)
    }

    if (length(sd) != 1 & length(sd) != nrow(centers))
        stop("sd must be 1 or the same length as the number of clusters")

    categories <- sample(nrow(centers), size = nobs, replace = TRUE)

    starting_points <- matrix(rnorm(n = nobs * features), ncol = features)

    if (length(sd) == 1)
        x <- starting_points * sd else x <- starting_points * sd[categories]

    x <- x + centers[categories, ]

    # plotting
    if (do.plot) {
        title <- paste("Gaussian Blobs")
        plot(x, xlab = NA, ylab = NA, main = title, col = categories, type = "p")
    }

    list(x = x, classes = factor(categories))
}

#' Circles
#'
#' @param n The data length to be generated.
#' @param r_vec The radius of circles.
#' @param start The center of circles.
#' @param s The level of Gaussian noise, default 0.
#' @param do.plot Logical value. If TRUE (default value), a plot of the generated Circles is shown.
#'
#' @return A list of two variables, x and classes.
#' @export
#'
#' @examples
#' Circles=data.gen.circles(n = 1000, r_vec=c(1,2), start=runif(1,-1,1), s=0.1, do.plot=TRUE)

data.gen.circles <- function(n, r_vec = c(1, 2), start = runif(1, -1, 1), s, do.plot = TRUE) {

    points <- NULL
    for (r in r_vec) {
        tmp <- cbind(x = r * cos(seq(start, start + 2 * pi, length.out = n)), y = r *
            sin(seq(start, start + 2 * pi, length.out = n)))

        points <- rbind(points, tmp)
    }

    categories <- rep(seq_along(r_vec), each = n)

    # add noise
    if (!missing(s)) {
        points <- vapply(seq_len(ncol(points)), function(i) points[, i] + rnorm(n,
            mean = 0, sd = s), FUN.VALUE = numeric(n * length(r_vec)))
    }

    # plotting
    if (do.plot) {
        title <- paste("Circles")
        plot(points, xlab = NA, ylab = NA, main = title, col = categories, type = "p")
    }

    list(x = points, classes = factor(categories))
}


#' Spirals
#'
#' @param n The data length to be generated.
#' @param cycles  The number of cycles of spirals.
#' @param s The level of Gaussian noise, default 0.
#' @param do.plot Logical value. If TRUE (default value), a plot of the generated Spirals is shown.
#'
#' @return A list of two variables, x and classes.
#' @export
#'
#' @references Friedrich Leisch & Evgenia Dimitriadou (2010). mlbench: Machine Learning Benchmark Problems. R package version 2.1-1.
#'
#' @examples
#' Spirals=data.gen.spirals(n = 2000, cycles=2, s=0.01, do.plot=TRUE)

data.gen.spirals <- function(n, cycles = 1, s = 0, do.plot = TRUE) {
    x <- matrix(0, nrow = n, ncol = 2)
    c2 <- sample(1:n, size = n/2, replace = FALSE)
    cl <- factor(rep(1, length = n), levels = as.character(1:2))
    cl[c2] <- 2

    x[-c2, ] <- data.gen.1spiral(n = n - length(c2), cycles = cycles, sd = s)$x
    x[c2, ] <- -data.gen.1spiral(n = length(c2), cycles = cycles, sd = s)$x

    # plotting
    if (do.plot) {
        title <- paste("Spirals")
        plot(x, xlab = NA, ylab = NA, main = title, col = cl, type = "p")
    }

    list(x = x, classes = cl)
}

data.gen.1spiral <- function(n, cycles = 1, sd = 0) {

    w <- seq(0, by = cycles/n, length = n)
    x <- matrix(0, nrow = n, ncol = 2)

    x[, 1] <- (2 * w + 1) * cos(2 * pi * w)/3
    x[, 2] <- (2 * w + 1) * sin(2 * pi * w)/3

    if (sd > 0) {
        eps <- rnorm(n, sd = sd)

        xs <- cos(2 * pi * w) - pi * (2 * w + 1) * sin(2 * pi * w)
        ys <- sin(2 * pi * w) + pi * (2 * w + 1) * cos(2 * pi * w)

        nrm <- sqrt(xs^2 + ys^2)
        x[, 1] <- x[, 1] + eps * ys/nrm
        x[, 2] <- x[, 2] - eps * xs/nrm
    }

    list(x = x)
}
