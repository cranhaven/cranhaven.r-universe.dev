#' Rössler system
#' @description
#' Generates a 3-dimensional time series using the Rossler equations.
#' @details
#' The Rössler system is a system of ordinary differential equations defined as:
#' \deqn{\dot{x} = -(y + z)}{dx/dt = -(y + z)}
#' \deqn{\dot{y} = x+a \cdot y}{dy/dt = x + a*y}
#' \deqn{\dot{z} = b + z*(x-w)}{dz/dt = b + z*(x-w)}
#' The default selection for the system parameters (\emph{a} = 0.2, \emph{b} = 0.2, \emph{w} = 5.7) is known to
#' produce a deterministic chaotic time series. However, the values a = 0.1, b = 0.1, and c = 14 are more commonly used.
#' These Rössler equations are simpler than those Lorenz used since only one nonlinear term appears (the product xz in the third equation).
#'
#' Here, a = b = 0.1 and c changes. The bifurcation diagram reveals that low values of c are periodic,
#' but quickly become chaotic as c increases. This pattern repeats itself as c increases ---
#' there are sections of periodicity interspersed with periods of chaos,
#' and the trend is towards higher-period orbits as c increases.
#' For example, the period one orbit only appears for values of c around 4
#' and is never found again in the bifurcation diagram. The same phenomenon is seen with period three;
#' until c = 12, period three orbits can be found, but thereafter, they do not appear.
#'
#' @param start A 3-dimensional numeric vector indicating the starting point for the time series.
#' Default: c(-2, -10, 0.2).
#' @param a The \emph{a} parameter. Default: 0.2.
#' @param b The \emph{b} parameter. Default: 0.2.
#' @param w The \emph{w} parameter. Default: 5.7.
#' @param time The temporal interval at which the system will be generated.
#' Default: time=seq(0,50,by=0.01) or time = seq(0,by=0.01,length.out = 1000)
#' @param s The level of noise, default 0.
#'
#' @return A list with four vectors named \emph{time}, \emph{x}, \emph{y}
#' and \emph{z} containing the time, the x-components, the
#' y-components and the z-components of the Rössler system, respectively.
#' @export
#' @importFrom graphics plot
#' @importFrom stats plot.ts runif ts
#'
#' @note Some initial values may lead to an unstable system that will tend to infinity.
#' @references Rössler, O. E. 1976. An equation for continuous chaos. Physics Letters A, 57, 397-398.
#' @references Constantino A. Garcia (2019). nonlinearTseries: Nonlinear Time Series Analysis. R package version 0.2.7. https://CRAN.R-project.org/package=nonlinearTseries
#' @references wikipedia https://en.wikipedia.org/wiki/R%C3%B6ssler_attractor
#'
#' @examples
#' ###synthetic example - Rössler
#'
#' ts.r <- data.gen.Rossler(a = 0.1, b = 0.1, w = 8.7, start = c(-2, -10, 0.2),
#'                          time = seq(0, by=0.05, length.out = 10000))
#'
#' oldpar <- par(no.readonly = TRUE)
#' par(mfrow=c(1,1), ps=12, cex.lab=1.5)
#' plot.ts(cbind(ts.r$x,ts.r$y,ts.r$z), col=c('black','red','blue'))
#'
#' par(mfrow=c(1,2), ps=12, cex.lab=1.5)
#' plot(ts.r$x,ts.r$y, xlab='x',ylab = 'y', type = 'l')
#' plot(ts.r$x,ts.r$z, xlab='x',ylab = 'z', type = 'l')
#' par(oldpar)
#'

data.gen.Rossler <- function(a = 0.2, b = 0.2, w = 5.7, start = c(-2, -10, 0.2),
    time = seq(0, by = 0.05, length.out = 1000), s) {
    params <- c(a, b, w)
    rosslerEquations <- function(coord, t, params) {
        x <- coord[[1]]
        y <- coord[[2]]
        z <- coord[[3]]
        a <- params[[1]]
        b <- params[[2]]
        w <- params[[3]]
        c(-y - z, x + a * y, b + z * (x - w))
    }
    r <- rungeKutta(rosslerEquations, start, time, params)

    # add noise
    if (!missing(s)) {
        r[, 1] <- ts(r[, 1] + rnorm(length(time), mean = 0, sd = s))
        r[, 2] <- ts(r[, 2] + rnorm(length(time), mean = 0, sd = s))
        r[, 3] <- ts(r[, 3] + rnorm(length(time), mean = 0, sd = s))
    }

    list(time = time, x = r[, 1], y = r[, 2], z = r[, 3])
}

#' Lorenz system
#' @description
#' Generates a 3-dimensional time series using the Lorenz equations.
#' @details
#' The Lorenz system is a system of ordinary differential equations defined as:
#' \deqn{\dot{x} = \sigma(y-x)}{dx/dt = sigma*( y - x )}
#' \deqn{\dot{y} = \rho x-y-xz}{dy/dt = rho*x - y - xz}
#' \deqn{\dot{z} = -\beta z + xy}{dz/dt = -beta*z + xy}
#' The default selection for the system parameters (\eqn{\sigma=10, \rho=28, \beta=8/3}{sigma=10, rho=28, beta=8/3}) is known to
#' produce a deterministic chaotic time series.
#'
#' @param start A 3-dimensional numeric vector indicating the starting point for the time series.
#' Default: c(-13, -14, 47).
#' @param sigma The \eqn{\sigma}{sigma} parameter. Default: 10.
#' @param rho The \eqn{\rho}{rho} parameter. Default: 28.
#' @param beta The \eqn{\beta}{beta} parameter. Default: 8/3.
#' @param time The temporal interval at which the system will be generated.
#' Default: time=seq(0,50,by = 0.01).
#' @param s The level of noise, default 0.
#'
#' @note Some initial values may lead to an unstable system that will tend to infinity.
#' @references Constantino A. Garcia (2019). nonlinearTseries: Nonlinear Time Series Analysis. R package version 0.2.7. https://CRAN.R-project.org/package=nonlinearTseries
#'
#' @return A list with four vectors named \emph{time}, \emph{x}, \emph{y}
#' and \emph{z} containing the time, the x-components, the
#' y-components and the z-components of the Lorenz system, respectively.
#' @export
#'
#' @examples
#' ###Synthetic example - Lorenz
#' ts.l <- data.gen.Lorenz(sigma = 10, beta = 8/3, rho = 28, start = c(-13, -14, 47),
#'                         time = seq(0, by=0.05, length.out = 2000))
#'
#' ts.plot(cbind(ts.l$x,ts.l$y,ts.l$z), col=c('black','red','blue'))

data.gen.Lorenz <- function(sigma = 10, beta = 8/3, rho = 28, start = c(-13, -14,
    47), time = seq(0, 50, length.out = 1000), s) {
    params <- c(sigma, beta, rho)
    lorenzEquations <- function(coord, t, params) {
        x <- coord[[1]]
        y <- coord[[2]]
        z <- coord[[3]]
        sigma <- params[[1]]
        beta <- params[[2]]
        rho <- params[[3]]
        c(sigma * (y - x), rho * x - y - x * z, x * y - beta * z)
    }
    l <- rungeKutta(lorenzEquations, start, time, params)

    # add noise
    if (!missing(s)) {
        l[, 1] <- ts(l[, 1] + rnorm(length(time), mean = 0, sd = s))
        l[, 2] <- ts(l[, 2] + rnorm(length(time), mean = 0, sd = s))
        l[, 3] <- ts(l[, 3] + rnorm(length(time), mean = 0, sd = s))
    }

    list(time = time, x = l[, 1], y = l[, 2], z = l[, 3])
}

#' Duffing map
#' @description
#' Generates a 2-dimensional time series using the Duffing map.
#' @details
#' The Duffing map is defined as follows:
#' \deqn{ x_n = y_{n - 1}}{x[n] = y[n - 1]}
#' \deqn{ y_n = -b \cdot x_{n - 1} + a \cdot y_{n - 1} - y_{n - 1}^3}{y[n] = -b*x[n - 1] + a*y[n-1] - y[n-1]^3.}
#' The default selection for both \emph{a} and \emph{b} parameters
#' (\emph{a}=1.4 and \emph{b}=0.3) is known to produce a deterministic chaotic
#' time series.
#'
#' @param nobs Length of the generated time series. Default: 5000 samples.
#' @param a The \emph{a} parameter. Default: 2.75.
#' @param b The \emph{b} parameter. Default: 0.2.
#' @param start A 2-dimensional vector indicating the starting values for the x and y Duffing coordinates.
#' Default: If the starting point is not specified, it is generated randomly.
#' @param s The level of noise, default 0.
#' @param do.plot Logical value. If TRUE (default value), a plot of the generated Duffing system is shown.
#'
#' @note Some initial values may lead to an unstable system that will tend to infinity.
#' @references Constantino A. Garcia (2019). nonlinearTseries: Nonlinear Time Series Analysis. R package version 0.2.7. https://CRAN.R-project.org/package=nonlinearTseries
#'
#' @return A list with two vectors named \emph{x} and \emph{y} containing the
#' x-components and the y-components of the Duffing map, respectively.
#' @export
#'
#' @examples
#' Duffing.map=data.gen.Duffing(nobs = 1000, do.plot=TRUE)

data.gen.Duffing <- function(nobs = 5000, a = 2.75, b = 0.2, start = runif(n = 2,
    min = -0.5, max = 0.5), s, do.plot = TRUE) {
    nwarm <- 500
    n <- nobs + nwarm
    y <- x <- vector(mode = "numeric", length = n)
    x[[1]] <- start[[1]]
    y[[1]] <- start[[2]]

    for (i in 2:n) {
        x[[i]] <- y[[i - 1]]
        y[[i]] <- -b * x[[i - 1]] + a * y[[i - 1]] - y[[i - 1]]^3
    }

    # add noise
    if (!missing(s)) {
        x <- x + rnorm(n, mean = 0, sd = s)
        y <- y + rnorm(n, mean = 0, sd = s)
    }

    x <- x[(nwarm + 1):n]
    y <- y[(nwarm + 1):n]

    # plotting
    if (do.plot) {
        title <- paste("Duffing map\n", "a = ", a, " b = ", b)
        plot(x, y, xlab = "x[n]", ylab = "y[n]", main = title, type = "p")
    }
    list(x = x, y = y)
}

#' Henon map
#' @description
#' Generates a 2-dimensional time series using the Henon map.
#' @details
#' The Henon map is defined as follows:
#' \deqn{ x_n = 1 - a \cdot x_{n - 1}^2 + y_{n - 1}}{x[n] = 1 - a*x[n - 1]^2 + y[n - 1]}
#' \deqn{ y_n = b \cdot x_{n - 1}}{y[n] = b*x[n - 1].}
#' The default selection for both \emph{a} and \emph{b} parameters
#' (\emph{a}=1.4 and \emph{b}=0.3) is known to produce a deterministic chaotic
#' time series.
#'
#' @param nobs Length of the generated time series. Default: 5000 samples.
#' @param a The \emph{a} parameter. Default: 1.4.
#' @param b The \emph{b} parameter. Default: 0.3.
#' @param start A 2-dimensional vector indicating the starting values for the x and y Henon coordinates.
#' Default: If the starting point is not specified, it is generated randomly.
#' @param s The level of noise, default 0.
#' @param do.plot Logical value. If TRUE (default value), a plot of the generated Henon system is shown.
#'
#' @note Some initial values may lead to an unstable system that will tend to infinity.
#' @references Constantino A. Garcia (2019). nonlinearTseries: Nonlinear Time Series Analysis. R package version 0.2.7. https://CRAN.R-project.org/package=nonlinearTseries
#'
#' @return A list with two vectors named \emph{x} and \emph{y} containing the
#' x-components and the y-components of the Henon map, respectively.
#' @export
#'
#' @examples
#' Henon.map=data.gen.Henon(nobs = 1000, do.plot=TRUE)

data.gen.Henon <- function(nobs = 5000, a = 1.4, b = 0.3, start = runif(n = 2, min = -0.5,
    max = 0.5), s, do.plot = TRUE) {
    nwarm <- 500
    n <- nobs + nwarm
    y <- x <- vector(mode = "numeric", length = n)
    x[[1]] <- start[[1]]
    y[[1]] <- start[[2]]

    for (i in 2:n) {
        x[[i]] <- y[[i - 1]] + 1 - a * x[[i - 1]]^2
        y[[i]] <- b * x[[i - 1]]
    }

    # add noise
    if (!missing(s)) {
        x <- x + rnorm(n, mean = 0, sd = s)
        y <- y + rnorm(n, mean = 0, sd = s)
    }

    x <- x[(nwarm + 1):n]
    y <- y[(nwarm + 1):n]

    # plotting
    if (do.plot) {
        title <- paste("Henon map\n", "a = ", a, " b = ", b)
        plot(x, y, xlab = "x[n]", ylab = "y[n]", main = title, type = "p")
    }
    list(x = x, y = y)
}

#' Logistic map
#' @description
#' Generates a time series using the logistic map.
#' @details
#' The logistic map is defined as follows:
#' \deqn{x_n = r  \cdot  x_{n-1}   \cdot  (1 - x_{n-1})}{x[n] = r * x[n-1]  * (1 - x[n-1])}
#'
#' @param nobs  Length of the generated time series. Default: 5000 samples.
#' @param r     The \emph{r} parameter. Default: 4
#' @param start A numeric value indicating the starting value for the time series.
#' If the starting point is not specified, it is generated randomly.
#' @param s The level of noise, default 0.
#' @param do.plot Logical value. If TRUE (default value), a plot of the generated Logistic system is shown.
#'
#' @return A vector of time series.
#' @export
#'
#' @references Constantino A. Garcia (2019). nonlinearTseries: Nonlinear Time Series Analysis. R package version 0.2.7. https://CRAN.R-project.org/package=nonlinearTseries
#'
#' @examples
#' Logistic.map=data.gen.Logistic(nobs = 1000, do.plot=TRUE)

data.gen.Logistic <- function(nobs = 5000, r = 4, start = runif(n = 1, min = 0, max = 1),
    s, do.plot = TRUE) {
    nwarm <- 500
    n <- nobs + nwarm
    x <- vector(mode = "numeric", length = n)
    x[[1]] <- start

    for (i in 2:n) {
        x[[i]] <- r * x[[i - 1]] * (1 - x[[i - 1]])
    }

    # add noise
    if (!missing(s)) {
        x <- x + rnorm(n, mean = 0, sd = s)
    }

    x <- x[(nwarm + 1):n]

    # plotting
    if (do.plot) {
        title <- paste("Logistic map\n", "r = ", r)
        # plot(1:nobs, x, xlab = 'n', ylab = 'x[n]', main = title, type = 'l')
        plot(x[-length(x)], x[-1], xlab = "x[t]", ylab = "x[t+1]", main = title,
            type = "p")
    }

    list(x = x)
}

# Runge-Kutta method for solving differential equations. It is used to generate
# both Lorenz and Rossler systems.
rungeKutta <- function(func, initial.condition, time, params) {
    n.samples <- length(time)
    h <- time[[2]] - time[[1]]
    y <- matrix(ncol = length(initial.condition), nrow = n.samples)
    y[1, ] <- initial.condition
    for (i in 2:n.samples) {
        k1 <- h * func(y[i - 1, ], time[[i - 1]], params)
        k2 <- h * func(y[i - 1, ] + k1/2, time[[i - 1]] + h/2, params)
        k3 <- h * func(y[i - 1, ] + k2/2, time[[i - 1]] + h/2, params)
        k4 <- h * func(y[i - 1, ] + k3, time[[i - 1]] + h, params)

        y[i, ] <- y[i - 1, ] + (k1 + 2 * k2 + 2 * k3 + k4)/6
    }
    y
}
