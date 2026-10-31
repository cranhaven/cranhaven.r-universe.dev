#' Generate Synthetic Data with Various Structures
#' 
#' Generates either:
#' - Structured (x, y) scatter data (linear, sine, circle, etc.), or
#' - A matrix of scaled orthogonal polynomial features.
#'
#' @param type Character string. Options:
#'   - `"polynomial"` for orthogonal polynomial features
#'   - `"linear"`, `"sine"`, `"circle"`, `"cluster"`, `"snake"`, `"outliers"`,
#'     `"sparse"`, `"clumpy"`, `"skewed"`, `"striated"`, `"concave"`, `"monotonic"`, `"doughnut"`,
#'     or `"all"` to generate all scatter structures.
#' @param n Integer. Number of samples to generate. Default is 500.
#' @param degree Integer. Degree of polynomial features (only for `type = "polynomial"`).
#' @param seed Optional integer. Sets random seed for reproducibility.
#' 
#' @return 
#' - If `type = "polynomial"`, returns a matrix (`n` x `degree`).
#' - Otherwise a tibble with columns:
#' - `x`: Numeric vector of x-values
#' - `y`: Numeric vector of y-values
#' - `structure`: Character name of the structure type
#'
#' @examples
#' data_gen("linear", n = 200)
#' data_gen("polynomial", degree = 4, n = 200)
#' data_gen("all", n = 200)
#'
#' @export
data_gen <- function(type = "all", n = 500, degree = NULL, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  structures <- c(
    "linear", "sine", "circle", "cluster", "snake",
    "outliers", "sparse", "clumpy", "skewed", "striated",
    "concave", "monotonic", "doughnut"
  )
  
  if (type == "polynomial") {
    stopifnot(!is.null(degree), is.numeric(degree), degree > 0, is.numeric(n), n > 1)
    x <- runif(n, 0, 1)
    return(sqrt(n - 1) * poly(x, degree))
  }
  
  if (type == "all") {
    return(dplyr::bind_rows(lapply(structures, function(t) data_gen(t, n, seed = NULL))))
  }
  
  if (!(type %in% structures)) stop("Unknown structure type.")
  
  switch(type,
         "linear" = {
           x <- runif(n, 0, 1)
           y <- 2 * x + rnorm(n, 0, 0.01)
         },
         "sine" = {
           x <- seq(0, 2 * pi, length.out = n)
           y <- sin(x) + rnorm(n, 0, 0.01)
         },
         "circle" = {
           angle <- runif(n, 0, 2 * pi)
           x <- cos(angle)
           y <- sin(angle)
         },
         "cluster" = {
           blob_n <- round(n / 3)
           x <- c(rnorm(blob_n, -2, 0.3), rnorm(blob_n, 0, 0.3), rnorm(blob_n, 2, 0.3))
           y <- c(rnorm(blob_n, -2, 0.3), rnorm(blob_n, 2, 0.3), rnorm(blob_n, -1, 0.3))
         },
         "snake" = {
           x <- seq(0, 10, length.out = n)
           y <- sin(x) + 0.2 * sin(5 * x) + rnorm(n, 0, 0.05)
         },
         "outliers" = {
           x <- c(rnorm(n - 5, 0, 1), runif(5, 10, 15))
           y <- c(rnorm(n - 5, 0, 1), runif(5, 10, 15))
         },
         "sparse" = {
           x <- runif(n, 0, 100)
           y <- runif(n, 0, 100)
         },
         "clumpy" = {
           blob_n <- round(n / 3)
           x <- c(rnorm(blob_n, 2, 0.1), rnorm(blob_n, 5, 0.1), rnorm(blob_n, 8, 0.1))
           y <- c(rnorm(blob_n, 2, 0.1), rnorm(blob_n, 5, 0.1), rnorm(blob_n, 8, 0.1))
         },
         "skewed" = {
           x <- rexp(n, rate = 1)
           y <- rexp(n, rate = 1)
         },
         "striated" = {
           x <- rep(seq(0, 1, length.out = 10), each = round(n / 10))
           y <- runif(length(x), 0, 1)
         },
         "concave" = {
           angle <- runif(n, 0, pi)
           x <- cos(angle)
           y <- sin(angle)
         },
         "monotonic" = {
           x <- seq(0, 10, length.out = n)
           y <- x^2 + rnorm(n, 0, 1)
         },
         "doughnut" = {
           angle <- runif(n, 0, 2 * pi)
           r <- sqrt(runif(n, 0.6^2, 1^2))
           x <- r * cos(angle)
           y <- r * sin(angle)
         }
  )
  
  x <- as.numeric(scale(x))
  y <- as.numeric(scale(y))
  
  tibble::tibble(x = x, y = y, structure = type)
}




#' Generate Synthetic Noise
#'
#'
#' @param n Integer. Number samples to generate. Default is 500.
#' @param type Character string specifying the type of noise to generate. Supported types:
#'   - `"gaussian"`: Standard normal distribution.
#'   - `"uniform"`: Uniform distribution between -level and +level.
#'   - `"lognormal"`: Log-normal distribution.
#'   - `"t_distributed"`: Heavy-tailed t-distribution with 3 degrees of freedom.
#'   - `"cauchy"`: Extremely heavy-tailed Cauchy distribution.
#'   - `"beta_noise"`: Beta distribution shifted and scaled to `[-level, level]`.
#'   - `"exponential"`: Positive-only exponential distribution.
#'   - `"microstructure"`: Oscillatory sinusoidal pattern with additive Gaussian noise.
#'   
#' @param level Numeric. Controls the scale (standard deviation, range, or spread) of the noise. Default is `0.01`.
#' @param seed Optional integer. Sets a random seed for reproducibility.
#'
#'
#' @return A tibble with two columns:
#'   - `value`: Numeric vector of generated noise samples.
#'   - `type`: Character string indicating the type of noise.
#'
#' @examples
#' # Gaussian noise with small scale
#' noise_gen(500, type = "gaussian", level = 0.05)
#' 
#' # Heavy-tailed noise
#' noise_gen(500, type = "t_distributed", level = 0.1)
#' 
#' @importFrom stats poly rbeta rcauchy rexp rlnorm rnorm rt runif
#' @export
noise_gen <- function(n = 500, type = "gaussian", level = 0.01, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  noise <- numeric(n)
  
  noise <- switch(type,
                  
                  "gaussian" = rnorm(n, mean = 0, sd = level),
                  
                  "uniform" = runif(n, min = -level, max = level),
                  
                  "lognormal" = rlnorm(n, meanlog = 0, sdlog = level),
                  
                  "t_distributed" = rt(n, df = 3) * level,
                  
                  "cauchy" = rcauchy(n) * level,
                  
                  "beta_noise" = (rbeta(n, 2, 5) - 0.5) * 2 * level,
                  
                  "exponential" = rexp(n, rate = 1/level),
                  
                  
                  "microstructure" = {
                    sin(seq(0, 20 * pi, length.out = n)) * level + rnorm(n, 0, level / 2)
                  },
                  
                  stop("Unknown noise type.")
  )
  
  return(tibble::tibble(value = noise, type = type))
}

