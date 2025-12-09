### * All functions in this file are exported

### * available_priors()

#' List the available priors for model parameters
#'
#' @return A tibble containing information about the available priors.
#'
#' @examples
#' available_priors()
#'
#' @export

available_priors <- function() {
    fun_names <- c("constant_p",
                   "uniform_p",
                   "normal_p",
                   "hcauchy_p",
                   "exponential_p",
                   "gamma_p",
                   "scaled_beta_p")
    prior_names <- c("Constant value",
                     "Uniform prior",
                     "Normal distribution",
                     "Half-Cauchy distribution",
                     "Exponential distribution",
                     "Gamma distribution",
                     "Scaled beta distribution")
    usage <- c("constant_p(value)",
               "uniform_p(min, max)",
               "normal_p(mean, sd)",
               "hcauchy_p(scale)",
               "exponential_p(lambda)",
               "gamma_p(alpha, beta)",
               "scaled_beta_p(alpha, beta, scale = 1)")
    out <- tibble::tibble(function_name = fun_names,
                          description = prior_names,
                          usage = usage)
    return(structure(out, class = c("prior_tibble", class(out))))
}

### * Methods for nice display of prior tibble

### ** format.prior_tibble()

#' Pretty formatting of a \code{prior_tibble} object
#'
#' @param x An object of class \code{prior_tibble}.
#' @param ... Not used.
#'
#' @return A character string for pretty printing of a prior tibble.
#' 
#' @export

format.prior_tibble <- function(x, ...) {
    x <- structure(x, class = class(x)[class(x) != "prior_tibble"])
    f <- format(x)
    f <- c(f,
           "-------------------------------------------------------",
           "(Note: All priors distributions are truncated at zero.)")
    return(f)
}

### ** print.prior_tibble()

#' Pretty printing of a \code{prior_tibble} object
#'
#' @param x An object of class \code{prior_tibble}.
#' @param ... Not used.
#'
#' @return Mostly called for its side effect of printing, but also returns its
#'     input invisibly.
#' 
#' @export

print.prior_tibble <- function(x, ...) {
    cat(format(x), sep = "\n")
    invisible(x)
}

### * constant_p(value)

#' Define a fixed-value prior
#'
#' This is equivalent to having a fixed parameter.
#'
#' @param value The constant value of the parameter.
#'
#' @return A list defining the prior.
#'
#' @examples
#' constant_p(2)
#'
#' @export

constant_p <- function(value) {
    x <- list(type = "constant",
              parameters = c(value = value))
    x <- structure(x, class = "prior")
    return(x)
} 

### * hcauchy_p(scale)

#' Define a half-Cauchy prior (on [0;+Inf])
#'
#' @param scale Median of the half-Cauchy distribution.
#'
#' @return A list defining the prior.
#'
#' @importFrom stats rcauchy
#'
#' @examples
#' hcauchy_p(scale = 0.5)
#'
#' @export

hcauchy_p <- function(scale) {
    x <- list(type = "hcauchy",
              parameters = c(scale = scale))
    x <- structure(x, class = "prior")
    return(x)
}

### * normal_p(mean, sd)

#' Define a truncated normal prior (on [0;+Inf])
#'
#' @param mean Mean of the untruncated normal.
#' @param sd Standard deviation of the untruncated normal.
#'
#' @return A list defining the prior.
#'
#' @importFrom stats rnorm
#'
#' @examples
#' normal_p(mean = 0, sd = 4)
#'
#' @export

normal_p <- function(mean, sd) {
    x <- list(type = "trun_normal",
              parameters = c(mean = mean,
                             sd = sd))
    x <- structure(x, class = "prior")
    return(x)
}

### * uniform_p(min, max)

#' Define a uniform prior
#'
#' @param min,max Minimum and maximum boundaries for the uniform prior.
#'
#' @return A list defining the prior.
#'
#' @importFrom stats runif
#' 
#' @examples
#' uniform_p(min = 0, max= 1)
#'
#' @export

uniform_p <- function(min, max) {
    x <- list(type = "uniform",
              parameters = c(min = min, max = max))
    x <- structure(x, class = "prior")
    return(x)
}


### * scaled_beta_p(alpha, beta, scale=1)

#' Define a beta prior (on [0;scale])
#'
#' If a random variable X follows a scaled beta distribution with parameters
#' (alpha, beta, scale), then X/scale follows a beta distribution with
#' parameters (alpha, beta).
#' 
#' @param alpha Alpha parameter of the unscaled beta distribution.
#' @param beta Beta parameter of the unscaled beta distribution.
#' @param scale The upper boundary of the prior.
#'
#' @return A list defining the prior.
#'
#' @examples
#' scaled_beta_p(0.8, 20, scale = 10)
#'
#' @export

scaled_beta_p <- function(alpha, beta, scale = 1) {
    x <- list(type = "scaled_beta",
              parameters = c(alpha = alpha, beta = beta, scale = scale))
    x <- structure(x, class = "prior")
    return(x)
}

### * exponential_p(lambda)

#' Define an exponential prior
#'
#' @param lambda Lambda parameter (rate) of the exponential distribution. The
#'     mean of the exponential distribution is 1/lambda.
#'
#' @return A list defining the prior.
#'
#' @examples
#' exponential_p(0.5)
#'
#' @export

exponential_p <- function(lambda) {
    x <- list(type = "exponential",
              parameters = c(lambda = lambda))
    x <- structure(x, class = "prior")
    return(x)
}

### * gamma_p(alpha, beta)

#' Define a gamma prior
#'
#' Note the name of the function to define a prior (\code{gamma_p}), in order
#' to avoid confusion with the R mathematical function \code{gamma}.
#'
#' @param alpha Shape parameter (equivalent to the \code{shape} parameter of
#'     R's \code{rgamma}).
#' @param beta Rate parameter (equivalent to the \code{rate} parameter of R's
#'     \code{rgamma}).
#'
#' @return A list defining the prior.
#'
#' @examples
#' gamma_p(9, 2)
#' hist(sample_from_prior(gamma_p(9, 2), 1e3))
#' 
#' @export

gamma_p <- function(alpha, beta) {
    x <- list(type = "gamma",
              parameters = c(alpha = alpha, beta = beta))
    x <- structure(x, class = "prior")
    return(x)
}

### * Methods for nice display of priors

### ** format.prior()

#' Pretty formatting of a \code{prior} object
#'
#' @param x An object of class \code{prior}.
#' @param ... Not used.
#'
#' @return A character string for pretty printing of a prior.
#' 
#' @export

format.prior <- function(x, ...) {
    params <- paste0("(",
                     paste(paste(names(x[["parameters"]]), x[["parameters"]],
                                 sep = "="), collapse = ","),
                     ")")
    out <- paste0(x[["type"]], "", params)
    return(out)
}

### ** print.prior()

#' Pretty printing of a \code{prior} object
#'
#' @param x An object of class \code{prior}.
#' @param ... Not used.
#'
#' @return Mostly called for its side effect of printing, but also returns its
#'     input invisibly.
#' 
#' @export

print.prior <- function(x, ...) {
    cat(format(x), sep = "\n")
    invisible(x)
}

### * Extending tibbles

# https://cran.r-project.org/web/packages/tibble/vignettes/extending.html

#' Function used for displaying \code{prior} object in tibbles
#'
#' @param x An object of class \code{prior}.
#'
#' @return Input formatted with \code{format(x)}.
#' 
#' @importFrom pillar type_sum
#' @export
type_sum.prior <- function(x) {
    format(x)
}

#' Function used for displaying \code{prior} object in tibbles
#'
#' @param x An object of class \code{prior}.
#' 
#' @return Input formatted with \code{format(x)}.
#' 
#' @importFrom pillar obj_sum
#' @export
obj_sum.prior <- function(x) {
    format(x)
}

#' Function used for displaying \code{prior} object in tibbles
#'
#' @param x An object of class \code{prior}.
#' @param ... Not used.
#'
#' @return An object prepared with pillar::new_pillar_shaft_simple.
#' 
#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.prior <- function(x, ...) {
    out <- format(x)
    out[is.na(x)] <- NA
    pillar::new_pillar_shaft_simple(out, align = "right")
}

### * Methods for Ops on priors (implementing '==' operator)

# https://stackoverflow.com/a/35902710

#' Implementation of the '==' operator for priors
#'
#' @param e1,e2 Objects of class "prior".
#'
#' @return Boolean (or throws an error for unsupported operators).
#' 
#' @examples
#' p <- constant_p(0)
#' q <- constant_p(4)
#' p == q
#'
#' p <- hcauchy_p(2)
#' q <- hcauchy_p(2)
#' p == q
#'
#' @method Ops prior
#' 
#' @export

Ops.prior <- function(e1, e2) {
    op <- .Generic[[1]]
    switch(op,
           `==` = {
               if (e1$type != e2$type) {
                   return(FALSE)
               }
               if (!all(e1$parameters == e2$parameters)) {
                   return(FALSE)
               }
               return(TRUE)
           },
           stop("Undefined operation for objects of class \"priors\".")
           )
}

### * sample_from_prior()

#' Sample from a prior object
#'
#' @param x A \code{prior} object.
#' @param n Integer, number of samples to draw.
#'
#' @return A numeric vector of length \code{n}.
#' 
#' @examples
#' sample_from_prior(constant_p(1))
#' sample_from_prior(constant_p(1), 10)
#' sample_from_prior(hcauchy_p(0.5), 1)
#' hist(sample_from_prior(hcauchy_p(0.5), 20))
#' hist(sample_from_prior(uniform_p(0, 3), 1000))
#' hist(sample_from_prior(scaled_beta_p(3, 7, 2), 1e4))
#' 
#' @export
#'

sample_from_prior <- function(x, n = 1) {
    switch(x$type,
           "constant" = {
               rep(x$parameters[["value"]], n)
           },
           "hcauchy" = {
               scale <- x$parameters[["scale"]]
               replicate(n,
               {
                   o <- stats::rcauchy(1, location = 0, scale = scale)
                   while (o < 0) {
                       o <- stats::rcauchy(1, location = 0, scale = scale)
                   }
                   return(o)
               })
           },
           "trun_normal" = {
               mean <- x$parameters[["mean"]]
               sd <- x$parameters[["sd"]]
               replicate(n, {
                   o <- stats::rnorm(1, mean = mean, sd = sd)
                   while (o < 0) {
                       o <- stats::rnorm(1, mean = mean, sd = sd)
                   }
                   return(o)
               })
           },
           "uniform" = {
               stats::runif(n, min = x$parameters[["min"]], max = x$parameters[["max"]])
           },
           "scaled_beta" = {
               p <- x$parameters
               p[["scale"]] * stats::rbeta(n, shape1 = p[["alpha"]], shape2 = p[["beta"]])
           },
           "exponential" = {
               lambda <- x$parameters[["lambda"]]
               stats::rexp(n = n, rate = lambda)
           },
           "gamma" = {
               alpha <- x$parameters[["alpha"]]
               beta <- x$parameters[["beta"]]
               stats::rgamma(n = n, shape = alpha, rate = beta)
           },
           stop("Unknown prior type."))
}
