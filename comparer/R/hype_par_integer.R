#' Parameter with uniform distribution over integer range
#' for hyperparameter optimization
#' @export
#' @param name Name of the parameter, must match the input to `eval_func`.
#' @param lower Lower bound of the parameter
#' @param upper Upper bound of the parameter
#' @examples
#' p1 <- par_integer('x1', 3, 8)
#' class(p1)
#' print(p1)
#' table(p1$generate(runif(1000)))
par_integer <- function(name, lower, upper) {
  R6_par_integer$new(
    name=name,
    lower=lower,
    upper=upper
  )
}

#' Parameter with uniform distribution over integer range
#' for hyperparameter optimization
#' @export
#' @field name Name of the parameter, must match the input to `eval_func`.
#' @field lower Lower bound of the parameter
#' @field upper Upper bound of the parameter
# @field fromraw Function to convert from raw scale to transformed scale
# @field toraw Function to convert from transformed scale to raw scale
#' @field ggtrans Transformation for ggplot, see ggplot2::scale_x_continuous()
#' @importFrom R6 R6Class
#' @examples
#' p1 <- R6_par_integer$new('x1', 0, 2)
#' class(p1)
#' print(p1)
R6_par_integer <- R6::R6Class(
  # R6_par_integer ----
  classname="par_integer",
  inherit = R6_par_hype,
  public=list(
    name=NULL,
    lower=NULL,
    upper=NULL,
    #' @description Function to convert from raw scale to transformed scale
    #' @param x Value of raw scale
    fromraw=function(x) {x}, #identity,
    #' @description Function to convert from transformed scale to raw scale
    #' @param x Value of transformed scale
    toraw= function(x) {x}, #identity,
    #' @description Generate values in the raw space based on quantiles.
    #' @param q In [0,1].
    generate = function(q) {
      stopifnot(is.numeric(q), q>=0, q<=1)
      # Use the 1e-16 to avoid 1 mapping above upper
      self$lower + floor(q*(1-1e-16) * (self$upper + 1 - self$lower))
    },
    #' @description Get a sequence, uniform on the transformed scale
    #' @param n Number of points. Ignored for discrete.
    getseq = function(n) {
      s <- unique(self$generate(seq(0,1,l=n)))
      list(
        trans=self$fromraw(s),
        raw=s
      )
    },
    #' @description Check if input is valid for parameter
    #' @param x Parameter value
    isvalid = function(x) {
      is.numeric(x) &
        (abs(x - round(x)) < 1e-8) &
        (x >= self$lower) &
        (x <= self$upper)
    },
    #' @description Convert this to a parameter for the
    #' mixopt R package.
    #' @param raw_scale Should it be on the raw scale?
    convert_to_mopar = function(raw_scale=FALSE) {
      mixopt::mopar_ordered(values=self$lower:self$upper)
    },
    ggtrans="identity", # ggplot trans to give to scale_x_continuous
    #' @description Create a hyperparameter with uniform distribution
    #' @param name Name of the parameter, must match the input to `eval_func`.
    #' @param lower Lower bound of the parameter
    #' @param upper Upper bound of the parameter
    initialize = function(name, lower, upper) {
      self$name <- name
      stopifnot(is.numeric(lower), length(lower) == 1)
      stopifnot(is.numeric(upper), length(upper) == 1)
      lowerint <- as.integer(lower)
      upperint <- as.integer(upper)
      stopifnot(abs(lower - lowerint) < 1e-8)
      stopifnot(abs(upper - upperint) < 1e-8)
      stopifnot(lower < upper)
      self$lower <- lowerint
      self$upper <- upperint
    },
    #' @description Print details of the object.
    #' @param ... not used,
    print = function(...) {
      s <- paste0("hype par_integer(name = ", self$name,
                  ", lower = ", self$lower,
                  ", upper = ", self$upper, ")")
      cat(s)
      invisible(self)
    }
  )
)
if (F) {
  p1 <- par_integer('x1', 4, 9)
  print(p1)
  class(p1)
  p1$generate(runif(22))
  table(p1$generate(runif(6e5)))
}
