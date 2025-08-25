#' Parameter with uniform distribution for hyperparameter optimization
#' @export
#' @param name Name of the parameter, must match the input to `eval_func`.
#' @param values Values, discrete numeric
#' @examples
#' p1 <- par_discretenum('x1', 0:2)
#' class(p1)
#' print(p1)
par_discretenum <- function(name, values) {
  R6_par_discretenum$new(
    name=name,
    values=values
  )
}

#' R6 object for discrete numeric
#'
#' Parameter with uniform distribution for hyperparameter optimization
#' @export
#' @field name Name of the parameter, must match the input to `eval_func`.
#' @field values Values, discrete numeric
#' @field ggtrans Transformation for ggplot, see ggplot2::scale_x_continuous()
#' @examples
#' p1 <- R6_par_discretenum$new('x1', 0:2)
#' class(p1)
#' print(p1)
R6_par_discretenum <- R6::R6Class(
  # R6_par_discretenum ----
  classname="par_discretenum",
  inherit = R6_par_hype,
  public=list(
    name=NULL,
    values=NULL,
    #' @description Function to convert from raw scale to transformed scale
    #' @param x Value of raw scale
    fromraw=function(x) {x}, #identity,
    #' @description Function to convert from transformed scale to raw scale
    #' @param x Value of transformed scale
    toraw= function(x) {x}, #identity,
    #' @description Generate values in the raw space based on quantiles.
    #' @param q In [0,1].
    # generate = function(q) {
    #   self$toraw(self$fromraw(self$lower) + q * (self$fromraw(self$upper) - self$fromraw(self$lower)))
    # },
    generate = function(q) {
      stopifnot(is.numeric(q), q>=0, q<=1)
      inds <- 1 + floor(q*(1-1e-12)*length(self$values))
      stopifnot(inds>=1, inds <= length(self$values))
      self$values[inds]
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
        sapply(x, function(x) {any(abs(x - self$values) < 1e-15)})
    },
    #' @description Convert this to a parameter for the
    #' mixopt R package.
    #' @param raw_scale Should it be on the raw scale?
    convert_to_mopar = function(raw_scale=FALSE) {
      mixopt::mopar_ordered(values=self$values)
    },
    ggtrans="identity", # ggplot trans to give to scale_x_continuous
    # fromraw=NULL,
    # toraw= NULL,
    # ggtrans=NULL, # ggplot trans to give to scale_x_continuous
    #' @description Create a hyperparameter with uniform distribution
    #' @param name Name of the parameter, must match the input to `eval_func`.
    #' @param values Numeric values, must be in ascending order
    initialize = function(name, values) {
      self$name <- name
      stopifnot(is.numeric(values))
      stopifnot(diff(values) > 0)
      self$values <- values
    },
    #' @description Print details of the object.
    #' @param ... not used
    print = function(...) {
      s <- paste0("hype par_discretenum(name = ", self$name,
                  ", values = ", paste(self$values, collapse=" "),
                  ")")
      cat(s)
      invisible(self)
    }
  )
)
if (F) {
  p1 <- par_discretenum('x1', c(1:5,100))
  p1
  class(p1)
  p1$generate(runif(22))
  table(p1$generate(runif(1e5)))
}
