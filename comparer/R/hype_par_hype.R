#' Parameter for hyperparameter optimization
#' @field partrans The transformation type.
#' @export
#' @examples
#' p1 <- R6_par_hype$new()
#' class(p1)
#' print(p1)
# R6_par_hype ----
R6_par_hype <- R6::R6Class(
  classname="par_hype",
  public=list(
    partrans="",
    #' @description Get a sequence, uniform on the transformed scale
    #' @param n Number of points. Ignored for discrete.
    getseq = function(n) {
      # Sequence equally spaced on transformed scale
      xtrans <- seq(self$fromraw(self$lower), self$fromraw(self$upper), l=n)
      xraw <- self$toraw(xtrans)
      list(
        trans=xtrans,
        raw=xraw
      )
    }
  )
)
