#' GMMPP: Approximation for MAP
#'
#' A point process dominated by a continuous-time Markov chain.
#' 

GMMPPClass <- R6::R6Class(
  "GMMPPClass",
  inherit = MAPClass,
  public = list(
    #' @description
    #' Create a MAP
    #' @param alpha A vector of initial probability
    #' @param D0 An infinitesimal generator
    #' @param D1 An infinitesimal generator
    #' @param xi An exit rate vector
    #' @return An instance of MAP
    initialize = function(alpha, D0, D1, xi) {
      if (!all(diag(D1) == apply(D1, 1, sum))) {
        stop("D1 is not a diagonal matrix")
      }
      super$initialize(alpha, D0, D1, xi)
    },
    
    #' @description 
    #' copy
    #' @return A new instance
    copy = function() {
      gmmpp(alpha=as.vector(self$alpha()),
            D0=as.matrix(self$D0()),
            D1=as.matrix(self$D1()))
    },
    
    #' @description 
    #' Run EM
    #' @param data A dataframe
    #' @param options A list of options
    #' @param ... Others
    emfit = function(data, options, ...) {
      con <- list(...)
      nmsC <- names(options)
      options[(namc <- names(con))] <- con
      
      alpha <- super$alpha()
      D0 <- super$D0()
      D1 <- super$D1()
      xi <- super$xi()
      P0 <- super$make.matrixD0()
      P1 <- super$make.matrixD1()
      en0 <- super$make.matrixD0()
      en1 <- super$make.matrixD1()
      G <- super$make.matrixD1()
      Psi1T <- super$make.matrixD1()
      Psi2T <- super$make.matrixD1()
      Psi1N <- super$make.matrixD1()
      Psi2N <- super$make.matrixD1()
      tmpm <- super$make.matrixD1()

      switch(class(data),
             "map.group" = emfit_gmmpp_group(alpha, xi, D0, D1, data, options,
                                             P0, P1, en0, en1, G, Psi1T, Psi2T, Psi1N, Psi2N, tmpm)
      )
    }
  )
)

#' Create GMMPP
#'
#' Create an instance of GMMPP
#'
#' @param size An integer for the number of phases
#' @param alpha A vector of initial probability
#' @param D0 An infinitesimal generator without arrivals
#' @param D1 An infinitesimal generator with arrivals
#' @return An instance of GMMPP
#'
#' @note
#' This function can omit several patterns of arguments. For example, `map(5)`
#' omit the arguments `alpha`, `Q` and `xi`. In this case, the default values are
#' assigned to them.
#'
#' @examples
#' ## create a map (full matrix) with 5 phases
#' (param1 <- gmmpp(5))
#'
#' ## create a map with specific parameters
#' (param2 <- gmmpp(alpha=c(1,0,0),
#'               D0=rbind(c(-4,2,0),c(2,-5,1),c(1,0,-1)),
#'               D1=rbind(c(2,0,0),c(0,2,0),c(0,0,0))))
#'
#' @export

gmmpp <- function(size, alpha, D0, D1) {
  if (missing(size)) {
    if (missing(alpha) || missing(D0) || missing(D1)) {
      stop("alpha, D0 and D1 are needed.")
    } else {
      size <- length(alpha)
    }
  } else {
    if (!missing(alpha) && !missing(D0) && !missing(D1)) {
      # warning("size is ignored.")
      size <- length(alpha)
    } else {
      if (missing(alpha)) {
        # warning("alpha is set by default")
        alpha <- rep(1.0/size, size)
      }
      if (missing(D1)) {
        # warning("D1 is set by default")
        D1 <- diag(1.0, size, size)
      }
      if (missing(D0)) {
        # warning("D0 is set by default")
        D0 <- matrix(1.0, size, size)
      }
    }
  }
  GMMPPClass$new(alpha=alpha, D0=D0, D1=D1, xi=rep(1, size))
}

