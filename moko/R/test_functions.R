#' Test function: The Nowacki Beam
#'
#' This function is a variation of the classic multi-objective optimization
#' problem (NOWACKI, 1980). In this problem the aim is to design a tip loaded
#' cantilever beam for minimum cross-sectional area and lowest bending stress
#' subject to a number of constraints.
#'
#' @references Forrester, A., Sobester, A., & Keane, A. (2008).
#'   \emph{Engineering design via surrogate modelling: a practical guide.} John
#'   Wiley & Sons.
#'
#' @param x vector of length 2 corresponds the normalized breadth and height of the
#'   beam
#' @param g vector of length 5 containing the upper limits of each constraint
#' @param l numeric length of the beam
#' @param F numeric force applied at the beam tip
#' @param E numeric elastic longitudinal moduli
#' @param G numeric elastic transversal moduli
#' @param v numeric poison ratio
#' @param box data.frame structure containing the upper and lower limits for
#'   \code{b} and \code{h}
#' @return vector of objective and constrain responses
#' @export
#' @examples
#'
#' grid <- expand.grid(seq(0, 1, , 50),seq(0, 1, , 50))
#' res <- apply(grid, 1, nowacki_beam, box = data.frame(b = c(10, 50), h = c(50, 250)))
#' par(mfrow = c(3,3))
#' for(i in 1:nrow(res))
#'  contour(matrix(res[i,],50))
nowacki_beam <- function(x,
                         g = c(5, 240, 120, 10, 2),
                         l = 1500, F = 5000,
                         E = 216620, G = 86650, v = 0.27,
                         box = data.frame(b = c(10, 50),h = c(20, 250))){

  b <- x[1] * (box$b[2] - box$b[1]) + box$b[1]
  h <- x[2] * (box$h[2] - box$h[1]) + box$h[1]

  Iy <- b*h^3/12
  Iz <- b^3*h/12
  It <- Iy + Iz
  J <- Iy + Iz

  A <- b*h
  S <- 6*F*l/(b*h^2)
  g1 <- F*l^3/(3*E*Iy) - g[1]
  g2 <- S - g[2]
  g3 <- 3*F/(2*b*h) - g[3]
  g4 <- h/b - g[4]
  g5 <- - 4/(l^2) * sqrt(G * It * E * Iz/(1-v^2)) + g[5] * F

  return(c(A,S,g1,g2,g3,g4,g5))
}

#' True pareto front for the nowacki beam problem
#'
'nowacki_beam_tps'

#' Test functions for optimization
#'
#' This page is a collection of test functions commonly used to test optimization algorithms
#'
#' @references \url{https://en.wikipedia.org/wiki/Test_functions_for_optimization}
#' @references \url{http://www.sfu.ca/~ssurjano/optimization.html}
#'
#' @param x, numeric value (or vector for multivariable functions)
#' @name test_functions
NULL


#' @rdname test_functions
#' @export
#' @examples
#'
#' #function should be evaluated in the -A < x < A interval,
#' #where A is from 10 to 10^5 and \length(x) = 1
#' Shaffer1(0)
Shaffer1 <- function(x){
  f1 <- x^2
  f2 <- (x-2)^2
  return(c(f1,f2))
}

#' @rdname test_functions
#' @export
#' @examples
#'
#' #function should be evaluated in the -5 < x < 10 interval \length(x) = 1
#' Shaffer2(0)
Shaffer2 <- function(x){
  f1 <- sapply(x,function(x){
    if(x<=1) return(x)
    else if (x<=3) return(x-2)
    else if (x<=4) return(4-x)
    else return(x-4)
  })
  f2 <- (x-5)^2
  return(c(f1,f2))
}

#' @rdname test_functions
#' @export
#' @examples
#'
#' #function should be evaluated in the -20 < x < 20 interval and \length(x) >= 1
#' Fonseca(rep(0,10))
Fonseca <- function(x){
  d <- length(x)
  f1 <- 1-exp(-sum(x-1/sqrt(d))^2)
  f2 <- 1-exp(-sum(x+1/sqrt(d))^2)
  return(c(f1,f2))
}

#' @rdname test_functions
#' @export
#' @examples
#'
#' #function should be evaluated in the -5 < x < 5 interval and \length(x) == 3
#' Kursawe(rep(0,3))
Kursawe <- function(x){
  d <- 3
  if (length(x) != d)
    stop('length(x) must be 3')
  f1 <- -10*(exp(-0.2*sqrt(x[1]^2+x[2]^2))+exp(-0.2*sqrt(x[2]^2+x[3]^2)))
  f2 <- abs(x[1])^0.8+5*sin(x[1]^3) + abs(x[2])^0.8+5*sin(x[2]^3) + abs(x[3])^0.8+5*sin(x[3]^3)
  return(c(f1,f2))
}

#' @rdname test_functions
#' @export
#' @examples
#'
#' #function should be evaluated in the -3 < x < 3 interval and \length(x) == 2
#' Viennet(c(0.5,0.5))
Viennet <- function(x){
  x <- x*6 - 3
  y <- x[2]
  x <- x[1]
  yy <- y^2
  xx <- x^2
  f1 <- 0.5*(xx+yy) + sin(xx+yy)
  f2 <- (3*x-2*y+4)^2/8 + (x-y+1)^2/27 + 15
  f3 <- 1/(xx+yy+1) - 1.1*exp(-(xx+yy))
  return(c(f1,f2,f3))
}

#' @rdname test_functions
#' @export
#' @examples
#'
#' #function should be evaluated in the 0 < x < (5,3) interval and \length(x) == 2
#' Binh(c(0,0))
Binh <- function(x){
  y <- x[2]
  x <- x[1]
  f1 <- 4*x^2 + 4*y^2
  f2 <- (x-5)^2 + (y-5)^2
  g1 <- (x-5)^2 + y^2 -25
  g2 <- - (x-8)^2 - (y+3)^2 + 7.7
  return(c(f1,f2,g1,g2))
}
