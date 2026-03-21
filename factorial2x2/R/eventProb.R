#' Calculate event probabilities
#'
#' Calculates the event probabilities for each of the four factorial groups
#' C, A, B, AB.  The time unit is in years, but of course, any time
#' unit could be used.  Average event probabilities across various
#' combinations of the groups are also calculated.
#' The event times are assumed to be exponentially distributed.  The
#' censoring times are assumed to be uniformly distributed and indepedent of
#' the event times.
#' @param rateC group C one year event rate
#' @param hrA   group A to group C hazard ratio
#' @param hrB  group B to group C hazard ratio
#' @param hrAB  group AB to group C hazard ratio
#' @param mincens  minimum censoring time
#' @param maxcens  maximum censoring time
#'
#' @return \item{hazC }{group C's exponential hazard rate}
#' @return \item{probC }{event probability of the C group}
#' @return \item{probA }{event probability of the A group}
#' @return \item{probB }{event probability of the B group}
#' @return \item{probAB }{event probability of the AB group}
#' @return \item{avgprob }{average event probability across all factorial groups}
#' @return \item{probA_C}{average event probablity of the A and C groups}
#' @return \item{probAB_C}{average event probablity of the AB and C groups}
#' @export eventProb
#' @examples
#' # Corresponds to scenario 5 of Table 2 from Leifer, Troendle, et al. (2019).
#' rateC <- 0.0445
#' hrA <- 0.80
#' hrB <- 0.80
#' hrAB <- 0.72
#' mincens <- 4.0
#' maxcens <- 8.4
#' eventProb(rateC, hrA, hrB, hrAB, mincens, maxcens)
#' # hazC
#' # [1] 0.04552052
#'
#' # probC
#' # [1] 0.2446365
#'
#' # probA
#' # [1] 0.201254
#'
#' # probB
#' # [1] 0.201254
#'
#' # probAB
#' # [1] 0.1831806
#'
#' # avgprob
#' # [1] 0.2075813
#'
#' # probAB_C
#' # [1] 0.2139086

eventProb <- function(rateC, hrA, hrB, hrAB, mincens, maxcens){

  survrtC <- 1 - rateC
  hazC <-  -log(survrtC)
  ratevec <- hazC * c(1, hrA, hrB, hrAB)
  a <- mincens
  b <- maxcens
  eventvec <- rep(0, 4)
  for(i in 1:4){
    eventvec[i] <- 1 +
      ((b-a)*ratevec[i])^(-1) * ( exp(-ratevec[i] * b) -
                                   exp(-ratevec[i] * a))
  }
  list(hazC = hazC, probC = eventvec[1], probA = eventvec[2],
       probB = eventvec[3], probAB = eventvec[4],
       avgprob = mean(eventvec), probA_C = mean(eventvec[c(1, 2)]),
       probAB_C = mean(eventvec[c(1, 4)]))
}


