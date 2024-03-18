#' Maximum number of unique one-to-one association over a number of runs
#'
#' For a binary matrix of host-symbiont associations, it finds the maximum
#' number of host-symbiont pairs, \code{n}, for which one-to-one unique
#' associations can be chosen.
#'
#' @param HS Host-symbiont association matrix.
#'
#' @param reps Number of runs to evaluate.
#'
#' @param interval Vector with the minimum and maximum \code{n} that the user
#'        wants to test. Default is \code{"NULL"}, where a minimum \code{n}
#'        (10% of the total associations) and a maximum \code{n} (20% of the
#'        total associations) are automatically assigned.
#'
#' @param strat Flag indicating whether execution is to be  \code{"sequential"}
#'        or \code{"parallel"}. Default is \code{"sequential"},
#'        resolves \R expressions sequentially in the current \R
#'        process. If \code{"parallel"} resolves \R expressions in parallel in
#'        separate \R sessions running in the background.
#'
#' @param cl Number of cluster to be used for parallel computing.
#'        \code{\link[parallelly:availableCores]{parallelly::availableCores()}}
#'        returns the number of clusters available.
#'        Default is \code{cl = 1} resulting in \code{"sequential"} execution.
#'
#' @param plot Default is \code{"TRUE"}, plots the number of unique host-
#'        symbiont associations in the \code{"interval"} range against the
#'        number of runs that could be completed.
#'
#'
#' @return The maximum number of unique one-to-one associations
#'         (\code{n}).
#'
#' @section NOTE:
#'          It can be used to decide the best \code{n} prior to application of
#'          \code{\link[=max_cong]{max_cong()}}.
#'
#' @import stringr
#'
#' @examples
#' N = 10  #for the example, we recommend 1e+4 value
#' data(amph_trem)
#' n <- one2one_f(am_matrix, reps = N, interval = c(2, 10), plot = TRUE)
#'
#' @export
one2one_f <- function(HS, reps = 1e+4, interval = NULL, strat = "sequential",
                      cl = 1, plot = TRUE){

  one2one <- function (HS, ...) {
    HS.LUT <- which(HS == 1, arr.ind = TRUE)
    HS.LUT <- cbind(HS.LUT,1:nrow(HS.LUT))
    df <- as.data.frame(HS.LUT)
    V <- rep(NA,reps)
    for(i in 1:reps){
      hs.lut <- subset(df[sample(nrow(df)),],
                       !duplicated(row) & !duplicated(col))
      n <- sum(HS)
      while (n > 0) {
        n <- n-1;
        if (nrow(hs.lut) == n) break
      }
      V[i] <- n
    }
    V <- min(V)
    return(V)
  }

  if (sum(HS) == ncol(HS))
  stop("If all associations are one-to-one associations, n max would be the
       sum of all associations in HS, not not suitable for running the
       algorithms. You can choose between 10 and 20 % of the total number
       of associations")

  if (is.null(interval) == TRUE) {
    N <- sum(HS)
    Nlo <- round(N * 0.1)
    Nhi <- round(N * 0.2)
    int <- c(Nlo, Nhi)
  } else {
    int <- interval
  }

  one  <- one2one(HS, reps)
  a <- int[1]:int[2]
  if (plot == TRUE) {
    b <- rep(NA, length(a))
    for (i in 1:length(a)) {
      THS <- trimHS_maxC(reps, HS, n=a[i], check.unique = TRUE,
                         strat = strat, cl = cl)
      b[i] <- length(THS)
    }
    plot(a, b, type = "b", xlim = c(min(a), max(a)), xaxt = "n",
         xlab = "Number of unique H-S associations",
         ylab = "Number of runs accomplished") +
      axis(1, seq(round(min(a)), round(max(a)), by = 1),
           labels = (min(a):max(a)))
  }
  return(one)
}
