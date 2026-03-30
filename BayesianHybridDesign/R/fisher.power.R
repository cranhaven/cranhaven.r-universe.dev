#' Power Calculation by Fisher's Exact Test
#'
#' This function calculates the power by simulations using Fisher's exact test.
#'
#' @param pt A scalar. Probablity of success in experimental arm
#' @param pc A scalar. Probablity of success in control arm
#' @param nt A scalar. Number of subject in experimental arm
#' @param nc A scalar. Number of subject in control arm
#' @param alpha A scalar. One sided type I error rate.
#' @param nsim A scalar. Number of replications to calculate power. Default
#'   100,000.
#' @param seed An integer. A seed for the random number generator to ensure
#'   reproducibility. Note: For best practice, it is recommended to call
#'   `set.seed()` outside of the function.
#'
#' @return A numeric scalar representing the calculated statistical power.
#'
#' @examples
#' # nsim is reduced for a quick example run
#' fisher.power(pt = 0.5, nt = 40, pc = 0.3, nc = 40, nsim = 1000, seed = 2000)
#'
#' @export
#'
fisher.power <- function(pt, nt, pc, nc, alpha = 0.1,
                         nsim = 10000, seed = NULL) {
  if(!is.null(seed)){
    set.seed(seed)
  }
  sig <- 0

  for (i in 1:nsim) {
    Yc <- rbinom(1, size = nc, prob = pc)
    Yt <- rbinom(1, size = nt, prob = pt)
    p <- fisher(Yc = Yc, nc = nc, Yt = Yt, nt = nt,
                alternative = "greater")$p.value
    if (p <= alpha) {
      sig <- sig + 1
    }
  }
  power <- sig / nsim
  return(power)
}
