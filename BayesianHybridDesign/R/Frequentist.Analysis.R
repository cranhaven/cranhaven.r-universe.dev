#' Statistical Analysis Using Frequentist Method
#'
#' Performs a frequentist analysis of a two-arm study using current data only.
#'
#' @param Yt A scalar. The number of responses in the experimental arm.
#' @param nt A scalar. The number of subjects in the experimental arm.
#' @param Yc A scalar. The number of responses in the control arm.
#' @param nc A scalar. The number of subjects in the control arm.
#' @param conflev A scalar. The desired confidence level for the confidence intervals.
#'
#' @return An object of class `list` with the following values:
#' \itemize{
#'   \item pt: The response rate of the experimental arm.
#'   \item pc: The response rate of the control arm.
#'   \item delta: The difference in response rates (pt - pc).
#'   \item exactCI.c: A vector containing the Clopper-Pearson confidence
#'     interval for the control arm.
#'   \item exactCI.t: A vector containing the Clopper-Pearson confidence
#'     interval for the experimental arm.
#'   \item p.fisher: A one-sided p-value from Fisher's exact test, testing
#'     the alternative hypothesis that the experimental response rate is greater
#'     than the control response rate.
#' }
#'
#' @examples
#' Frequentist.Analysis(Yt=18, nt=40, Yc=13, nc=40, conflev=0.8)
#'
#' @export
#'
Frequentist.Analysis <- function(Yt = 20, nt = 40, Yc = 12, nc = 40, conflev = 0.8) {

  # Clopper-Pearson CI
  CI_c <- exactci(r = Yc, n = nc, conflev = conflev)
  CI_t <- exactci(r = Yt, n = nt, conflev = conflev)

  pt <- Yt / nt
  pc <- Yc / nc

  delta <- pt - pc

  # Fisher's exact test
  p.fisher <- fisher(Yc = Yc, nc = nc, Yt = Yt, nt = nt)$p.value

  return(list(pt = pt, pc = pc, delta = delta,
              exactCI.c = CI_c, exactCI.t = CI_t,
              p.fisher = p.fisher))
}
