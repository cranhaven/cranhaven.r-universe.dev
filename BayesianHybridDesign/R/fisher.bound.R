#' Calculate Rejection Boundary for Fisher's Exact Test
#'
#' For a given control group response rate and sample sizes, this function
#' finds the smallest number of responders in the experimental arm (`rt`) that
#' achieves statistical significance based on a one-sided Fisher's exact test.
#'
#' @param pc A scalar numeric. The response rate for the control arm.
#' @param nc A scalar integer. The number of subjects in the control arm.
#' @param nt A scalar integer. The number of subjects in the experimental arm.
#' @param alpha A scalar numeric. The one-sided p-value threshold for
#'   statistical significance. Default is 0.1.
#'
#' @return A list containing the following components:
#' \describe{
#'   \item{M}{The 2x2 contingency table at the boundary.}
#'   \item{p}{The p-value corresponding to the boundary.}
#'   \item{rc}{The number of responders in the control arm, calculated as `round(nc * pc)`.}
#'   \item{nc}{The sample size of the control arm.}
#'   \item{rt}{The smallest number of responders in the experimental arm that
#'     achieves a p-value <= `alpha`.}
#'   \item{nt}{The sample size of the experimental arm.}
#'   \item{delta}{The minimum detectable difference in response rates (`rt/nt - rc/nc`).}
#' }
#'
#' @examples
#' fisher.bound(pc=0.3, nc=40, nt=40, alpha=0.1)
#'
#' @export
#'
fisher.bound <- function(pc, nc, nt, alpha = 0.1) {
  rc <- round(nc * pc)

  for (rt in 1:nt) {
    M <- as.table(cbind(c(rt, nt - rt), c(rc, nc - rc)))
    dimnames(M) <- list(Response = c("Response", "Non-resp"),
                        Group = c("Exp", "control"))

    o <- stats::fisher.test(M, alternative = "greater")

    p <- o$p.value
    if (p <= alpha) {
      break
    }
  }

  deltaboundary <- rt / nt - rc / nc

  if (p > alpha) {
    warning("Could not find a rejection boundary. The returned p-value is > alpha.")
  }

  return(list(M = M,
              p = p,
              rc = rc,
              nc = nc,
              rt = rt,
              nt = nt,
              delta = deltaboundary))
}
