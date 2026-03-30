#' Fisher's Exact Test for a 2x2 Contingency Table
#'
#' A wrapper for `stats::fisher.test` to conveniently compare response rates
#' between two arms (experimental and control).
#'
#' @param Yt A scalar integer. The number of subjects with a response in the
#'   experimental (treatment) arm.
#' @param nt A scalar integer. The total number of subjects in the
#'   experimental arm.
#' @param Yc A scalar integer. The number of subjects with a response in the
#'   control arm.
#' @param nc A scalar integer. The total number of subjects in the control arm.
#' @param alternative A character string specifying the alternative hypothesis.
#'   Must be one of "two.sided" (default), "greater" or "less".
#'
#' @return An object of class `htest` as returned by `stats::fisher.test`.
#'
#' @seealso \code{\link[stats]{fisher.test}}
#'
#' @examples
#' fisher(Yc=12, nc=40, Yt=19, nt=40)
#'
#' @export
#'
fisher <- function(Yc, nc, Yt, nt, alternative = "greater") {


  M <- as.table(rbind(c(Yt, Yc), c(nt - Yt, nc - Yc)))
  dimnames(M) <- list(Response = c("Response", "Non-resp"),
                      Group = c("Exp", "control"))

  f <- stats::fisher.test(M, alternative = alternative)
  return(f)
}
