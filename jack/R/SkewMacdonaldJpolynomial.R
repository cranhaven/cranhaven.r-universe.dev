.als <- function(lambda, lambdap) {
  do.call(
    rbind,
    apply(
      cbind(lambda, seq_along(lambda)),
      1L,
      function(mi) {
        m <- mi[1L]
        i <- mi[2L]
        t(apply(
          cbind(head(lambdap, m), seq_len(m)),
          1L,
          function(mpj) {
            mp <- mpj[1L]
            j <- mpj[2L]
            c(m - j, mp - i)
          }
        ))
      }, simplify = FALSE
    )
  )
}

.poly <- function(alc) {
  spray <- new(
    "qspray",
    powers = list(integer(0L), c(alc[1L], alc[2L] + 1L)),
    coeffs = c("1", "-1")
  )
  spray^(alc[3L])
}

#' @importFrom partitions conjugate
#' @importFrom qspray qone
#' @noRd
clambdamu <- function(lambda, mu) {
  if(length(mu) == 0L) {
    return(clambda(lambda))
  }
  lambdap <- conjugate(lambda)
  mup <- conjugate(mu)
  als_lambda <- .als(lambda, lambdap)
  als_mu <- .als(mu, mup)
  matrices <- simplifyTheTwoMatrices(als_lambda, als_mu)
  matrix1 <- matrices[[1L]]
  if(nrow(matrix1) >= 1L) {
    num <-
      Reduce(
        `*`,
        apply(matrix1, 1L, .poly, simplify = FALSE)
      )
  } else {
    num <- qone()
  }
  matrix2 <- matrices[[2L]]
  if(nrow(matrix2) >= 1L) {
    den <-
      Reduce(
        `*`,
        apply(matrix2, 1L, .poly, simplify = FALSE)
      )
  } else {
    den <- qone()
  }
  num / den
}
