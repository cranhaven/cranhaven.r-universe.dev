rmvt <- function(n, SqrtSigma, df){
  stopifnot(is.numeric(df), length(df) == 1, df > 0,
            isInteger(n), n > 0, is.numeric(SqrtSigma), is.matrix(SqrtSigma))
  t(t(rmvnorm(reps = n, SqrtSigma = SqrtSigma))/sqrt(rchisq(n, df)/df))
}
