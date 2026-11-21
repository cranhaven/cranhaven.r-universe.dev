#' @title LDA_survival_single
#'
#' @description calculate clonogenic survival fraction from LDA_activity
#'   objects.
#'
#' @param act.0 reference activity
#' @param act.x activity after treatment
#'
#' @return list object with survival fraction, estimated confidence intervals
#'   (by error propagation through first order Taylor series approximation and
#'   by combination of 84%-uncertainty-intervals of activity estimates)
#'
#' @examples
#' x.a <- data.frame("cells" = c(10,50,100,250),
#'                 "wells" = rep(25,4),
#'                 "positive" = c(2,5,10,20))
#' x.b <- data.frame("cells" = c(10,50,100,250),
#'                 "wells" = rep(25,4),
#'                 "positive" = c(1,2,6,11))
#' act.a <- LDA_activity_single(x.a)
#' act.b <- LDA_activity_single(x.b)
#' sf <- LDA_survival_single(act.0 = act.a,act.x = act.b)
#' data(LDAdata)
#' cell.line <- unique(LDAdata$name)[1]
#' x <- subset.data.frame(LDAdata, subset = (name==cell.line) & (Group < 2))
#' act <- LDA_activity(x[,c(4:6,3)])
#' sf <- LDA_survival_single(act.0 = act[[1]],act.x = act[[2]])
#' @export
#'
LDA_survival_single <- function(act.0,act.x){
  result <- list("treat" = act.x$treatment,
              "sf" = NA,
              "CI.ep" = c(NA,NA),
              "CI.act" = c(NA,NA))
  est.0 <- act.0$est
  Sigma.0 <- act.0$Sigma
  est.x <- act.x$est
  Sigma.x <- act.x$Sigma

  # g = log(sf)
  g <- est.x[1]/est.x[2] - est.0[1]/est.0[2]
  result$sf <- exp(g)

  sig.g2 <- 1/est.0[2]^2 * ( Sigma.0[1,1] -
                               2 * est.0[1]/est.0[2] * Sigma.0[1,2] +
                               (est.0[1]/est.0[2])^2 * Sigma.0[2,2] )  +
    1/est.x[2]^2 * ( Sigma.x[1,1] - 2 * est.x[1]/est.x[2] * Sigma.x[1,2] +
                       (est.x[1]/est.x[2])^2 * Sigma.x[2,2] )
  sig.g <- sqrt(sig.g2)

  result$CI.ep <- exp(g+qnorm(p = c(0.025,0.975))*sig.g)

  result$CI.act <- c(act.0$CI.appr.SF[1]/act.x$CI.appr.SF[2],
                     act.0$CI.appr.SF[2]/act.x$CI.appr.SF[1])
  # see Schenker, Austin, Payton, Knol, etc.
  return(result)
}
