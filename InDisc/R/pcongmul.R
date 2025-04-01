pcongmul <- function(thres, alpha, theta, vari, vres){

  nume <- thres - (alpha %*% theta)
  tmp1 <- sqrt(alpha %*% transpose(alpha))
  tmp2 <- tmp1 * (sqrt(vari+vres))
  tmp3 <- (-1) * (nume/tmp2)
  n <- exp(1.702*tmp3)
  d <- n / (1+n)


}
