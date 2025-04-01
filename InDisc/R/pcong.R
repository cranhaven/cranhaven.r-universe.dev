pcong <- function(thres, alpha, theta, vari, vres){

  tmp1 <- thres - (alpha * theta)
  tmp2 <- alpha * (sqrt(vari+vres))
  tmp3 <- (-1) * (tmp1/tmp2)
  n <- exp(1.702*tmp3)
  d <- n / (1+n)


}
