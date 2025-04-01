lrtseg <- function(DIF, RES, alpha, varc, vres, th0, th1, vari){

  tmp1 <- verosi2(DIF, RES, alpha, vres, th1, vari)

  tmp2 <- verosi2(DIF, RES, alpha, vres, th0, varc)

  LR <- tmp2 / tmp1

  if (LR>1){
    LR <- 1
  }

  chi <- -2 * log(LR)

  OUT <- list("LR"=LR, "chi" = chi)

}
