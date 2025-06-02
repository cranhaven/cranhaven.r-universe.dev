mi_report <- function(theta){
  fxy <- theta$f
  fx <- theta$fx
  fy <- theta$fy
  fx.fy <- fx[fxy$x] * fy[fxy$y]
  den <- fxy$prob / ifelse(fx.fy == 0,1e-300,fx.fy)
  den <- ifelse(den == 0,1e-300, den)
  IM <- sum(fxy$prob * log(den))
  return(IM)
}
