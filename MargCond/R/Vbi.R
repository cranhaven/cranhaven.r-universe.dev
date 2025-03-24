Vbi <-
function(D, Z_i, R){
  D - (D %*% t(Z_i)) %*% solve(Z_i %*% D %*% t(Z_i) + R) %*% t(D %*% t(Z_i))
}
