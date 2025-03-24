Ebi <-
function(D, Z_i, R, y, X_i, B){
  (D %*% t(Z_i)) %*% solve(Z_i %*% D %*% t(Z_i) + R) %*% (y - X_i %*% B) 
}
