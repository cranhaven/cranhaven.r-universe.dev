# Internal function for the rotation (flip) of copula
.rotateCopula = function(x, flip) {
  if (dim(x)[2] > 2 & flip != 0) {
stop(
"Rotated copula are only available for dimension 2."
)}
  if (flip == 90) {
    flipCop = cbind(1 - x[,2], x[,1])
  } else if (flip == 180) {
    flipCop = cbind(1 - x[,1], 1 - x[,2])
  } else if (flip == 270) {
    flipCop = cbind(x[,2], 1 - x[,1])
  } else {
    flipCop = x
  }
  return(flipCop)
}