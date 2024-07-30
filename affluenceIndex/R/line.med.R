line.med <-
function(x, weight, k){
  n <- length(x)
  ifelse(is.null(weight), weight <- rep(1, n), weight <- weight)
  
  median_inc <- weighted.median(x, weight)
  Gp <- sum(sapply(1:length(x), function(i) {
    max(k*median_inc - x[i], 0)*weight[i]
  }))
  
  x4 <- data.frame(x=x, weight=weight)
  x4 <- x4[with(x4, order(x, decreasing=TRUE)), ]
  
  x4_inc <- x4$x
  x4_w <- x4$weight
  
  xy <- 0
  xyz <- 0
  xyza <- 1
  
  while (xy == 0) {
    xyz <- sum(sapply(1:length(x4_inc), function(j) {max(x4_inc[j] - x4_inc[xyza], 0)*x4_w[j]}))
    if (xyz > Gp & xy == 0) {xy <- xyza} else {xyza <- xyza + 1}
  }
  
  rho_medeiros <- x4_inc[xy]
  median_multiple <- x4_inc[xy]/median_inc
  
  return(list(median_inc = median_inc, Gp = Gp, rho_medeiros = rho_medeiros, 
              median_multiple = median_multiple))
}
