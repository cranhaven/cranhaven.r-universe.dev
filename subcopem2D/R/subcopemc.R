subcopemc <-
function(mat.xy, m = nrow(mat.xy), display = FALSE){
# 
# Input:  mat.xy = 2-column matrix with n bivariate observations of continuous random vector (X,Y)
#                  ** repeated values are not expected but allowed (jittering will be applied)
#                  ** NA values are not allowed
#                  ** usually m = 50 is a good recommended value if n > 500, higher values demand more computing time
#                  ** if display = TRUE dependence measures and graphs are displayed
#
# Output: Empirical 2-subcopula matrix of order m in {1,...,n}, 
#         induced partitions, standardized sample, and dependence measures
#              depMon = monotone standardized supremum distance in [-1,1]
#        depMonNonSTD = monotone non-standardized supremum distance [min,value,max]
#              depSup = standardized supremum distance in [0,1]
#        depSupNonSTD = non-standardized supremum distance [min,value,max]
#             ** If m = n (default) it is the usual empirical subcopula
#       ** if display = TRUE then dependence measures and graphs are displayed
#
# Checking for appropriate order m
  n <- nrow(mat.xy)  # sample size
  if (!(m %in% (1:n))){
    error.msg <- paste("Order m must be an integer value in 1:n, n =", n)
    stop(error.msg)
  } else{
# Checking for repeated values:
  mensaje <- character(0)
  X <- mat.xy[ , 1]
  Y <- mat.xy[ , 2]
  if ((length(unique(X)) < n) | (length(unique(Y)) < n)){
    mensaje <- "Presence of repeated values, jittering has been applied"
    ind.X <- which(duplicated(X))
    ind.Y <- which(duplicated(Y))
    if (length(ind.X) > 0) X[ind.X] <- jitter(X[ind.X])
    if (length(ind.Y) > 0) Y[ind.Y] <- jitter(Y[ind.Y])
    mat.xy <- cbind(X, Y)
  }  
# Subcopula of order m:
  r.xy <- apply(mat.xy, 2, rank)  # bivariate ranks
  kparticion <- round(seq(0, n, length = (m + 1)), 0)  # integer partition of order m
  r.ordx <- r.xy[order(r.xy[ , 1]) , ]  # reordering bivariare ranks on first variable
  contar.aux <- function(kx, ky) sum(r.ordx[1:kx, 2] <= ky)  # counting function
  contar <- function(kx, ky) mapply(contar.aux, kx, ky)  # vectorized counting function
  subcopula <- matrix(0, nrow = (m + 1), ncol = (m + 1))  # creating subcopula matrix
  # subcopula calculation:
  subcopula[2:(m + 1), 2:(m + 1)] <- (1/n)*outer(kparticion[2:(m + 1)], kparticion[2:(m + 1)], contar)
# Induced partition:
  particion <- kparticion/n
# Standardized sample:
  muestra.std <- r.xy/n
# Dependence measures:
  M <- function(u, v) (u + v - abs(u - v))/2  # upper bound copula
  W <- function(u, v) (u + v - 1 + abs(u + v - 1))/2  # lower bound copula
  P <- function(u, v) u*v  # product copula (independence)
  subcopM <- outer(particion, particion, M)  # upper bound subcopula
  subcopW <- outer(particion, particion, W)  # lower bound subcopula
  subcopP <- outer(particion, particion, P)  # product subcopula (independence)
  dsgn <- function(A, B) max(A - B) - max(B - A)  # signed supremum distance
  dsup <- function(A, B) max(abs(A - B))  # supremum distance
  Msgn <- dsgn(subcopM, subcopP)  # upper bound signed distance
  Wsgn <- dsgn(subcopW, subcopP)  # lower bound signed distance
  d <- dsgn(subcopula, subcopP)
  depMon <- (d >= 0)*d/Msgn - (d < 0)*d/Wsgn  # monotone standardized supremum distance
  depMonNonSTD <- c(4*Wsgn, 4*d, 4*Msgn) # monotone non-standardized supremum distance
  names(depMonNonSTD) <- c("min", "value", "max")
  Msup <- dsup(subcopM, subcopP)  # upper bound supremum distance
  Wsup <- dsup(subcopW, subcopP)  # lower bound supremum distance
  supBound <- max(Msup, Wsup)  # supremum distance bound
  depSup <- dsup(subcopula, subcopP)/supBound  # standardized supremum distance
  depSupNonSTD <- c(0, 4*supBound*depSup, 4*supBound) # non-standardized supremum distance
  names(depSupNonSTD) <- c("min", "value", "max")
# Output:
  SC <- list(depMon = depMon, depMonNonSTD = depMonNonSTD, depSup = depSup,
             depSupNonSTD = depSupNonSTD, matrix = subcopula, part1 = particion,
             part2 = particion, sample.size = n, order = m, std.sample = muestra.std,
             sample = mat.xy)
  if (length(mensaje) > 0) warning(mensaje)
  if (display == TRUE){
    message("monotone dependence = [ -1 , ", round(depMon, 8), " , 1 ]")
    message("non-std interval = [ ", round(depMonNonSTD[1], 8), " , ",
            round(depMonNonSTD[2], 8), " , ", round(depMonNonSTD[3], 8), " ]")
    message("supremum dependence = [ 0 , ", round(depSup, 8), " , 1 ]")
    message("non-std interval = [ ", round(depSupNonSTD[1], 8), " , ",
            round(depSupNonSTD[2], 8), " , ", round(depSupNonSTD[3], 8), " ]")
    dev.new(); par(mfcol = c(2, 3))
    hist(mat.xy[ , 1], main = "histogram", xlab = "X")
    hist(mat.xy[ , 2], main = "histogram", xlab = "Y")
    plot(mat.xy, main = "sample", xlab = "X", ylab = "Y")
    plot(c(0, 1), c(0, 1), type = "n", main = "standardized sample", ylab = "",
         xlab = paste("monotone dependence =", round(depMon, 3)))
    points(muestra.std)
    contour(particion, particion, subcopula, nlevels = 20, main = "subcopula", ylab = "",
            xlab = "")
    image(particion, particion, subcopula, col = heat.colors(20), main = "subcopula", ylab = "",
          xlab = paste("supremum dependence =", round(depSup, 3)))
  }
  return(SC)
  }
}
