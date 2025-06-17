subcopem <-
function(mat.xy, display = FALSE){
# 
# Input:  mat.xy = 2-column matrix with bivariate observations of (X,Y)
#                  ** repeated values are allowed, NA values not allowed
#                  ** slow if number of distinct values > 2000 (time > 2 min)
#                  ** if X,Y continuous => should better use <subcopemc> instead
#
# Output: Empirical 2-subcopula matrix, induced partitions,
#         standardized sample, and dependence measures
#              depMon = monotone standardized supremum distance in [-1,1]
#        depMonNonSTD = monotone non-standardized supremum distance [min,value,max]
#              depSup = standardized supremum distance in [0,1]
#        depSupNonSTD = non-standardized supremum distance [min,value,max]
#       ** if display = TRUE then dependence measures and graphs are displayed
#
  n <- nrow(mat.xy)  # sample size
  X <- mat.xy[ , 1]  # X observed values
  Y <- mat.xy[ , 2]  # Y observed values
# Subcopula:
  R <- sort(unique(X))  # unique values of X (sorted)
  m1 <- length(R)  # number of unique values of X
  S <- sort(unique(Y))  # unique values of Y (sorted)
  m2 <- length(S)  # number of unique values of Y
  contar.aux <- function(r, s) sum((X <= r)*(Y <= s))  # counting function
  contar <- function(r, s) mapply(contar.aux, r, s)  # vectorized counting function
  subcopula <- matrix(0, nrow = (m1 + 1), ncol = (m2 + 1))  # creating subcopula matrix
  subcopula[2:(m1 + 1), 2:(m2 + 1)] <- (1/n)*outer(R, S, contar)  # subcopula calculation
# Partitions:
  p1 <- as.vector(table(sort(X))/n)  # observed proportion of unique values of X
  p2 <- as.vector(table(sort(Y))/n)  # observed proportion of unique values of Y
  q1 <- cumsum(p1)  # cumulative values of p1
  q2 <- cumsum(p2)  # cumulative values of p2
  partQ1 <- c(0, q1)  # first partition
  partQ2 <- c(0, q2)  # second partiion
# Standardized sample:
  f.aux <- function(x, A, q) q[which(x == A)]  # mapping function A -> q
  fR <- function(x) sapply(x, f.aux, A = R, q = q1)  # mapping function for X
  fS <- function(y) sapply(y, f.aux, A = S, q = q2)  # mapping function for Y
  muestra.std <- matrix(nrow = n, ncol = 2)  # creating matrix
  muestra.std[ , 1] <- fR(mat.xy[ , 1])  # calculating standardized sample
  muestra.std[ , 2] <- fS(mat.xy[ , 2])  # calculating standardized sample
# Dependence measures:
  M <- function(u, v) (u + v - abs(u - v))/2  # upper bound copula
  W <- function(u, v) (u + v - 1 + abs(u + v - 1))/2  # lower bound copula
  P <- function(u, v) u*v  # product copula (independence)
  subcopM <- outer(partQ1, partQ2, M)  # upper bound subcopula
  subcopW <- outer(partQ1, partQ2, W)  # lower bound subcopula
  subcopP <- outer(partQ1, partQ2, P)  # product subcopula (independence)
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
             depSupNonSTD = depSupNonSTD, matrix = subcopula, part1 = partQ1, 
             part2 = partQ2, sample.size = n, std.sample = muestra.std, sample = mat.xy)
  if (display == TRUE){
    message("monotone dependence = [ -1 , ", round(depMon, 8), " , 1 ]")
    message("non-std interval = [ ", round(depMonNonSTD[1], 8), " , ",
            round(depMonNonSTD[2], 8), " , ", round(depMonNonSTD[3], 8), " ]")
    message("supremum dependence = [ 0 , ", round(depSup, 8), " , 1 ]")
    message("non-std interval = [ ", round(depSupNonSTD[1], 8), " , ",
            round(depSupNonSTD[2], 8), " , ", round(depSupNonSTD[3], 8), " ]")
    dev.new(); par(mfcol = c(2,  3))
    hist(mat.xy[ , 1], main = "histogram", xlab = "X")
    hist(mat.xy[ , 2], main = "histogram", xlab = "Y")
    plot(mat.xy, main = "sample", xlab = "X", ylab = "Y")
    plot(c(0, 1), c(0, 1), type = "n", main = "standardized sample", ylab = "",
         xlab = paste("monotone dependence =", round(depMon, 3)))
    points(muestra.std)
    contour(partQ1, partQ2, subcopula, nlevels = 20, main = "subcopula", ylab = "",
            xlab = "")
    image(partQ1, partQ2, subcopula, col = heat.colors(20), main = "subcopula", ylab = "",
          xlab = paste("supremum dependence =", round(depSup, 3)))   
  }
  return(SC) 
}
