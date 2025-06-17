Bcopula <-
function(mat.xy, m, both.cont = FALSE, tolimit = 0.00001){
#
# Input:  mat.xy = 2-column matrix with bivariate observations of a random vector (X,Y)
#                  ** repeated values are allowed, NA values not allowed
#              m = Approximation order (an integer 2,3,...,n = sample size)
#                  ** it is recommended tu use m = min{50, n^(1/2)}
#      both.cont = if TRUE then (X,Y) are considered (both) as continuos random variables,
#                  and jittering will be applied to repeated values (if any)
#        tolimit = tolerance limit in numerical approximation of the inverse of the
#                  partial derivatives of the estimated Bernstein copula
#
# Output:     copula = bivariate function Bernstein Copula (BC) of order m
#             du = bivariate function dBCopula(u,v)/du 
#             dv = bivariate function dBCopula(u,v)/dv
#             du.inv = inverse of BC.du with respect to v given u and alpha (numerical aprox with tol)
#             dv.inv = inverse of BC.du with respect to u given v and alpha (numerical aprox with tol)
#             density = bivariate function Bernstein copula density of order m
#             bilinearCopula = bivariate function of bilinear approximation of copula
#             bilinearSubcopula = (m+1) x (m+1) matrix with empirical subcopula values
#             sample.size of bivariate observations
#             order of approximation (m)
#             both.cont = logical value, TRUE if both variables considered as continuous
#             tolimit = tolerance limit in numerical approximation of du.inv and dv.inv
#             subcopemObject = list with the output of subcopem (both.cont = FALSE)
#                              or subcopemc (both.cont = TRUE)
#
# Dependencies: subcopem and subcopemc functions in subcopem2D package
#
# Checking for appropriate order m
  n <- nrow(mat.xy)  # sample size
  if (!(m %in% (2:n))){
    error.msg <- paste("Order m must be an integer value in 2:n, n =", n)
    stop(error.msg)
  } else{
# Calculate bilinear interpolation of order m:
  if (both.cont){
    SC <- subcopemc(mat.xy, m)
  } else{
    SC <- subcopem(mat.xy)
  }
  um <- (0:m)/m
  vm <- um
  Cm <- matrix(0, nrow = (m + 1), ncol = (m + 1))
  Cm[ , m + 1] <- um # C(i/m, 1) = i/m
  Cm[m + 1, ] <- vm  # C(1, i/m) = i/m
  L <- function(u){
         u.inf <- max(SC$part1[SC$part1 <= u])
         u.sup <- min(SC$part1[SC$part1 >= u])
         valor <- ifelse(u.inf < u.sup, (u - u.inf)/(u.sup - u.inf), 1)
         return(valor)
       }
  M <- function(v){
         v.inf <- max(SC$part2[SC$part2 <= v])
         v.sup <- min(SC$part2[SC$part2 >= v])
         valor <- ifelse(v.inf < v.sup, (v - v.inf)/(v.sup - v.inf), 1)
         return(valor)
       }
  S <- SC$matrix # empirical subcopula
  n1 <- nrow(S)
  n2 <- ncol(S)
  iu.inf <- function(u) sum(SC$part1 <= u)
  iu.sup <- function(u) n1 - sum(SC$part1 >= u) + 1
  iv.inf <- function(v) sum(SC$part2 <= v)
  iv.sup <- function(v) n2 - sum(SC$part2 >= v) + 1
  C.bilineal <- function(u, v) (1-L(u))*(1-M(v))*S[iu.inf(u),iv.inf(v)] + 
                               (1-L(u))*M(v)*S[iu.inf(u),iv.sup(v)] +
                                L(u)*(1-M(v))*S[iu.sup(u),iv.inf(v)] +
                                L(u)*M(v)*S[iu.sup(u),iv.sup(v)]
  for (i in 2:m){
    for (j in 2:m){
      Cm[i, j] <- C.bilineal(um[i], vm[j])
    }
  }
# Bernstein copula functions
  BCopula <- function(u, v) sum(Cm*(dbinom(0:(dim(Cm)[1] - 1), dim(Cm)[1] - 1, u)%*%t(dbinom(0:(dim(Cm)[1] - 1), dim(Cm)[1] - 1, v))))
  BC.du <- function(u, v) (dim(Cm)[1] - 1)*sum(Cm*((dbinom(-1:(dim(Cm)[1] - 2), dim(Cm)[1] - 2, u) - dbinom(0:(dim(Cm)[1] - 1), 
                          dim(Cm)[1] - 2, u)*c(-1, rep(1, dim(Cm)[1] - 1)))%*%t(dbinom(0:(dim(Cm)[1] - 1), dim(Cm)[1] - 1, v))))
  BC.du.aux <- function(v, ua.vec) BC.du(ua.vec[1], v) - ua.vec[2]
  BC.du.inv <- function(u, a) uniroot(BC.du.aux, interval = c(0, 1), ua.vec = c(u, a), tol = tolimit)$root
  tCm <- t(Cm) # transpose u <-> v
  BC.dv0 <- function(u, v) (dim(tCm)[1] - 1)*sum(tCm*((dbinom(-1:(dim(tCm)[1] - 2), dim(tCm)[1] - 2, u) - dbinom(0:(dim(tCm)[1] - 1), 
                           dim(tCm)[1] - 2, u)*c(-1, rep(1, dim(tCm)[1] - 1)))%*%t(dbinom(0:(dim(tCm)[1] - 1), dim(tCm)[1] - 1, v))))
  BC.dv0.aux <- function(v, ua.vec) BC.dv0(ua.vec[1], v) - ua.vec[2]
  BC.dv0.inv <- function(u, a) uniroot(BC.dv0.aux, interval = c(0, 1), ua.vec = c(u, a), tol = tolimit)$root
  BC.dv <- function(u, v) BC.dv0(v, u)
  BC.dv.inv <- function(v, a) BC.dv0.inv(v, a)
  Bdensity <- function(u, v) ((dim(Cm)[1] - 1)^2)*sum(Cm*((dbinom(-1:(dim(Cm)[1] - 2), dim(Cm)[1] - 2, u) - dbinom(0:(dim(Cm)[1] - 1),
                             dim(Cm)[1] - 2, u)*c(-1, rep(1, dim(Cm)[1] - 1)))%*%t(dbinom(-1:(dim(Cm)[1] - 2), dim(Cm)[1] - 2, v) -
                             dbinom(0:(dim(Cm)[1] - 1), dim(Cm)[1] - 2, v) * c(-1, rep(1, dim(Cm)[1] - 1)))))
# Output:
  lista <- list(copula = BCopula, du = BC.du, du.inv = BC.du.inv, dv = BC.dv, dv.inv = BC.dv.inv,
                density = Bdensity, bilinearCopula = C.bilineal, bilinearSubcopula = Cm, sample.size = n, 
                order = m, both.cont = both.cont, tolerance = tolimit, subcopemObject = SC)
  return(lista)
  } # cierre de else inicial
}
