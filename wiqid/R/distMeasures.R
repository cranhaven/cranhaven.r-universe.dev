# Functions for a range of distance measures

distBrayCurtis <-
function(d1, d2)
  1 - 2*sum(pmin(d1, d2)) / (sum(d1)+sum(d2))


distChaoJaccCorr <-
function(d1, d2) {
  n <- sum(d1) ; m <- sum(d2)
  f1. <- sum(d1==1 & d2>0)
  f2. <- max(1, sum(d1==2 & d2>0))
  f.1 <- sum(d2==1 & d1>0)
  f.2 <- max(1, sum(d2==2 & d1>0))
  U <- min(1, sum(d1[d2>0])/n + ((m-1)/m)*(f.1/(2*f.2))*sum(d1[d2==1]/n))
  V <- min(1, sum(d2[d1>0])/m + ((n-1)/n)*(f1./(2*f2.))*sum(d2[d1==1]/m))
  1 - U*V / (U+V-U*V)
}


distChaoJaccNaive <-
function(d1, d2) {
  shared <- d1 & d2   # which sps are shared
  U <- sum(d1[shared])/sum(d1) ; V <- sum(d2[shared])/sum(d2)
  1 - U*V / (U+V-U*V)
}


distChaoSorCorr <-
function(d1, d2) {
  n <- sum(d1) ; m <- sum(d2)
  f1. <- sum(d1==1 & d2>0)
  f2. <- max(1, sum(d1==2 & d2>0))
  f.1 <- sum(d2==1 & d1>0)
  f.2 <- max(1, sum(d2==2 & d1>0))
  U <- min(1, sum(d1[d2>0])/n + ((m-1)/m)*(f.1/(2*f.2))*sum(d1[d2==1]/n))
  V <- min(1, sum(d2[d1>0])/m + ((n-1)/n)*(f1./(2*f2.))*sum(d2[d1==1]/m))
  1 - 2*U*V / (U+V)
}


distChaoSorNaive <-
function(d1, d2) {
  shared <- d1 & d2   # which sps are shared
  U <- sum(d1[shared])/sum(d1) ; V <- sum(d2[shared])/sum(d2)
  1 - 2*U*V / (U+V)
}


distChord <-
function(d1, d2) {
  s1 <- d1/sqrt(sum(d1^2))
  s2 <- d2/sqrt(sum(d2^2))
  sqrt(sum((s1 - s2)^2))
}


distJaccard <-
function(d1, d2) {
  shared <- sum(d1 & d2)
  1 - shared / (sum(d1>0)+sum(d2>0)-shared)
}


distMatching <-
function(d1, d2) {
  same <- sum((d1 > 0) == (d2 > 0))
  1 - same / length(d1)
}


distMorisitaHorn <-
function(d1, d2) {
  p1 <- d1 / sum(d1)
  p2 <- d2 / sum(d2)
  1 - 2 * sum(p1*p2) / ( sum(p1^2)+sum(p2^2) )
}


distOchiai <-
function(d1, d2) {
  shared <- sum(d1 & d2)
  1 - shared / sqrt(sum(d1 > 0) * sum(d2 > 0))
}


distPreston <-
function(d1, d2) {
  total <- sum(d1 | d2)
  f <- function(n, x, y) abs(x^n+y^n-1)
  1/nlm(f, 2, x=sum(d1 > 0) / total, y=sum(d2 > 0) / total)$estimate
}


distRogersTanimoto <-
function(d1, d2) {
  same <- sum((d1 > 0) == (d2 > 0))
  different <- sum((d1 > 0) != (d2 > 0))
  1 - same / (length(d1) + different)
}


distSimRatio <-
function(d1, d2) {
  1 - sum(d1 * d2) / 
   (sum(d1^2) + sum(d2^2) - sum(d1 * d2))
}


distSorensen <-
function(d1, d2) {
  shared <- sum(d1 & d2)
  1 - 2*shared / (sum(d1>0)+sum(d2>0))
}


distWhittaker <-
function(d1, d2)  {
  p1 <- d1 / sum(d1)
  p2 <- d2 / sum(d2)
  sum(abs(p1 - p2))/2
}

