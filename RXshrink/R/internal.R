"kofm1" <- function (muobj, dMSE, delmax = 0.999999) 
{ 
    kM <- 1/dMSE 
    if (muobj <= 0) {
        d <- delmax         # d strictly < 1...
        kStar <- kM 
        return(list(kStar = kStar, d = d)) 
     }
    if (muobj >= 1) { 
        d <- 0 
        kStar <- 0                     # Terminus of the shrinkage path
        return(list(kStar = kStar, d = d)) 
    } 
    mVar <- mofk1(k=kM, dMSE) 
    kMin <- (1-muobj)/dMSE 
    if( muobj > mVar ) {               # Tail; Large m, small k Cases...
        d <- kMin*dMSE 
        kStar <- kMin 
        return(list(kStar = kStar, d = d)) 
    } else {                           # Locally Linear cases... 
        if( mVar < muobj ) { 
            B <- muobj - mVar 
            D <- 1 - muobj 
            kStar <- (B*D/(B+D))*kM/B 
        }
        dp <- min(delmax,kStar*dMSE) 
        return(list(kStar = kStar, d = dp)) 
    } 
} 

"meff" <-
function (meobj, p, dMSE) 
{ 
    mStar <- p - sum(dMSE)             # Optimal m-Extent
    if (meobj <= 0) { 
        d <- diag(1, p)
        meobj <- 0 
        return(list(meobj = meobj, d = d)) 
    } 
    if (meobj >= p) {
        d <- matrix(0, p, p) 
        meobj <- p
        return(list(meobj = meobj, d = d)) 
    }
    de <- rep(1, p)
    if( meobj > mStar ) {              # Larger meobj values...
        de <- dMSE * (p - meobj) / (p - mStar)
        d <- diag(de) 
        return(list(meobj = meobj, d = d)) 
    } else {                           # Smaller meobj values...
        de <- dMSE + (rep(1,p) - dMSE) * (mStar - meobj) / mStar
        d <- diag(de) 
        return(list(meobj = meobj, d = d)) 
    } 
}

"mstep" <- function (mobj, kinc, p, qp, eqm1) 
{ 
    if (mobj <= 0) { 
        d <- diag(p) 
        kinc <- 0 
        return(list(kinc = kinc, d = d)) 
    } 
    if (mobj >= p) { 
        d <- matrix(0, p, p) 
        kinc <- Inf 
        return(list(kinc = kinc, d = d)) 
    } 
    funs <- mobj - p 
    if (qp == 1) 
        kinc <- (-1 * mobj)/funs 
    else { 
        while (abs(funs) > 1e-05) { 
            funs <- mobj - p + sum((1 + kinc * eqm1)^-1) 
            derivs <- sum(eqm1/(1 + kinc * eqm1)^2) 
            kinc <- kinc + funs/derivs 
        } 
    } 
    d <- diag(as.vector(1/(1 + kinc * eqm1)), p) 
    iter <- list(kinc = kinc, d = d) 
    iter 
} 

"mofk1" <- function(k, dMSE)  # Many-to-One Function for large k-values...
{ 
  if( k < 0 ) k <- 0 
  kM <- 1/dMSE 
  if( k > kM ) k <- kM 
  m <- 1 - min(1,k*dMSE) 
  m 
} 
  
"mapp" <- function(mVal, YXobj)  # Approximate index on YonX() lattice of m = mVal...
{ 
  steps <- length(YXobj$spat)-1
  m <- 1  
  if (mVal <= 0) return(m)
  m <- steps+1
  if (mVal >= 1) return(m)
  m <- round(mVal*steps,0)+1
  m 
}

"B19" <- 
function(x) {
  if (x < 1.5) return(1)
  if (x > 8.5) return(9)
  if (x < 2.5) return(2)
  if (x > 7.5) return(8)
  if (x < 3.5) return(3)
  if (x > 6.5) return(7)
  if (x < 4.5) return(4)
  if (x > 5.5) return(6)
  5
}
