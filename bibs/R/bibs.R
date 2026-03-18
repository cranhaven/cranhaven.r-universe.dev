referencebs <-function(x, CI = 0.95, M0 = 800, M = 1000)
{
  t <- x[x != 0]
  n <- length(t)
  u <- rep(NA, n)
  von <- rep(NA, n)
  anderson <- rep(NA, n)
  sort.t   <- sort(t)
  s.x <- sum(t)
  s.rx <- sum(1/t)
  rb <- rep(NA, M)
  ra <- rep(NA, M)
  rb[1] <- sqrt(s.x/s.rx)
  ra[1] <- sqrt(s.x/(n*rb[1]) + s.rx*rb[1]/n - 2)
  I2 <- matrix( NA, nrow = 2, ncol = 2)
  h <- function(a) a*sqrt(pi/2) - pi*exp( 2/(a^2) )*( 1 - pnorm(2/a) )
  h1 <- function(a) (a/2)*sqrt(pi/2)*( 1 + (a^2)/4 - 3*(a^4)/16 + 5*(a^6)/64 )
  pdf0 <- function(par, x)
  {
    a = par[1]; b = par[2]; 1/(sqrt(2*pi*b)*a*x^0.5)*(1 - 0.5 + b*0.5/x)*exp( -(x - b)^2/( 2*a^2*b*x^(2*0.5) ) )
  }
  cdf0 <- function(par, x)
  {
    a = par[1]; b = par[2]; pnorm( 1/a*( x^(1 - 0.5)/sqrt(b) - sqrt(b)/x^0.5 ) )
  }
  for (r in 1:M)
  {
    s.z <- sum( rbinom(n, 1, rep(0.5, n) ) )# 1/(1+rb[r]/x)
    rb[r + 1] <- rgig( 1, lambda = abs(n/2 - s.z), chi = s.x/ra[r]^2, psi = s.rx/ra[r]^2)
    ra[r + 1] <- 1/sqrt( rgamma(1, shape = n/2, rate = ( s.x/(2*rb[r + 1]) + rb[r + 1]*s.rx/2 - n) ) )
  }
  out.a  <- c(mean(ra[M0:M]),  median(ra[M0:M]),  sd(ra[M0:M]),  quantile(ra[M0:M],  (1 - CI)/2), quantile(ra[M0:M],  1/2 + CI/2) )
  out.b  <- c(mean(rb[M0:M]),  median(rb[M0:M]),  sd(rb[M0:M]),  quantile(rb[M0:M],  (1 - CI)/2), quantile(rb[M0:M],  1/2 + CI/2) )
  out1   <- rbind(out.a, out.b)
  a.hat <- out1[1, 1]
  b.hat <- out1[2, 1]
  h.alpha <- function(x) ifelse( x < 0.3, h1(x), h(x) )
  I2[1, 1] <- 3*n/a.hat^2
  I2[1, 2] <- 0
  I2[2, 1] <- 0
  I2[2, 2] <- n*( 1 + a.hat/sqrt(2*pi)*h.alpha(a.hat) )/ (a.hat^2*b.hat^2) + n/b.hat^2
  S.I2 <- solve( matrix( c(I2[1, 1], I2[1, 2], I2[2, 1], I2[2, 2]), nrow = 2, ncol = 2) )
  for(i in 1:n)
  {
    u[i] <- ifelse( cdf0( out1[, 1], sort.t[i] ) == 1, 0.99999999, cdf0( out1[, 1], sort.t[i] ) )
    von[i] <- ( cdf0( out1[, 1], sort.t[i] ) - (2*i - 1)/(2*n) )^2
    anderson[i] <- (2*i - 1)*log( cdf0( out1[, 1], sort.t[i] ) ) + (2*n + 1 - 2*i)*log( 1 - cdf0( out1[, 1], sort.t[i] ) )
  }
  log.likelihood <- suppressWarnings( sum( log( pdf0( out1[, 1], t) ) ) )
  n.p <- 2
  Anderson.stat <- suppressWarnings(-n - mean( anderson ) )
  Von.stat <- suppressWarnings( sum(von) + 1/(12*n) )
  AIC<- -2*log.likelihood + 2*n.p
  BIC<- -2*log.likelihood + n.p*log(n)
  out.ks <- suppressWarnings( ks.test( t, "cdf0", par = out1[, 1] ) )
  KS.stat <- out.ks[[1]][[1]]
  p.value <- out.ks[[2]]
  out2 <- cbind(AIC, BIC, Anderson.stat, Von.stat, KS.stat, p.value, log.likelihood)
  out3 <- cbind(sqrt( diag(S.I2) ), c(out1[, 1]) + sqrt( diag(S.I2) )*qnorm( (1 - CI)/2),
                c(out1[, 1]) + sqrt( diag(S.I2) )*qnorm(1 - (1 - CI)/2) )
  colnames(out1) <- c("Mean", "Median", "Std. Error", "Lower bound", "Upper bound")
  rownames(out1) <- rbind("alpha", "beta")
  colnames(out2)<-c("AIC", "BIC", "AD", "CVM", "KS", "p-value", "log.likelihood")
  colnames(out3) <- c("Std. Error", "Lower bound", "Upper bound")
  rownames(out3) <- c("alpha", "beta")
  rownames(I2) <- c("alpha", "beta")
  colnames(I2) <- c("alpha", "beta")
list("Estimates" = out1, "Measures" = out2 , "Asymptotic inference" = out3, "Fisher information matrix" = I2)
}

conjugatebs <-function(x, gamma0 = 1, theta0 = 1, lambda0 = 0.001, chi0 = 0.001, psi0 = 0.001, CI = 0.95, M0 = 800, M = 1000)
{
  t <- x[x != 0]
  n  <- length(t)
  u  <- rep(NA, n)
  von <- rep(NA, n)
  anderson <- rep(NA, n)
  sort.t   <- sort(t)
  s.x <- sum(t)
  s.rx <- sum(1/t)
  rb <- rep(NA, M + 1)
  ra <- rep(NA, M + 1)
  rb[1] <- sqrt(s.x/s.rx)
  ra[1] <- sqrt(s.x/(n*rb[1]) + s.rx*rb[1]/n - 2)
  I1 <- matrix( NA, nrow = 2, ncol = 2)
  h <- function(a) a*sqrt(pi/2) - pi*exp( 2/(a^2) )*( 1 - pnorm(2/a) )
  h1 <- function(a) (a/2)*sqrt(pi/2)*( 1 + (a^2)/4 - 3*(a^4)/16 + 5*(a^6)/64 )
  pdf0 <- function(par, x)
  {
    a = par[1]; b = par[2]; 1/(sqrt(2*pi*b)*a*x^0.5)*(1 - 0.5 + b*0.5/x)*exp( -(x - b)^2/( 2*a^2*b*x^(2*0.5) ) )
  }
  cdf0 <- function(par, x)
  {
    a = par[1]; b = par[2]; pnorm( 1/a*( x^(1 - 0.5)/sqrt(b) - sqrt(b)/x^0.5 ) )
  }
  for (r in 1:M)
  {
    s.z <- sum( rbinom(n, 1, rep(.5, n) ) )#1/(1+rb[r]/x)
    rb[r + 1] <- rgig( 1, lambda = n/2 - s.z + lambda0, chi = s.x/ra[r]^2 + chi0, psi = s.rx/ra[r]^2 + psi0)
    ra[r + 1] <- 1/sqrt( rgamma(1, shape =  n/2 + gamma0, rate = ( s.x/(2*rb[r + 1]) + rb[r + 1]*s.rx/2 - n + theta0) ) )
  }
  out.a <- c(mean(ra[M0:M]),  median(ra[M0:M]),  sd(ra[M0:M]),  quantile(ra[M0:M],  (1 - CI)/2), quantile(ra[M0:M],  1/2 + CI/2) )
  out.b <- c(mean(rb[M0:M]),  median(rb[M0:M]),  sd(rb[M0:M]),  quantile(rb[M0:M],  (1 - CI)/2), quantile(rb[M0:M],  1/2 + CI/2) )
  out1  <- rbind(out.a, out.b)
  a.hat <- out1[1, 1]
  b.hat <- out1[2, 1]
  h.alpha <- function(x) ifelse( x < 0.3, h1(x), h(x) )
  I1[1, 1] <- 2*n/a.hat^2 + 2*n*(gamma0 + 1)/a.hat^2 - 6*n*theta0/a.hat^4
  I1[1, 2] <- 0
  I1[2, 1] <- 0
  I1[2, 2] <- n*( 1 + a.hat/sqrt(2*pi)*h.alpha(a.hat) )/ (a.hat^2*b.hat^2) - n*(lambda0 - 1)/b.hat^2 - n*chi0/b.hat^3
  S.I1 <- solve( matrix( c(I1[1, 1], I1[1, 2], I1[2, 1], I1[2, 2]), nrow = 2, ncol = 2) )
  for(i in 1:n)
  {
    u[i] <- ifelse( cdf0( out1[, 1], sort.t[i] ) == 1, 0.99999999, cdf0( out1[, 1], sort.t[i] ) )
    von[i] <- ( cdf0( out1[, 1], sort.t[i] ) - (2*i - 1)/(2*n) )^2
    anderson[i] <- (2*i - 1)*log( cdf0( out1[, 1], sort.t[i] ) ) + (2*n + 1 - 2*i)*log( 1 - cdf0( out1[, 1], sort.t[i] ) )
  }
  log.likelihood <- suppressWarnings( sum( log( pdf0( out1[, 1], t) ) ) )
  n.p <- 2
  Anderson.stat <- suppressWarnings(-n - mean( anderson ) )
  Von.stat <- suppressWarnings( sum(von) + 1/(12*n) )
  AIC<- -2*log.likelihood + 2*n.p
  BIC<- -2*log.likelihood + n.p*log(n)
  out.ks <- suppressWarnings( ks.test( t, "cdf0", par = out1[, 1] ) )
  KS.stat <- out.ks[[1]][[1]]
  p.value <- out.ks[[2]]
  out2 <- cbind(AIC, BIC, Anderson.stat, Von.stat, KS.stat, p.value, log.likelihood)
  out3 <- cbind(sqrt( diag(S.I1) ), c(out1[, 1]) + sqrt( diag(S.I1) )*qnorm( (1 - CI)/2),
          c(out1[, 1]) + sqrt( diag(S.I1) )*qnorm(1 - (1 - CI)/2) )
  colnames(out1) <- c("Mean", "Median", "Std. Error", "Lower bound", "Upper bound")
  rownames(out1) <- c("alpha", "beta")
  colnames(out2)<-c("AIC", "BIC", "AD", "CVM", "KS", "p-value", "log.likelihood")
  colnames(out3) <- c("Std. Error", "Lower bound", "Upper bound")
  rownames(out3) <- c("alpha", "beta")
  rownames(I1) <- c("alpha", "beta")
  colnames(I1) <- c("alpha", "beta")
list("Estimates" = out1, "Measures" = out2 , "Asymptotic inference" = out3, "Fisher information matrix" = I1)
}

mlebs <- function(x, start, method = "Nelder-Mead", CI = 0.95)
{
  t <- x[x != 0]
  n  <- length(t)
  u  <- rep(NA, n)
  von <- rep(NA, n)
  anderson <- rep(NA, n)
  sort.t   <- sort(t)
  param <- c("alpha", "beta")
  if( length(start) != 2 ) stop("The length of initial values must be 2.")
  I0 <- matrix( NA, nrow = 2, ncol = 2)
  h <- function(a) a*sqrt(pi/2) - pi*exp( 2/(a^2) )*( 1 - pnorm(2/a) )
  h1 <- function(a) (a/2)*sqrt(pi/2)*( 1 + (a^2)/4 - 3*(a^4)/16 + 5*(a^6)/64 )
  pdf0 <- function(par, x)
  {
    a = par[1]; b = par[2]; 1/(sqrt(2*pi*b)*a*x^0.5)*(1 - 0.5 + b*0.5/x)*exp( -(x - b)^2/( 2*a^2*b*x^(2*0.5) ) )
  }
  cdf0 <- function(par, x)
  {
    a = par[1]; b = par[2]; pnorm( 1/a*( x^(1 - 0.5)/sqrt(b) - sqrt(b)/x^0.5 ) )
  }
  pdf <- quote( ( t/2 + beta/2 )/( sqrt(2*pi*beta)*alpha*t^(3/2) )*exp( -(t - beta)^2/(2*alpha^2*beta*t) )  )
  f  <- function(x, par)
  {
    for(i in 1:2) assign(param[i], par[i])
    -sum( log( (eval(pdf)) ) )
  }
  out <- suppressWarnings( optim( start, fn = f, x = t, method = method )$par )
  out1 <- cbind(out[1], out[2])
  a.hat <- out1[1]
  b.hat <- out1[2]
  h.alpha <- function(x) ifelse( x < 0.3, h1(x), h(x) )
  I0[1, 1] <- 2*n/a.hat^2
  I0[1, 2] <- 0
  I0[2, 1] <- 0
  I0[2, 2] <- n*( 1 + a.hat/sqrt(2*pi)*h.alpha(a.hat) )/ (a.hat^2*b.hat^2)
  S.I0 <- solve( matrix( c(I0[1, 1], I0[1, 2], I0[2, 1], I0[2, 2]), nrow = 2, ncol = 2) )
  for(i in 1:n)
  {
    u[i] <- ifelse( cdf0( out1, sort.t[i] ) == 1, 0.99999999, cdf0( out1, sort.t[i] ) )
    von[i] <- ( cdf0( out1, sort.t[i] ) - (2*i - 1)/(2*n) )^2
    anderson[i] <- (2*i - 1)*log( cdf0( out1, sort.t[i] ) ) + (2*n + 1 - 2*i)*log( 1 - cdf0( out1, sort.t[i] ) )
  }
  log.likelihood <- suppressWarnings( sum( log( pdf0( out1, t) ) ) )
  n.p <- 2
  Anderson.stat <- suppressWarnings(-n - mean( anderson ) )
  Von.stat <- suppressWarnings( sum(von) + 1/(12*n) )
  AIC<- -2*log.likelihood + 2*n.p
  BIC<- -2*log.likelihood + n.p*log(n)
  out.ks <- suppressWarnings( ks.test( t, "cdf0", par = out1 ) )
  KS.stat <- out.ks[[1]][[1]]
  p.value <- out.ks[[2]]
  out2 <- cbind(AIC, BIC, Anderson.stat, Von.stat, KS.stat, p.value, log.likelihood)
  out3 <- cbind(sqrt( diag(S.I0) ), c(out1) + sqrt( diag(S.I0) )*qnorm( (1 - CI)/2),
                c(out1) + sqrt( diag(S.I0) )*qnorm(1 - (1 - CI)/2) )
  colnames(out1) <- c("alpha", "beta")
  colnames(out2)<-c("AIC", "BIC", "AD", "CVM", "KS", "p-value", "log.likelihood")
  colnames(out3) <- c("std. error", "lower bound", "upper bound")
  rownames(out3) <- c("alpha", "beta")
  rownames(I0) <- c("alpha", "beta")
  colnames(I0) <- c("alpha", "beta")
list("Estimates" = out1, "Measures" = out2 , "Asymptotic inference" = out3, "Fisher information matrix" = I0)
}

Jeffreysbs <-function(x, CI = 0.95, M0 = 800, M = 1000)
{
  t <- x[x != 0]
  n <- length(t)
  u <- rep(NA, n)
  von <- rep(NA, n)
  anderson <- rep(NA, n)
  sort.t   <- sort(t)
  s.x <- sum(t)
  s.rx <- sum(1/t)
  rb <- rep(NA, M)
  ra <- rep(NA, M)
  rb[1] <- sqrt(s.x/s.rx)
  ra[1] <- sqrt(s.x/(n*rb[1]) + s.rx*rb[1]/n - 2)
  I2 <- matrix( NA, nrow = 2, ncol = 2)
  h <- function(a) a*sqrt(pi/2) - pi*exp( 2/(a^2) )*( 1 - pnorm(2/a) )
  h1 <- function(a) (a/2)*sqrt(pi/2)*( 1 + (a^2)/4 - 3*(a^4)/16 + 5*(a^6)/64 )
  pdf0 <- function(par, x)
  {
    a = par[1]; b = par[2]; 1/(sqrt(2*pi*b)*a*x^0.5)*(1 - 0.5 + b*0.5/x)*exp( -(x - b)^2/( 2*a^2*b*x^(2*0.5) ) )
  }
  cdf0 <- function(par, x)
  {
    a = par[1]; b = par[2]; pnorm( 1/a*( x^(1 - 0.5)/sqrt(b) - sqrt(b)/x^0.5 ) )
  }
  for (r in 1:M)
  {
    s.z <- sum( rbinom(n, 1, rep(0.5, n) ) )# 1/(1+rb[r]/x)
    rb[r + 1] <- rgig( 1, lambda = abs(n/2 - s.z), chi = s.x/ra[r]^2, psi = s.rx/ra[r]^2)
    j <- 1
    k <- ( s.x/(2*rb[r + 1]) + rb[r + 1]*s.rx/2 - n)
    while(j < 2)
    {
      #u <- rbinom(1, 1, 1/(1 + (n/2 + 2)/(n/2 + 1) ))
      u <- rbinom(1, 1, 1/(1 + (2*k)/n ))
      r.candid <- ifelse(u == 1, 1/sqrt( rgamma(1, shape = n/2, rate = k ) ),
                         1/sqrt( rgamma(1, shape = (n - 1)/2, rate = k ) ) )
      Upper <- sqrt( (1 + r.candid/4) )/(1 + sqrt(r.candid)/2)
      if( runif(1) < Upper)
      {
        ra[r + 1] <- r.candid
        j <- j + 1
      }
    }
  }
  out.a  <- c(mean(ra[M0:M]),  median(ra[M0:M]),  sd(ra[M0:M]),  quantile(ra[M0:M],  (1 - CI)/2), quantile(ra[M0:M],  1/2 + CI/2) )
  out.b  <- c(mean(rb[M0:M]),  median(rb[M0:M]),  sd(rb[M0:M]),  quantile(rb[M0:M],  (1 - CI)/2), quantile(rb[M0:M],  1/2 + CI/2) )
  out1   <- rbind(out.a, out.b)
  a.hat <- out1[1, 1]
  b.hat <- out1[2, 1]
  h.alpha <- function(x) ifelse( x < 0.3, h1(x), h(x) )
  I2[1, 1] <- 2*n/a.hat^2 + n*(32 + 20*a.hat^2 + a.hat^4)/( a.hat^2*(4 + a.hat^2)^2 )
  I2[1, 2] <- 0
  I2[2, 1] <- 0
  I2[2, 2] <- n*( 1 + a.hat/sqrt(2*pi)*h.alpha(a.hat) )/ (a.hat^2*b.hat^2) + n/b.hat^2
  S.I2 <- solve( matrix( c(I2[1, 1], I2[1, 2], I2[2, 1], I2[2, 2]), nrow = 2, ncol = 2) )
  for(i in 1:n)
  {
    u[i] <- ifelse( cdf0( out1[, 1], sort.t[i] ) == 1, 0.99999999, cdf0( out1[, 1], sort.t[i] ) )
    von[i] <- ( cdf0( out1[, 1], sort.t[i] ) - (2*i - 1)/(2*n) )^2
    anderson[i] <- (2*i - 1)*log( cdf0( out1[, 1], sort.t[i] ) ) + (2*n + 1 - 2*i)*log( 1 - cdf0( out1[, 1], sort.t[i] ) )
  }
  log.likelihood <- suppressWarnings( sum( log( pdf0( out1[, 1], t) ) ) )
  n.p <- 2
  Anderson.stat <- suppressWarnings(-n - mean( anderson ) )
  Von.stat <- suppressWarnings( sum(von) + 1/(12*n) )
  AIC<- -2*log.likelihood + 2*n.p
  BIC<- -2*log.likelihood + n.p*log(n)
  out.ks <- suppressWarnings( ks.test( t, "cdf0", par = out1[, 1] ) )
  KS.stat <- out.ks[[1]][[1]]
  p.value <- out.ks[[2]]
  out2 <- cbind(AIC, BIC, Anderson.stat, Von.stat, KS.stat, p.value, log.likelihood)
  out3 <- cbind(sqrt( diag(S.I2) ), c(out1[, 1]) + sqrt( diag(S.I2) )*qnorm( (1 - CI)/2),
                c(out1[, 1]) + sqrt( diag(S.I2) )*qnorm(1 - (1 - CI)/2) )
  colnames(out1) <- c("Mean", "Median", "Std. Error", "Lower bound", "Upper bound")
  rownames(out1) <- rbind("alpha", "beta")
  colnames(out2) <-c("AIC", "BIC", "AD", "CVM", "KS", "p-value", "log.likelihood")
  colnames(out3) <- c("Std. Error", "Lower bound", "Upper bound")
  rownames(out3) <- c("alpha", "beta")
  rownames(I2) <- c("alpha", "beta")
  colnames(I2) <- c("alpha", "beta")
  list("Estimates" = out1, "Measures" = out2 , "Asymptotic inference" = out3, "Fisher information matrix" = I2)
}

typeIIbs <- function(plan, M0 = 4000, M = 6000, CI = 0.95)
{
  n.rep <- 200
  x <- plan$X
  m <- length(x)
  r <- plan$R
  n <- sum(r) + length(r)
  s.w <- sum(x)
  rs.w <- sum(1/x)
  beta.old  <- rep(NA, n.rep)
  alpha.old <- rep(NA, n.rep)
  stat_A <- stat_C <- Alpha <- rep(NA, m-1)
  rb <- rep(NA, M)
  ra <- rep(NA, M)
  ra[1] <- 1
  rb[1] <- 1
  beta.old[1]  <- rgig( 1, s.w/ra[1]^2, rs.w/ra[1]^2, 1)
  alpha.old[1] <- 1/sqrt( rgamma(1, shape = m/2, rate = ( s.w/rb[1] + rb[1]*rs.w - 2*m )/2 ) )
  ra[2] <- alpha.old[1]
  cdf0 <- function(par, x)
  {
    a = par[1]; b = par[2]; pnorm( ( sqrt(x/b) - sqrt(b/x) )/a )
  }
  for (k in 2: M)
  {
    s.z <- sum( rbinom(m, 1, rep(.5, m) ) )#1/(1+rb[r]/x)
    for (j in 2:n.rep)
    {
      beta.new <- rgig( 1, m/2 - s.z + 1, s.w/ra[k]^2, rs.w/ra[k]^2 )
      ratio <- exp( sum( r*log( pnorm( ( sqrt(x/beta.new)        - sqrt(beta.new/x)        )/ra[k], lower.tail = FALSE)/
                                  pnorm( ( sqrt(x/beta.old[j - 1]) - sqrt(beta.old[j - 1]/x) )/ra[k], lower.tail = FALSE) )  )  )
      beta.old[j] <- beta.old[j - 1]
      if ( runif(1) < ratio ) beta.old[j] <- beta.new
    }
    rb[k + 1] <- beta.old[n.rep]
    beta.old[1] <- rb[k + 1]
    for (j in 2:n.rep)
    {
      alpha.new <- 1/sqrt( rgamma(1, shape = m/2 + 1/2, rate = ( s.w/rb[k + 1] + rb[k + 1]*rs.w - 2*m )/2 ) )
      ratio <- exp( sum( r*log( pnorm( ( sqrt(x/rb[k + 1])        - sqrt(rb[k + 1]/x) )/alpha.new, lower.tail = FALSE)/
                                  pnorm( ( sqrt(x/rb[k + 1]) - sqrt(rb[k + 1]/x) )/alpha.old[j - 1], lower.tail = FALSE) )  )  )
      alpha.old[j] <- alpha.old[j - 1]
      if ( runif(1) < ratio ) alpha.old[j] <- alpha.new
    }
    ra[k + 1] <- alpha.old[n.rep]
    alpha.old[1] <- ra[k + 1]
  }
  out.a <- c(mean(ra[M0:M]), median(ra[M0:M]), sd(ra[M0:M]), quantile(ra[M0:M], (1 - CI)/2), quantile(ra[M0:M], 1/2 + CI/2) )
  out.b <- c(mean(rb[M0:M]), median(rb[M0:M]), sd(rb[M0:M]), quantile(rb[M0:M], (1 - CI)/2), quantile(rb[M0:M], 1/2 + CI/2) )
  out   <- cbind(out.a[1], out.b[1])
  out1  <- rbind(out.a, out.b)
  U     <- cdf0(out, x)
  for (i in 1:(m-1) )
  {
    Prod1 <- 1
    for ( j in (m-i+1):m ) Prod1 <- Prod1*( j+sum(r[(m-j+1):(m)]) )/( j+1+sum( r[(m-j+1):(m)] ) )
    Alpha[i] <- 1 - Prod1
    stat_A[i] <- Alpha[i]^2*log( U[i+1]*( 1-U[i] )/( U[i]*( 1-U[i+1] ) ) ) + 2*Alpha[i]*log( ( 1-U[i+1] )/( 1-U[i] ) )
    stat_C[i] <- Alpha[i]*( U[i+1]-U[i] )*( Alpha[i]-U[i+1]-U[i] )
  }
  Anderson <- n*sum(stat_A) - n*log( 1 - U[m] ) - n*U[m]
  Cramer <- n*sum(stat_C) + n/3*U[m]^3
  Prod2 <- 1
  for ( j in 1:m ) Prod2 <- Prod2*( j+sum(r[(m-j+1):(m)]) )/( j+1+sum(r[(m-j+1):(m)]) )
  KS <- max( abs( c(Alpha, 1 - Prod2) - U ) )
  out2 <- cbind(Anderson, Cramer, KS)
  colnames(out1) <- c("Mean", "Median", "Std. Error", "Lower bound", "Upper bound")
  rownames(out1) <- rbind("alpha", "beta")
  colnames(out)  <- cbind("alpha", "beta")
  colnames(out2) <- c("AD", "CVM", "KS")
list("Estimates" = out, "Asymptotic inference" = out1, "cov" = cov( cbind(ra[M0:M], rb[M0:M]) ), "Measures" = out2)
}

rbs <- function(n, alpha, beta)
{
  #if (nu > 1 | nu < 0) stop("Parameter nu must be within (0, 1)")
  nu <- 0.5
  y <- rep(NA, n)
  for(i in 1:n)
  {
    z <- rnorm(1)
    f <- function(x) beta + alpha*sqrt(beta)*z*x^nu - x
    y[i] <- uniroot(f, lower = 0, upper = 10e307)$root
  }
y
}
