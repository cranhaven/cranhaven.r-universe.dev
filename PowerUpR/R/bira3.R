mdes.bira3r1 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                        rho2, rho3, esv2=NULL, esv3=NULL,
                        omega2=esv2/rho2, omega3=esv3/rho3,
                        p=.50, r21=0, r2t2=0, r2t3=0, g3=0,
                        n, J, K){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- K - g3 - 1
  SSE <- sqrt(rho3*omega3*(1-r2t3)/K +
                rho2*omega2*(1-r2t2)/(J*K) +
               (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*K*n))

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.bira3r1",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, rho3=rho3, esv2=esv2, esv3=esv3,
                                omega2=omega2, omega3=omega3,
                                p=p, r21=r21, r2t2=r2t2, r2t3=r2t3, g3=g3,
                                n=n, J=J, K=K),
                   df=df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}
# example
# mdes.bira3r1(rho3=.20, rho2=.15, omega3=.10, omega2=.10, n=69, J=10, K=100)
mdes.bira3 <- mdes.bira3r1

power.bira3r1 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                         rho2, rho3, esv2=NULL, esv3=NULL,
                         omega2=esv2/rho2, omega3=esv3/rho3,
                         p=.50, r21=0, r2t2=0, r2t3=0, g3=0,
                         n, J, K){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- K - g3 - 1
  SSE <- sqrt(rho3*omega3*(1-r2t3)/K +
                     rho2*omega2*(1-r2t2)/(J*K) +
                    (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*K*n))

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.bira3r1",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                  rho2=rho2, rho3=rho3, esv2=esv2, esv3=esv3,
                                  omega2=omega2, omega3=omega3,
                                  p=p, r21=r21, r2t2=r2t2, r2t3=r2t3, g3=g3,
                                  n=n, J=J, K=K),
                     df=df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# example
# power.bira3r1(rho3=.20, rho2=.15, omega3=.10, omega2=.10, n=69, J=10, K=100)
power.bira3 <- power.bira3r1

mrss.bira3r1 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                         n, J, K0=10, tol=.10,
                         rho2, rho3, esv2=NULL, esv3=NULL,
                         omega2=esv2/rho2, omega3=esv3/rho3,
                         p=.50, r21=0, r2t2=0, r2t3=0, g3=0){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    df <- K0-g3-1
    if(df<= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    K1 <- (M/es)^2 * (rho3*omega3*(1-r2t3) +
                          rho2*omega2*(1-r2t2)/J +
                          (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*n))
    K0 <- (K1+K0)/2
    i <- i+1
  }
  K <- ifelse(df>0,round(K0),NA)

  mrss.out <-  list(fun = "mrss.bira3r1",
                    parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                 n=n, J=J, K0=K0, tol=tol,
                                 rho2=rho2, rho3=rho3, esv2=esv2, esv3=esv3,
                                 omega2=omega2, omega3=omega3,
                                 p=p, r21=r21, r2t2=r2t2, r2t3=r2t3, g3=g3),
                    df=df,
                    ncp = M,
                    K = K)
  class(mrss.out) <- c("main", "mrss")
  cat("K =", K, "\n")
  return(invisible(mrss.out))
}
# example
# mrss.bira3r1(rho3=.20, rho2=.15, omega3=.10, omega2=.10, n=12, J=3)
mrss.bira3 <- mrss.bira3r1


