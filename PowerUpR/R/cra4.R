mdes.cra4r4 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                        rho2, rho3, rho4, p=.50, r21=0, r22=0, r23=0, r24=0, g4=0,
                        n, J, K, L){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- L - g4 - 2
  SSE <- sqrt(rho4*(1-r24)/(p*(1-p)*L) +
                rho3*(1-r23)/(p*(1-p)*K*L) +
                rho2*(1-r22)/(p*(1-p)*J*K*L) +
               (1-rho4-rho3-rho2)*(1-r21)/(p*(1-p)*J*K*L*n))

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.cra4r4",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, rho3=rho3, rho4=rho4,
                                p=p, r21=r21, r22=r22, r23=r23, r24=r24, g4=g4,
                                df=df,
                                n=n, J=J, K=K, L=L),
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}
# example
# mdes.cra4r4(rho4=.05, rho3=.05, rho2=.10, n=10, J=2, K=3, L=20,  two.tailed = FALSE, r23 = .80)
mdes.cra4 <- mdes.cra4r4

power.cra4r4 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                         rho2, rho3, rho4, p=.50, r21=0, r22=0, r23=0, r24=0, g4=0,
                         n, J, K, L){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- L - g4 - 2
  SSE <- sqrt(rho4*(1-r24)/(p*(1-p)*L) +
                     rho3*(1-r23)/(p*(1-p)*K*L) +
                     rho2*(1-r22)/(p*(1-p)*J*K*L) +
                    (1-rho4-rho3-rho2)*(1-r21)/(p*(1-p)*J*K*L*n))

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.cra4r4",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                  rho2=rho2, rho3=rho3, rho4=rho4,
                                  p=p, r21=r21, r22=r22, r23=r23, r24=r24, g4=g4,
                                  n=n, J=J, K=K, L=L),
                     df=df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# example
# power.cra4r4(rho4=.05, rho3=.05, rho2=.10, n=10, J=2, K=3, L=20)
power.cra4 <- power.cra4r4

mrss.cra4r4 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                        n, J, K, L0=10, tol=.10,
                        rho2, rho3, rho4, p=.50, r21=0, r22=0, r23=0, r24=0, g4=0){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    df <- L0-g4-2
    if(df<= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    L1 <- (M/es)^2 * (rho4*(1-r24)/(p*(1-p)) +
                          rho3*(1-r23)/(p*(1-p)*K) +
                          rho2*(1-r22)/(p*(1-p)*K*J) +
                          (1-rho4-rho3-rho2)*(1-r21)/(p*(1-p)*J*K*n))
    if(abs(L1-L0)<tol){conv <- TRUE}
    L0 <- (L1+L0)/2
    i <- i+1
  }
  L <- ifelse(df>0,round(L0),NA)

  mrss.out <-  list(fun = "mrss.cra4r4",
                    parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                 n=n, J=J, K=K, L0=L0, tol=tol,
                                 rho2=rho2, rho3=rho3, rho4=rho4,
                                 p=p, r21=r21, r22=r22, r23=r23, r24=r24, g4=g4),
                    df=df,
                    ncp = M,
                    L = L)
  class(mrss.out) <- c("main", "mrss")
  cat("L =", L, "\n")
  return(invisible(mrss.out))
}
# example
# mrss.cra4r4(rho4=.05, rho3=.05, rho2=.10, n=10, J=2, K=3)
mrss.cra4 <- mrss.cra4r4
