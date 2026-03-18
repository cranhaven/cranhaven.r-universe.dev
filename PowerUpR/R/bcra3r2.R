mdes.bcra3r2 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                        rho2, rho3, esv3=NULL, omega3=esv3/rho3,
                        p=.50, g3=0, r21=0, r22=0, r2t3=0,
                        n, J, K){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- K - g3 - 1
  SSE <- sqrt(rho3*omega3*(1-r2t3)/K +
                rho2*(1-r22)/(p*(1-p)*J*K) +
               (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*K*n))

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.bcra3r2",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, rho3=rho3,
                                esv3=esv3, omega3=omega3,
                                p=p, r21=r21, r22=r22, r2t3=r2t3, g3=g3,
                                n=n, J=J, K=K),
                   df=df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}
# example
# mdes.bcra3r2(rho3=.13, rho2=.10, omega3=.4, n=10, J=6, K=24)

power.bcra3r2 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                         rho2, rho3, esv3=NULL, omega3=esv3/rho3,
                         p=.50, g3=0, r21=0, r22=0, r2t3=0,
                         n, J, K){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- K - g3 - 1
  SSE <- sqrt(rho3*omega3*(1-r2t3)/K +
                     rho2*(1-r22)/(p*(1-p)*J*K) +
                    (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*K*n))

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.bcra3r2",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                  rho2=rho2, rho3=rho3,
                                  esv3=esv3, omega3=omega3,
                                  p=p, r21=r21, r22=r22, r2t3=r2t3, g3=g3,
                                  df=df,
                                  n=n, J=J, K=K),
                     df=df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# example
# power.bcra3r2(rho3=.13, rho2=.10, omega3=.4, n=10, J=6, K=24)

mrss.bcra3r2 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                         n, J, K0=10, tol=.10,
                         rho2, rho3, esv3=NULL, omega3=esv3/rho3,
                         p=.50, g3=0, r21=0, r22=0, r2t3=0){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    df = K0-g3-1
    if(df<= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 = abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    K1 <- (M/es)^2 * (rho3*omega3*(1-r2t3) +
                          rho2*(1-r22)/(p*(1-p)*J) +
                          (1-rho3-rho2)*(1-r21)/(p*(1-p)*J*n))
    K0 <- (K1+K0)/2
    i <- i+1
  }
  K <- ifelse(df>0,round(K0),NA)

  mrss.out <-  list(fun = "mrss.bcra3r2",
                    parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                 n=n, J=J, K0=K0, tol=tol,
                                 rho2=rho2, rho3=rho3,
                                 esv3=esv3, omega3=omega3,
                                 p=p, r21=r21, r22=r22, r2t3=r2t3, g3=g3),
                    df=df,
                    ncp = M,
                    K = K)
  class(mrss.out) <- c("main", "mrss")
  cat("K =", K, "\n")
  return(invisible(mrss.out))
}
# example
# mrss.bcra3r2(rho3=.13, rho2=.10, omega3=.4, n=10, J=6)

mdes.bcra3r2_pn <- function(power=.80, alpha=.05, two.tailed=TRUE, df=NULL,
                            rho3_trt=.10, omega3=.50, rho2_trt=.20, rho_ic=0,
                            p=.50, r21=0, g3=0, n, J, K, ic_size=1){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(ic_size == 1 & rho_ic != 0) {
    rho_ic <- 0
    warning("Forcing 'rho_ic = 0'", call. = FALSE)
  } else  if(ic_size > 1 & rho_ic == 0) {
    warning("'rho_ic = 0'?", call. = FALSE)
  }

  # needs Satterthwaite (1946) degrees of freedom
  if(is.null(df)) {
    df <- K - g3 - 1
  }

  deff_rand_ic <- 1 + ((rho3_trt * omega3 * J * n * p * (1 - p) - rho3_trt) + rho2_trt * (n - 1) + rho_ic * (1 - p) * (ic_size - 1)) / (1 - p * rho_ic)
  SSE <- sqrt(((1 - r21) / (K * J * n * p * (1 - p))) * ((1 - p * rho_ic) / (1 - rho_ic)) * deff_rand_ic)

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE,
                               df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE,
                        df = round(df,3), two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.bcra3r2_pn",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho3_trt=rho3_trt, omega3=omega3, rho2_trt=rho2_trt, rho_ic = rho_ic,
                                r21=r21, p=p, n=n, J=J, K=K, ic_size=ic_size),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}
# example
# mdes.bcra3r2_pn(rho3_trt = .10, omega3 = .50, rho2_trt = .15, rho_ic = .20,
#                 n = 40, J = 60, K = 6, ic_size = 10)

power.bcra3r2_pn <- function(es=.25,alpha=.05, two.tailed=TRUE, df=NULL,
                             rho3_trt=.10, omega3=.50, rho2_trt=.20, rho_ic=0,
                             p=.50, r21=0, g3=0, n, J, K, ic_size=1){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(ic_size == 1 & rho_ic != 0) {
    rho_ic <- 0
    warning("Forcing 'rho_ic = 0'", call. = FALSE)
  } else  if(ic_size > 1 & rho_ic == 0) {
    warning("'rho_ic = 0'?", call. = FALSE)
  }

  # needs Satterthwaite (1946) degrees of freedom
  if(is.null(df)) {
    df <- K - g3 - 1
  }

  deff_rand_ic <- 1 + ((rho3_trt * omega3 * J * n * p * (1 - p) - rho3_trt) + rho2_trt * (n - 1) + rho_ic * (1 - p) * (ic_size - 1)) / (1 - p * rho_ic)
  SSE <- sqrt(((1 - r21) / (K * J * n * p * (1 - p))) * ((1 - p * rho_ic) / (1 - rho_ic)) * deff_rand_ic)

  power <- .power.fun(es = es, alpha = alpha, sse = SSE,
                                 df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE,
                         df = round(df,3), two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.bcra3r2_pn",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                  rho3_trt=rho3_trt, omega3=omega3, rho2_trt=rho2_trt, rho_ic=rho_ic,
                                  r21=r21, p=p, n=n, J=J, K=K, ic_size=ic_size),
                     df = df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# example
# power.bcra3r2_pn(es=.399, rho3_trt = .10, omega3 = .50, rho2_trt = .15, rho_ic = .20,
#                  n = 40, J = 60, K = 6, ic_size = 10)

mrss.bcra3r2_pn  <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE, z.test=FALSE,
                             rho3_trt=.10, omega3 = .50, rho2_trt=.20, rho_ic=0,
                             p=.50, r21=0, g3=0, n, J, ic_size=1, K0=10, tol=.10){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(ic_size == 1 & rho_ic != 0) {
    rho_ic <- 0
    warning("Forcing 'rho_ic = 0'", call. = FALSE)
  } else  if(ic_size > 1 & rho_ic == 0) {
    warning("'rho_ic = 0'?", call. = FALSE)
  }

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){

    # needs Satterthwaite (1946) degrees of freedom
    df <- K0 - g3 - 1

    if(df <= 0) stop("Increase 'K0'", call. = FALSE)
    if(df <= 0 | is.infinite(df)){break}

    if(z.test) df <- Inf

    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)

    deff_rand_ic <- 1 + ((rho3_trt * omega3 * J * n * p * (1 - p) - rho3_trt) + rho2_trt * (n - 1) + rho_ic * (1 - p) * (ic_size - 1)) / (1 - p * rho_ic)
    VAR <- ((1 - r21) / (J * n * p * (1 - p))) * ((1 - p * rho_ic) / (1 - rho_ic)) * deff_rand_ic

    K1 <- (M/es)^2 * VAR
    if(abs(K1-K0)<tol){conv <- TRUE}
    K0 <- (K1+K0)/2
    i <- i+1
  }

  K <- round(ifelse(df>0,round(K0),NA))

  K.out <-  list(fun = "mrss.bcra3r2_pn",
                 parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                              rho3_trt=rho3_trt, omega3=omega3, rho2_trt=rho2_trt, rho_ic=rho_ic,
                              r21=r21, g3=g3, p=p, n=n, J=J, ic_size=ic_size, K0=K0, tol=tol),
                 df=df,
                 ncp = M,
                 K = K)
  class(K.out) <- c ("main", "mrss")
  cat("K =", K, "\n")
  return(invisible(K.out))
}
# example
# mrss.bcra3r2_pn(es=.399, rho3_trt = .10, omega3 = .50, rho2_trt = .15, rho_ic = .20,
#                 n = 40, J = 60, ic_size = 10)
