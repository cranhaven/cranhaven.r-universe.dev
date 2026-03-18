
mdes.ira1r1 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                             p=.50, g1=0, r21=0, n){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- n-g1-2
  SSE <- sqrt((1-r21)/(p*(1-p)*n))

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.ira1r1",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                p=p, r21=r21, g1=g1,
                                n=n),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}
# example
# mdes.ira1r1(n=200)
mdes.ira <- mdes.ira1r1

power.ira1r1 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                              p=.50, g1=0, r21=0, n){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- n-g1-2
  SSE <- sqrt((1-r21)/(p*(1-p)*n))

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.ira1r1",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                 p=p, r21=r21, g1=g1,
                                 n=n),
                     df = df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# example
# power.ira1r1(n=200)
power.ira <- power.ira1r1

mrss.ira1r1 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                        n0=10, tol=.10,
                        p=.50, g1=0, r21=0){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    df <- n0-g1-2
    if(df<= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    n1 <- (M/es)^2 * ((1-r21)/(p*(1-p)))
    if(abs(n1-n0)<tol){conv <- TRUE}
    n0 <- (n1+n0)/2
    i <- i+1
  }
  n <- round(ifelse(df>0,round(n0),NA))

  n.out <-  list(fun = "mrss.ira1r1",
                 parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                              n0=n0, tol=tol,
                              p=p, r21=r21, g1=g1),
                 df=df,
                 ncp = M,
                 n = n)
  class(n.out) <- c("main", "mrss")
  cat("n =", n, "\n")
  return(invisible(n.out))
}
# mrss.ira1r1()
mrss.ira <- mrss.ira1r1

mdes.bira2f1 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                         p=.50, g1=0, r21=0, n, J){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- J * (n - 2) - g1
  SSE <- sqrt((1-r21)/(p*(1-p)*J*n))

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.bira2f1",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                p=p, r21=r21, g1=g1,
                                n=n, J=J),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}
# example
# mdes.bira2f1(n=55, J=3)

power.bira2f1 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                          p=.50, g1=0, r21=0, n, J){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- J * (n - 2) - g1
  SSE <- sqrt((1-r21)/(p*(1-p)*J*n))

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.bira2f1",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                  p=p, r21=r21, g1=g1,
                                  n=n, J=J),
                     df = df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# example
# power.bira2f1(n=55, J=3)

mrss.bira2f1 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                         J, n0=10, tol=.10,
                         p=.50, g1=0, r21=0){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    df <- J*(n0-2)-g1
    if(df<= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    n1 <- (M/es)^2 * ((1-r21)/(p*(1-p)*J))
    if(abs(n1-n0)<tol){conv <- TRUE}
    n0 <- (n1+n0)/2
    i <- i+1
  }
  n <- ifelse(df>0,round(n0),NA)

  mrss.out <-  list(fun = "mrss.bira2f1",
                    parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                 J=J, n0=n0, tol=tol,
                                 p=p, r21=r21, g1=g1),
                    df = df,
                    ncp = M,
                    n = n)
  class(mrss.out) <- c("main", "mrss")
  cat("n =", n, "(per block)\n")
  return(invisible(mrss.out))
}
# example
# mrss.bira2f1(J=5)

mdes.bira2c1 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                         p=.50, g1=0, r21=0, n, J){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- J * (n - 1) - g1 - 1
  SSE <- sqrt((1-r21)/(p*(1-p)*J*n))

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.bira2c1",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                p=p, r21=r21, g1=g1,
                                n=n, J=J),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}
# example
# mdes.bira2c1(n=55, J=14)

power.bira2c1 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                          p=.50, g1=0, r21=0, n, J){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- J * (n - 1) - g1 - 1
  SSE <- sqrt((1-r21)/(p*(1-p)*J*n))

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.bira2c1",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                  p=p, r21=r21, g1=g1,
                                  n=n, J=J),
                     df = df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# example
# power.bira2c1(n=55, J=14)

mrss.bira2c1 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                         J, n0=10, tol=.10,
                         p=.50, g1=0, r21=0){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    df <- J*(n0-1)-g1-1
    if(df<= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    n1 <- (M/es)^2 * ((1-r21)/(p*(1-p)*J))
    if(abs(n1-n0)<tol){conv <- TRUE}
    n0 <- (n1+n0)/2
    i <- i+1
  }
  n <- ifelse(df>0,round(n0),NA)

  mrss.out <-  list(fun = "mrss.bira2c1",
                    parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                 J=J, n0=n0, tol=tol,
                                 p=p, r21=r21, g1=g1),
                    df = df,
                    ncp = M,
                    n = n)
  class(mrss.out) <- c("main", "mrss")
  cat("n =", n, "(per block)\n")
  return(invisible(mrss.out))
}
# example
# mrss.bira2c1(J=5)


mdes.ira_pn <- function(power=.80, alpha=.05, two.tailed=TRUE, df=NULL, ratio_tc_var=1,
                        rho_ic=.20, p=.50, r21=0, n, ic_size=1){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(!is.null(df) & ratio_tc_var != 1)
    warning("'ratio_tc_var' argment is ignored", call. = FALSE)

  # Satterthwaite (1946) approximation to df
  if(is.null(df)) {
    it <- n * p / ic_size
    nc <- n * (1 - p)
    vt_vc_ratio <- ratio_tc_var*(1 / ic_size + rho_ic / (1 - rho_ic))
    df <- (nc-1)*(it-1)*(it + nc*vt_vc_ratio)^2 / ((it-1)*it^2 + (nc-1)*nc^2 * vt_vc_ratio^2)
  }

  deff_rand_ic <- 1 +  rho_ic * (1 - p) * (ic_size - 1) / (1 - p*rho_ic)
  SSE <- sqrt(((1 - r21) / (n * p * (1 - p))) * ((1 - p * rho_ic) / (1 - rho_ic)) * deff_rand_ic )

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  cat(ifelse(ic_size == 1,"Fixed", "Random"), "intervention cluster effects\n")
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE,
                        df = round(df,3), two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.ira_pn",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed, ratio_tc_var=ratio_tc_var,
                                rho_ic = rho_ic, r21=r21, p=p, n=n, ic_size = ic_size),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}
# constructed data example 3.3 (Lohr, Schochet, Sanders, 2014, p . 51 - 59)
# mdes.ira_pn(n = 250, rho_ic = 0.10, ic_size = 5, ratio_tc_var = 1, df = Inf)

power.ira_pn <- function(es=.25,alpha=.05, two.tailed=TRUE, df=NULL, ratio_tc_var=1,
                         rho_ic=.20, p=.50, r21=0, n, ic_size=1){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(!is.null(df) & ratio_tc_var != 1)
    warning("'ratio_tc_var' argment is ignored for z-test", call. = FALSE)

  # Satterthwaite (1946) approximation to df
  if(is.null(df)) {
    it <- n * p / ic_size
    nc <- n * (1 - p)
    vt_vc_ratio <- ratio_tc_var*(1 / ic_size + rho_ic / (1 - rho_ic))
    df <- (nc-1)*(it-1)*(it + nc*vt_vc_ratio)^2 / ((it-1)*it^2 + (nc-1)*nc^2 * vt_vc_ratio^2)
  }

  deff_rand_ic <- 1 +  rho_ic * (1 - p) * (ic_size - 1) / (1 - p*rho_ic)
  SSE <- sqrt(((1 - r21) / (n * p * (1 - p))) * ((1 - p * rho_ic) / (1 - rho_ic)) * deff_rand_ic)

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  cat(ifelse(ic_size == 1,"Fixed", "Random"), "intervention cluster effects\n")
  .summ.power(power = power, alpha = alpha, sse = SSE, df = round(df,3), two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.ira_pn",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed, df=df, ratio_tc_var=ratio_tc_var,
                                  rho_ic = rho_ic, r21=r21, p=p, n=n, ic_size = ic_size),
                     df = df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# constructed data example 3.3 (Lohr, Schochet, Sanders, 2014, p . 51 - 59)
# power.ira_pn(es = .40, n = 250, rho_ic = 0.10, ic_size = 5, ratio_tc_var = 1, df = Inf)

mrss.ira_pn <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE, ratio_tc_var=1, z.test=FALSE,
                        rho_ic=.20, p=.50, r21=0, ic_size=1, n0=500, tol=.10){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(isTRUE(z.test) & ratio_tc_var != 1)
    warning("'ratio_tc_var' argment is ignored for z-test", call. = FALSE)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){

    it <- n0 * p / ic_size
    nc <- n0 * (1 - p)
    vt_vc_ratio <- ratio_tc_var*(1 / ic_size + rho_ic / (1 - rho_ic))
    df <- (nc-1)*(it-1)*(it + nc*vt_vc_ratio)^2 / ((it-1)*it^2 + (nc-1)*nc^2 * vt_vc_ratio^2)

    if(df <= 0) stop("Increase 'n0'", call. = FALSE)
    if(df <= 0 | is.infinite(df)){break}

    if(z.test) df <- Inf

    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)

    deff_rand_ic <- 1 +  rho_ic * (1 - p) * (ic_size - 1) / (1 - p*rho_ic)
    VAR <- ((1 - r21) / (p * (1 - p))) * ((1 - p * rho_ic) / (1 - rho_ic)) * deff_rand_ic

    n1 <- (M/es)^2 * VAR
    if(abs(n1-n0)<tol){conv <- TRUE}
    n0 <- (n1+n0)/2
    i <- i+1
  }

  n <- round(ifelse(df>0,round(n0),NA))
  J <- round(ifelse(df>0,round(it),NA))

  n.out <-  list(fun = "mrss.ira_pn",
                 parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                              ratio_tc_var=ratio_tc_var, z.test=z.test,
                              rho_ic = rho_ic, r21=r21, p=p, ic_size = ic_size,
                              n0=n0, tol=tol),
                 df=df,
                 ncp = M,
                 n = n)
  class(n.out) <- c("main", "mrss")
  cat("n =", n, "(total)\n")
  return(invisible(n.out))
}
# constructed data example 3.3 (Lohr, Schochet, Sanders, 2014, p . 51 - 59)
# mrss.ira_pn(es = .40, rho_ic = 0.10, ic_size = 5, ratio_tc_var = 1, z.test = TRUE)
