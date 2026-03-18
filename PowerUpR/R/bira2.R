mdes.bira2r1 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                        rel1=1, rho2, esv2=NULL, omega2=esv2/rho2, g2=0, r21=0, r2t2=0,
                        p=.50, n, J){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- J - g2 - 1
  SSE <- sqrt(rho2*omega2*(1-r2t2)/J +
               (1-rho2)*(1-r21)/(p*(1-p)*J*n*rel1))

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.bira2r1",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rel1=rel1, rho2=rho2, esv2=esv2, omega2=omega2,
                                r21=r21, r2t2=r2t2, g2=g2,
                                p=p, n=n, J=J),
                   df=df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}
# example
# mdes.bira2r1(rho2=.35, omega2=.10, n=83, J=480)
mdes.bira2 <- mdes.bira2r1

power.bira2r1 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                         rel1=1, rho2, esv2=NULL, omega2=esv2/rho2, g2=0, r21=0, r2t2=0,
                         p=.50, n, J){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- J - g2 - 1
  SSE <- sqrt(rho2*omega2*(1-r2t2)/J +
                    (1-rho2)*(1-r21)/(p*(1-p)*J*n*rel1))

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.bira2r1",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                  rel1=rel1, rho2=rho2, esv2=esv2, omega2=omega2,
                                  r21=r21, r2t2=r2t2, g2=g2,
                                  p=p, n=n, J=J),
                     df=df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# example
# power.bira2r1(rho2=.35, omega2=.10, n=83, J=480)
power.bira2 <- power.bira2r1

mrss.bira2r1 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                         rel1=1, rho2, esv2=NULL, omega2=esv2/rho2, r21=0, r2t2=0,
                         J0=10, tol=.10, g2=0, p=.50, n){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    df <- J0-g2-1
    if(df<= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    J1 <- (M/es)^2 * (rho2*omega2*(1-r2t2) +
                          (1-rho2)*(1-r21)/(p*(1-p)*n*rel1))
    if(abs(J1-J0)<tol){conv <- TRUE}
    J0 <- (J1+J0)/2
    i <- i+1
  }
  J <- ifelse(df>0,round(J0),NA)

  mrss.out <-  list(fun = "mrss.bira2r1",
                    parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                 rel1=rel1, rho2=rho2, esv2=esv2, omega2=omega2, J0=J0, tol=tol,
                                 p=p, n=n, r21=r21, r2t2=r2t2, g2=g2),
                    df=df,
                    ncp = M,
                    J = J)
  class(mrss.out) <- c("main", "mrss")
  cat("J =", J, "\n")
  return(invisible(mrss.out))
}
# example
# mrss.bira2r1(rho2=.35, omega2=.10, n=83)
mrss.bira2 <- mrss.bira2r1

power.mod211 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                         rho2, omega2tm, r21=0,
                         p=.50, q=NULL, n, J){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(omega2tm == 0) {
    df <- J*(n-1)- 4
    cat("\nModerator effect: Non-randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  } else {
    df <- J - 1
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  }

  SSE <- ifelse(is.null(q),
                sqrt(omega2tm / J +
                       (1-rho2)*(1-r21)/(p*(1-p)*J*n)), # continuous mod
                sqrt(omega2tm / J +
                       (1-rho2)*(1-r21)/(p*(1-p)*q*(1-q)*J*n)) # binary mod
  )

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)

  power.out <- list(fun = "power.mod211",
                    parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                 rho2=rho2, omega2tm=omega2tm, r21=r21,
                                 p=p, q=q, n=n, J=J),
                    df = df,
                    ncp = es/SSE,
                    power = power)
  class(power.out) <- c("mod211", "power")
  return(invisible(power.out))
}
# examples: randomly-varying cont. mod
# power.mod211(es = .249, rho2=.241, omega2tm=.149, r21=.497, n=20, J=20)
# power.mod211(es = .251, rho2=.245, omega2tm=.151, r21=.500, n=20, J=40)
# examples: randomly-varying bin. mod
# power.mod211(es = .248, rho2=.247, omega2tm=.148, r21=.493, q = .50, n=20, J=40)
# power.mod211(es = .251, rho2=.248, omega2tm=.151, r21=.505, q = .50, n=20, J=80)
# examples: non-randomly-varying cont. mod
# power.mod211(es = .150, rho2=.245, omega2tm=0, r21=.498, n=20, J=20)
# power.mod211(es = .150, rho2=.246, omega2tm=0, r21=.500, n=20, J=40)
# examples: non-randomly-varying bin. mod
# power.mod211(es = .250, rho2=.246, omega2tm=0, r21=.490, q = .50, n=20, J=20)
# power.mod211(es = .248, rho2=.250, omega2tm=0, r21=.490, q = .50, n=20, J=40)

power.mod212 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                         rho2, omega2t, r21=0,
                         p=.50, q=NULL, n, J) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(omega2t == 0) {
    df <- J*(n-1) - 3
    cat("\nModerator effect: Non-randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  } else {
    df <- J - 2
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  }

  if(omega2t == 0) {
    df <- J*(n-1) - 3
    SSE <- ifelse(is.null(q),
                  sqrt((1-rho2)*(1-r21) / (p*(1-p)*J*n)), # continuous mod
                  sqrt((1-rho2)*(1-r21) / (p*(1-p)*q*(1-q)*J*n)) # binary mod
    )
  } else if(omega2t > 0) {
    df <- J - 2
    SSE <- ifelse(is.null(q),
                  sqrt((omega2t - es^2) / J +
                         (1-rho2)*(1-r21) / (p*(1-p)*J*n)), # continuous mod
                  sqrt((omega2t - es^2*q*(1-q)) / (q*(1-q)*J) +
                         (1-rho2)*(1-r21) / (p*(1-p)*q*(1-q)*J*n)) # binary mod
    )
  }

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)

  power.out <- list(fun = "power.mod212",
                    parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                 rho2=rho2, omega2t=omega2t, r21=r21,
                                 p=p, q=q, n=n, J=J),
                    df = df,
                    ncp = es/SSE,
                    power = power)
  class(power.out) <- c("mod212", "power")
  return(invisible(power.out))
}
# examples: randomly-varying cont. mod
# power.mod212(es=.253, rho2=.242, omega2t = .216, r21=.500, n=20, J=20)
# power.mod212(es=.253, rho2=.247, omega2t = .213, r21=.501, n=20, J=40)
# examples: randomly-varying bin. mod
# power.mod212(es=.249, rho2=.247, omega2t = .166, r21=.501, q = .50, n=20, J=40)
# power.mod212(es=.252, rho2=.247, omega2t = .166, r21=.501, q = .50, n=20, J=80)
# examples: non-randomly-varying cont. mod
# power.mod212(es=.152, rho2=.245, omega2t = 0, r21=.499, n=20, J=20)
# power.mod212(es=.150, rho2=.247, omega2t = 0, r21=.499, n=20, J=40)
# examples: non-randomly-varying bin. mod
# power.mod212(es=.250, rho2=.249, omega2t = 0, r21=.498, q = .50, n=20, J=20)
# power.mod212(es=.249, rho2=.251, omega2t = 0, r21=.500, q = .50, n=20, J=40)

mdesd.mod211 <- mdes.mod211 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                                        rho2, omega2tm, g1=0, r21=0,
                                        p=.50, q=NULL, n, J){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(omega2tm == 0) {
    df <- J*(n-1)- 4
    cat("\nModerator effect: Non-randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  } else {
    df <- J - 1
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  }

  SSE <- ifelse(is.null(q),
                sqrt(omega2tm / J +
                       (1-rho2)*(1-r21)/(p*(1-p)*J*n)), # continuous mod
                sqrt(omega2tm / J +
                       (1-rho2)*(1-r21)/(p*(1-p)*q*(1-q)*J*n)) # binary mod
  )


  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "mod", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.mod211",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, omega2tm=omega2tm, r21=r21,
                                p=p, q=q, n=n, J=J),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("mod211", "mdes")
  return(invisible(mdes.out))
}
# examples: randomly-varying cont. mod
# mdesd.mod211(power = .605, rho2=.241, omega2tm=.149, r21=.497, n=20, J=20) # mdesd = .249,
# mdesd.mod211(power = .902, rho2=.245, omega2tm=.151, r21=.500, n=20, J=40) # mdesd = .251,
# examples: randomly-varying bin. mod
# mdesd.mod211(power = .622, rho2=.247, omega2tm=.148, r21=.493, q = .50, n=20, J=40) # mdesd = .248
# mdesd.mod211(power = .912, rho2=.248, omega2tm=.151, r21=.505, q = .50, n=20, J=80) # mdesd = .251
# examples: non-randomly-varying cont. mod
# mdesd.mod211(power = .681, rho2=.245, omega2tm=0, r21=.498, n=20, J=20) # mdesd = .150
# mdesd.mod211(power = .932, rho2=.246, omega2tm=0, r21=.500, n=20, J=40) # mdesd = .150
# examples: non-randomly-varying bin. mod
# mdesd.mod211(power = .520, rho2=.246, omega2tm=0, r21=.490, q = .50, n=20, J=20) # mdesd = .250
# mdesd.mod211(power = .808, rho2=.250, omega2tm=0, r21=.490, q = .50, n=20, J=40) # mdesd = .248

mdesd.mod212 <- mdes.mod212 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                                        rho2, omega2t, g1=0, r21=0,
                                        p=.50, q=NULL, n, J){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(omega2t == 0) {
    df <- J*(n-1) - 3
    cat("\nModerator effect: Non-randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  } else {
    df <- J - 2
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  }

  t1 <- ifelse(two.tailed == TRUE, abs(qt(alpha / 2, df)), abs(qt(alpha, df)))
  t2 <- abs(qt(power, df))
  mv <- ifelse(power >= 0.5, t1 + t2, t1 - t2)

  if(omega2t == 0) {
    df <- J*(n-1) - 3
    SSE <- ifelse(is.null(q),
                  sqrt( ((1-rho2)*(1-r21) / (p*(1-p)*J*n)) ), # continuous mod
                  sqrt( ((1-rho2)*(1-r21) / (p*(1-p)*q*(1-q)*J*n)) ) # binary mod
    )
  } else if(omega2t > 0) {
    df <- J - 2
    SSE <- ifelse(is.null(q),
                  sqrt(
                    (omega2t / J +
                       (1-rho2)*(1-r21) / (p*(1-p)*J*n)) / (1 + mv^2 / J) ), # continuous mod
                  sqrt(
                    (omega2t / (q*(1-q)*J) +
                       (1-rho2)*(1-r21) / (p*(1-p)*q*(1-q)*J*n)) / (1 + mv^2 / J) ) # binary mod
    )
  }

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "mod", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.mod212",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, omega2t=omega2t, r21=r21,
                                p=p, q=q, n=n, J=J),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("mod212", "mdes")
  return(invisible(mdes.out))
}
# examples: randomly-varying cont. mod
# mdesd.mod212(power = .611, rho2=.242, omega2t = .216, r21=.500, n=20, J=20) # mdesd = .253,
# mdesd.mod212(power = .909, rho2=.247, omega2t = .213, r21=.501, n=20, J=40) # mdesd = .253,
# examples: randomly-varying bin. mod #
# mdesd.mod212(power = .366, rho2=.247, omega2t = .166, r21=.501, q = .50, n=20, J=40) # mdesd = .249
# mdesd.mod212(power = .650, rho2=.247, omega2t = .166, r21=.501, q = .50, n=20, J=80) # mdesd = .252
# examples: non-randomly-varying cont. mod
# mdesd.mod212(power = .693, rho2=.245, omega2t = 0, r21=.499, n=20, J=20) # mdesd = .152
# mdesd.mod212(power = .932, rho2=.247, omega2t = 0, r21=.499, n=20, J=40) # mdesd = .150
# examples: non-randomly-varying bin. mod
# mdesd.mod212(power = .528, rho2=.249, omega2t = 0, r21=.498, q = .50, n=20, J=20) # mdesd = .250
# mdesd.mod212(power = .820, rho2=.251, omega2t = 0, r21=.500, q = .50, n=20, J=40) # mdesd = .249

mrss.mod211 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                        n, J0=10, tol=.10, rho2, omega2tm, r21=0,
                        p=.50, q=NULL) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    if(omega2tm == 0) {
      df <- J0*(n-1)- 4
    } else {
      df <- J0 - 1
    }
    if(df <= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    J1 <- ifelse(is.null(q),
                 (M/es)^2 * (omega2tm +
                               (1-rho2)*(1-r21)/(p*(1-p)*n)), # continuous mod
                 (M/es)^2 * (omega2tm +
                               (1-rho2)*(1-r21)/(p*(1-p)*q*(1-q)*n)) # binary mod
    )

    if(abs(J1-J0)<tol){conv <- TRUE}
    J0 <- (J1+J0)/2
    i <- i+1
  }
  J <- ifelse(df>0,round(J0),NA)

  mrss.out <-  list(fun = "mrss.mod211",
                    parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                 n=n, J0=J0, tol=tol, rho2=rho2, omega2tm=omega2tm,
                                 r21=r21, p=p, q=q),
                    df = df,
                    ncp = M,
                    J = J)

  if(omega2tm == 0) {
    df <- J*(n-1)- 4
    cat("\nModerator effect: Non-randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"),
        "\nJ =", J)
  } else {
    df <- J - 1
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"),
        "\nJ =", J)
  }

  class(mrss.out) <- c("mod211", "mrss")
  return(invisible(mrss.out))
}

# examples: randomly-varying mod
# mrss.mod211(es = .248, power = .298, rho2=.247, omega2tm=.148, r21=.493, q=.50, n=20)
# mrss.mod211(es = .248, power = .527, rho2=.247, omega2tm=.148, r21=.493, n=20)
# examples: non-randomly-varying mod
# mrss.mod211(es = .248, power = .340, rho2=.247, omega2tm=0, r21=.493, q=.50, n=20)
# mrss.mod211(es = .248, power = .987, rho2=.247, omega2tm=0, r21=.493, n=20)

mrss.mod212 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                        n, J0=10, tol=.10, rho2, omega2t, r21=0,
                        p=.50, q=NULL) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    if(omega2t == 0) {
      df <- J0*(n-1)- 3
    } else {
      df <- J0 - 2
    }
    if(df <= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    if(omega2t == 0) {
      J1 <- ifelse(is.null(q),
                   (M/es)^2 * ((1-rho2)*(1-r21) / (p*(1-p)*n)), # continuous mod
                   (M/es)^2 * ((1-rho2)*(1-r21) / (p*(1-p)*q*(1-q)*n)) # binary mod
      )
    } else if(omega2t > 0) {
      J1 <- ifelse(is.null(q),
                   (M/es)^2 * (omega2t + (1-rho2)*(1-r21) / (p*(1-p)*n)) - M^2, # continuous mod
                   (M/es)^2 * (omega2t / (q*(1-q)) + (1-rho2)*(1-r21) / (p*(1-p)*q*(1-q)*n)) - M^2# binary mod
      )
    }
    if(abs(J1-J0)<tol){conv <- TRUE}
    J0 <- (J1+J0)/2
    i <- i+1
  }
  J <- ifelse(df>0,round(J0),NA)

  mrss.out <-  list(fun = "mrss.mod212",
                    parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                 n=n, J0=J0, tol=tol, rho2=rho2, omega2t=omega2t,
                                 r21=r21, p=p, q=q),
                    df = df,
                    ncp = M,
                    J = J)

  if(omega2t == 0) {
    df <- J*(n-1) - 3
    cat("\nModerator effect: Non-randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"),
        "\nJ =", J)
  } else {
    df <- J - 2
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"),
        "\nJ =", J)
  }

  class(mrss.out) <- c("mod212", "mrss")
  return(invisible(mrss.out))
}

# examples: randomly-varying mod
# mrss.mod212(es = .248, power = .483, rho2=.247, omega2t=.148, r21=.493, q=.50, n=20)
# mrss.mod212(es = .248, power = .659, rho2=.247, omega2t=.148, r21=.493, n=20)
# examples: non-randomly-varying mod
# mrss.mod212(es = .248, power = .34, rho2=.247, omega2t=0, r21=.493, q=.50, n=20)
# mrss.mod212(es = .248, power = .987, rho2=.247, omega2t=0, r21=.493, n=20)

# r21 factored in both levels
mdes.bira2_pn <- function(power=.80, alpha=.05, two.tailed=TRUE, df=NULL,
                          rho2_trt=.20, omega2=.50, rho_ic=0,
                          p=.50, g2=0, r21=0, n, J, ic_size=1){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(ic_size == 1 & rho_ic != 0) {
    rho_ic <- 0
    warning("Forcing 'rho_ic = 0'", call. = FALSE)
  } else  if(ic_size > 1 & rho_ic == 0) {
    warning("'rho_ic = 0'?", call. = FALSE)
  }

  # if(is.null(df)) df <- J - g2 - 1
  # Satterthwaite (1946) approximation
  # assuming equal level 1 variance for treatment and control groups
  if(is.null(df)) {
   q <- 1 - p
   it <- n / ic_size
   xt <- rho2_trt + rho_ic / it + (1 - rho2_trt - rho_ic) / (it * ic_size)
   xc <- rho2_trt + (1 - rho2_trt - rho_ic) / n # n -> nc
   df <- (xc/q + xt/p)^2 / (xc^2 / (q^2 * (J*q - 1)) + xt^2 / (p^2 *(J*p - 1)))
  }


  deff_rand_ic <- 1 + ((rho2_trt * omega2 * n * p * (1 - p) - rho2_trt) + rho_ic * (1 - p) * (ic_size - 1)) / (1 - p * rho_ic)
  SSE <- sqrt(((1 - r21) / (J * n * p * (1 - p))) * ((1 - p * rho_ic) / (1 - rho_ic)) * deff_rand_ic )

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE,
                        df = round(df,3), two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.bira2_pn",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed, df=df,
                                rho2_trt=rho2_trt, omega2=omega2, rho_ic=rho_ic, r21=r21,
                                p=p, n=n, J=J, ic_size=ic_size),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}
# constructed data example 3.5.2 (Lohr, Schochet, Sanders, 2014, p. 69 - 72)
# mdes.bira2_pn(rho2_trt=.15, omega2=.50, rho_ic=.10, n=20, J=15, ic_size=5, df=Inf)

# r21 factored in both levels
power.bira2_pn <- function(es=.25,alpha=.05, two.tailed=TRUE, df=NULL,
                           rho2_trt=.20, omega2=.50, rho_ic=0,
                           p=.50, g2=0, r21=0, n, J, ic_size=1){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(ic_size == 1 & rho_ic != 0) {
    rho_ic <- 0
    warning("Forcing 'rho_ic = 0'", call. = FALSE)
  } else  if(ic_size > 1 & rho_ic == 0) {
    warning("'rho_ic = 0'?", call. = FALSE)
  }

  # if(is.null(df)) df <- J - g2 - 1
  # Satterthwaite (1946) degrees of freedom
  if(is.null(df)) {
    q <- 1 - p
    it <- n / ic_size
    xt <- rho2_trt + rho_ic / it + (1 - rho2_trt - rho_ic) / (it * ic_size)
    xc <- rho2_trt + (1 - rho2_trt - rho_ic) / n # n -> nc
    df <- (xc/q + xt/p)^2 / (xc^2 / (q^2 * (J*q - 1)) + xt^2 / (p^2 *(J*p - 1)))
  }

  deff_rand_ic <- 1 + ((rho2_trt * omega2 * n * p * (1 - p) - rho2_trt) + rho_ic * (1 - p) * (ic_size - 1)) / (1 - p * rho_ic)
  SSE <- sqrt(((1 - r21) / (J * n * p * (1 - p))) * ((1 - p * rho_ic) / (1 - rho_ic)) * deff_rand_ic )

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = round(df,3), two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.bira2_pn",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed, df=df,
                                  rho2_trt=rho2_trt, omega2=omega2, rho_ic=rho_ic, r21=r21,
                                  p=p, n=n, J=J, ic_size=ic_size),
                     df = df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# constructed data example 3.5.2 (Lohr, Schochet, Sanders, 2014, p. 69 - 72)
# power.bira2_pn(es=.40, rho2_trt=.15, omega2=.50, rho_ic=.10, n=20, J=15, ic_size=5, df=Inf)

# r21 factored in both levels
mrss.bira2_pn  <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE, z.test=FALSE,
                           rho2_trt=.20, omega2=.50, rho_ic=0,
                           p=.50, g2=0, r21=0, n, ic_size=1, J0=10, tol=.10){

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

    # if(is.null(df)) df <- J0 - g2 - 1
    # Satterthwaite (1946) degrees of freedom
    q <- 1 - p
    it <- n / ic_size
    xt <- rho2_trt + rho_ic / it + (1 - rho2_trt - rho_ic) / (it * ic_size)
    xc <- rho2_trt + (1 - rho2_trt - rho_ic) / n # n -> nc
    df <- (xc/q + xt/p)^2 / (xc^2 / (q^2 * (J0*q - 1)) + xt^2 / (p^2 *(J0*p - 1)))

    if(df <= 0) stop("Increase 'J0'", call. = FALSE)
    if(df <= 0 | is.infinite(df)){break}

    if(z.test) df <- Inf

    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)

    deff_rand_ic <- 1 + ((rho2_trt * omega2 * n * p * (1 - p) - rho2_trt) + rho_ic * (1 - p) * (ic_size - 1)) / (1 - p * rho_ic)
    VAR <- ((1 - r21) / (n * p * (1 - p))) * ((1 - p * rho_ic) / (1 - rho_ic)) * deff_rand_ic

    J1 <- (M/es)^2 * VAR
    if(abs(J1-J0)<tol){conv <- TRUE}
    J0 <- (J1+J0)/2
    i <- i+1
  }

  J <- round(ifelse(df>0,round(J0),NA))

  J.out <-  list(fun = "mrss.bira2_pn",
                 parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed, z.test=z.test,
                              rho2_trt=rho2_trt, omega2=omega2, rho_ic=rho_ic, r21=r21,
                              p=p, n=n, ic_size=ic_size,
                              J0=J0, tol=tol),
                 df=df,
                 ncp = M,
                 J =  J)
  class(J.out) <- c("main", "mrss")
  cat("J = ", J, "\n")
  return(invisible(J.out))
}
# constructed data example 3.5.2 (Lohr, Schochet, Sanders, 2014, p. 69 - 72)
# mrss.bira2_pn(es=.40, rho2_trt=.15, omega2=.50, rho_ic=.10, n=20, ic_size=5, z.test = TRUE)


