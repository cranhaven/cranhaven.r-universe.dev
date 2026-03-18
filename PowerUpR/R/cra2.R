mdes.cra2r2 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                        rel1=1, rho2, p=.50, g2=0, r21=0, r22=0,
                        n, J){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- J - g2 - 2
  SSE <- sqrt(rho2*(1-r22)/(p*(1-p)*J) +
               (1-rho2)*(1-r21)/(p*(1-p)*J*n*rel1))

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.cra2r2",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rel1=rel1, rho2=rho2, g2=g2, r21=r21, r22=r22,
                                p=p, n=n, J=J),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}
# example
# mdes.cra2r2(rho2=.20, n=4, J=20, alpha=.01)
mdes.cra2 <- mdes.cra2r2

mdesd.mod221 <- mdes.mod221 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                        rho2, omegam2, r21=0, r2m2=0, p=.50, q=NULL, n, J){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(omegam2 == 0 || r2m2 == 1) {
    df <- n*J - J - 2
    cat("\nModerator effect: Non-randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  } else {
    df <- J - 2
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  }

  SSE <- ifelse(is.null(q),
                sqrt(rho2*omegam2*(1-r2m2)/(p*(1-p)*J) +
                       (1-rho2)*(1-r21)/(p*(1-p)*J*n)), # continuous mod
                sqrt(rho2*omegam2*(1-r2m2)/(p*(1-p)*J) +
                       (1-rho2)*(1-r21)/(p*(1-p)*q*(1-q)*J*n)) # binary mod
  )

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "mod", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.mod221",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, omegam2=omegam2, r21=r21, r2m2=r2m2,
                                p=p, q=q, n=n, J=J),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("mod221", "mdes")
  return(invisible(mdes.out))
}
# examples
# mdes.mod221(omegam2=.2, r2m2=1, rho2=.20, n=4, J=20, q=.3)
# mdesd.mod221(rho2=.2, omegam2=0, r2m2=.2, n=4, J=20)
# mdes.mod221(rho2=.2, omegam2=.2, r2m2=.2, q=NULL, n=4, J=20)

mdesd.mod222 <- mdes.mod222 <- function(power=.80, alpha=.05, two.tailed=TRUE,
                                        rho2, r22=0, p=.50, q=NULL, n, J){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  cat(ifelse(is.null(q), "\nModerator type: Continuous\n", "\nModerator type: Binary\n"))

  df <- J - 5
  SSE <- ifelse(is.null(q),
                sqrt(rho2*(1-r22)/(p*(1-p)*(J-5)) +
                       (1-rho2)/(p*(1-p)*(J-5)*n)), # continuous mod
                sqrt(rho2*(1-r22)/(p*(1-p)*q*(1-q)*(J-5)) +
                       (1-rho2)/(p*(1-p)*q*(1-q)*(J-5)*n)) # binary mod
  )

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "mod", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.mod222",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, r22=r22, p=p, q=q, n=n, J=J),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("mod222", "mdes")
  return(invisible(mdes.out))
}
# examples
# mdes.mod222(rho2=.20, n=4, J=20)
# mdes.mod222(alpha = .04, rho2=.20, n=4, J=20, q=.5)

power.cra2r2 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                         rel1=1, rho2, g2=0, p=.50, r21=0, r22=0,
                         n, J){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- J - g2 - 2
  SSE <- sqrt(rho2*(1-r22)/(p*(1-p)*J) +
                    (1-rho2)*(1-r21)/(p*(1-p)*J*n*rel1))

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.cra2r2",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                  rel1=rel1, rho2=rho2,
                                  p=p, r21=r21, r22=r22, g2=g2,
                                  n=n, J=J),
                     df = df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# example
# power.cra2r2(es=.50, rho2=.20, n=4, J=20)
power.cra2 <- power.cra2r2

power.mod221 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                               rho2, omegam2, r21=0, r2m2=0,
                               p=.50, q=NULL, n, J){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(omegam2 == 0 || r2m2 == 1) {
    df <- n*J - J - 2
    cat("\nModerator effect: Non-randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  } else {
    df <- J - 2
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"))
  }

  SSE <- ifelse(is.null(q),
                sqrt(rho2*omegam2*(1-r2m2)/(p*(1-p)*J) +
                       (1-rho2)*(1-r21)/(p*(1-p)*J*n)), # continuous mod
                sqrt(rho2*omegam2*(1-r2m2)/(p*(1-p)*J) +
                       (1-rho2)*(1-r21)/(p*(1-p)*q*(1-q)*J*n)) # binary mod
  )

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)

  power.out <- list(fun = "power.mod221",
                   parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, omegam2=omegam2, r21=r21, r2m2=r2m2,
                                p=p, q=q, n=n, J=J),
                   df = df,
                   ncp = es/SSE,
                   power = power)
  class(power.out) <- c("mod221", "power")
  return(invisible(power.out))
}
# examples
# power.mod221(rho2=.2, omegam2=.2, r2m2=.2, n=4, J=20)
# power.mod221(rho2=.2, omegam2=0, r2m2=.2, q=.3, n=4, J=20)

power.mod222 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                              rho2, r22=0, p=.50, q=NULL, n, J) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  cat(ifelse(is.null(q), "\nModerator type: Continuous\n", "\nModerator type: Binary\n"))

  df <- J - 5
  SSE <- ifelse(is.null(q),
                sqrt(rho2*(1-r22)/(p*(1-p)*(J-5)) +
                       (1-rho2)/(p*(1-p)*(J-5)*n)), # continuous mod
                sqrt(rho2*(1-r22)/(p*(1-p)*q*(1-q)*(J-5)) +
                       (1-rho2)/(p*(1-p)*q*(1-q)*(J-5)*n)) # binary mod
  )

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)

  power.out <- list(fun = "power.mod222",
                   parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2, r22=r22, p=p, q=q, n=n, J=J),
                   df = df,
                   ncp = es/SSE,
                   power = power)
  class(power.out) <- c("mod222", "power")
  return(invisible(power.out))
}
# examples
# power.mod222(rho2=.20, n=4, J=20)
# power.mod222(rho2=.20, n=4, J=20)

power.med211 <- function(esa, esb1, esB, escp, two.tailed = TRUE, alpha = .05,
                         mc = FALSE, nsims = 1000, ndraws = 1000,
                         rhom2, rho2, r21, r22, r2m1, r2m2,
                         p, n, J) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  # standard errors for 2-1-1 mediation
  .se.a211 <- function(esa, rhom2, r2m1, r2m2, n, J, p) {
    t2mbar <- rhom2 * (1 - r2m2 - (p * (1 - p)  * esa^2) / rhom2)
    sig2mbar <- (1 - rhom2) * (1 - r2m1)
    var.a211 <- (t2mbar + sig2mbar / n) / (J * p * (1 - p))
    if(is.nan(var.a211) | var.a211 <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.a211)))
  }

  .se.b1211 <- function(esb1, rho2, rhom2, r21, r2m1, n, J) {
    sig2mbar <- (1 - rhom2) * (1 - r2m1)
    sig2ybar <- (1 - rho2) * (1 - r21 - (((1 - rhom2) / (1 - rho2)) * esb1^2 * (1 - r2m1)))
    var.b1211 <- sig2ybar / ((J * n - J) * sig2mbar)
    if(is.nan(var.b1211) | var.b1211 <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.b1211)))
  }

  .se.B211 <- function(esa, esB, esb1, escp, rho2, rhom2, r22, r21, r2m2, r2m1, n, J, p) {
    t2mbar <- rhom2 * (1 - r2m2 - (p * (1 - p) * esa^2) / rhom2)
    sig2mbar <- (1 - rhom2) * (1 - r2m1)
    t2ybar <- rho2 * (1 - r22) - p * (1 - p) * (esa * esB + escp)^2 -
      ((1 / (p * (1 - p))) * esB^2 * rhom2 * (1 - r2m2) +
         (1 / (p * (1 - p))) * esB^2 * (1 - rhom2) * (1 - r2m1) / n - esa^2 * esB^2) / (1 / (p * (1 - p)))
    sig2ybar <- (1 - rho2) * (1 - r21 - (((1 - rhom2) / (1 - rho2)) * esb1^2 * (1 - r2m1)))
    var.B211 <- (t2ybar + sig2ybar / n) / (J * (t2mbar + sig2mbar / n))
    if(is.nan(var.B211) | var.B211 <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.B211)))
  }

  dfa <- J - 4
  dfb1 <- n * J - 3
  dfB <- J - 5
  df <- rbind(dfa, dfb1, dfB)
  colnames(df) <- "df"
  rownames(df) <- c("a", "b1", "B")

  sea211 <- .se.a211(esa = esa, rhom2 = rhom2, r2m1 = r2m1, r2m2 = r2m2, n = n, J = J, p = p)
  seb1211 <- .se.b1211(esb1 =esb1, rho2 = rho2, rhom2 = rhom2, r21 = r21, r2m1 = r2m1, n = n, J = J)
  seB211 <- .se.B211(esa = esa, esB = esB, esb1 = esb1, escp = escp, rho2 = rho2, rhom2 = rhom2, r22 = r22,
                     r21 = r21, r2m2 = r2m2, r2m1 = r2m1, n = n, J = J, p = p)
  ncpa <- esa/sea211
  ncpb1 <- esb1/seb1211
  ncpB <- esB/seB211
  ncp <- rbind(ncpa, ncpb1, ncpB)
  colnames(ncp) <- "ncp"
  rownames(ncp) <- c("a", "b1", "B")

  seb2211 <- sqrt(seB211^2 + seb1211^2)

  powera <- .power.fun(es = esa, alpha = alpha, sse = sea211, two.tailed = two.tailed, df = dfa)
  powerb1 <- .power.fun(es = esb1, alpha = alpha, sse = seb1211, two.tailed = two.tailed, df = dfb1)
  powerB <- .power.fun(es = esB, alpha = alpha, sse = seB211, two.tailed = two.tailed, df = dfB)
  power.sobel.ab1 <- .power.sobel(x = esa, y =esb1, sex = sea211, sey = seb1211, alpha = alpha, two.tailed = two.tailed)
  power.sobel.ab2 <- .power.sobel(x = esa, y =esB-esb1, sex = sea211, sey = seb2211, alpha = alpha, two.tailed = two.tailed)
  power.sobel.aB <- .power.sobel(x = esa, y =esB, sex = sea211, sey = seB211, alpha = alpha, two.tailed = two.tailed)
  power.joint.ab1 <- .power.jt(x = esa, y =esb1, sex = sea211, sey = seb1211, alpha = alpha, dfx = dfa, dfy = dfb1, two.tailed = two.tailed)
  power.joint.ab2 <- .power.jt(x = esa, y =esB-esb1, sex = sea211, sey = seb2211, alpha = alpha, dfx = dfa, dfy = dfB, two.tailed = two.tailed)
  power.joint.aB <- .power.jt(x = esa, y =esB, sex = sea211, sey = seB211, alpha = alpha, dfx = dfa, dfy = dfB, two.tailed = two.tailed)
  power.mc.ab1 <- ifelse(mc, .power.mc(nsims = nsims, ndraws = ndraws, x = esa, y =esb1, sex = sea211, sey = seb1211, alpha = alpha, two.tailed = two.tailed), NA)
  power.mc.ab2 <- ifelse(mc, .power.mc(nsims = nsims, ndraws = ndraws, x = esa, y =esB-esb1, sex = sea211, sey = seb2211, alpha = alpha, two.tailed = two.tailed), NA)
  power.mc.aB <- ifelse(mc, .power.mc(nsims = nsims, ndraws = ndraws, x = esa, y =esB, sex = sea211, sey = seB211, alpha = alpha, two.tailed = two.tailed), NA)

  power <- rbind(
    c(round(powera, 3), NA, NA, NA),
    c(round(powerb1, 3), NA, NA, NA),
    c(round(powerB, 3), NA, NA, NA),
    c(NA, round(power.sobel.ab1, 3), round(power.joint.ab1, 3), round(power.mc.ab1, 3)),
    c(NA, round(power.sobel.ab2, 3), round(power.joint.ab2, 3), round(power.mc.ab2, 3)),
    c(NA, round(power.sobel.aB, 3), round(power.joint.aB, 3), round(power.mc.aB, 3))
  )
  colnames(power) <- c("t", "sobel", "joint", "mc")
  rownames(power) <- c("a", "b1", "B", "ab1", "ab2", "aB")

  power.out <- list(fun = "power.med211",
                    parms = list(esa=esa, esb1=esb1, esB=esB, escp=escp,
                                 two.tailed=two.tailed, alpha=alpha,
                                 mc=mc, nsims=nsims, ndraws=ndraws,
                                 rho2=rho2, rhom2=rhom2, r21=r21, r22=r22, r2m1 = r2m1, r2m2=r2m2,
                                 p=p, n=n, J=J),
                    df = df,
                    ncp = ncp,
                    power = round(power, 3))
  cat("Statistical power: \n")
  cat("------------------------------------ \n")
  print(power)
  cat("------------------------------------ \n")
  cat("Degrees of freedom for path a:", dfa,
      "\nDegrees of freedom for path b1:", dfb1,
      "\nDegrees of freedom for path B:", dfB,
      "\nStandardized standard error for path a:", round(sea211, 3),
      "\nStandardized standard error for path b1:", round(seb1211, 3),
      "\nStandardized standard error for path B:", round(seB211, 3),
      "\nType I error rate:", alpha,
      "\nTwo-tailed test:", two.tailed, "\n")
  class(power.out) <- c("power", "med211")
  return(invisible(power.out))
}

# example
# power.med211(esa=.6, esB=.5, esb1=.1, escp=.1, rhom2=.3, rho2=.3, r22=.6, r21=.6, r2m2=.6, r2m1=.6, n=30, J=80, p=.1)

# statistical power
power.med221 <- function(esa, esb, escp, two.tailed = TRUE, alpha = .05,
                         mc = FALSE, nsims = 1000, ndraws = 1000,
                         rho2, r22, r21, r2m2,
                         p = .50, n, J) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  # standard errors for 2-2-1 mediation
  .se.a221 <- function(esa, r2m2, p, J) {
    var.a221 <- (1-(r2m2 + p * (1 - p) * esa^2)) / (p * (1 - p) * J)
    if(is.nan(var.a221) | var.a221 <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    return(invisible(sqrt(var.a221)))
  }

  .se.b221 <- function(esa, esb, escp, rho2, r22, r21, r2m2, p, n, J) {
    var.b221 <- (rho2 * (1 - (r22 + p * (1 - p) * (esa * esb + escp)^2 / rho2 + (esb^2 / rho2) * (1 - r2m2 - p * (1 - p) * esa^2))) +
                   (1 - rho2) * (1 - r21) / n) / (J * (1 - (r2m2 + p * (1 - p) * esa^2)))
    if(is.nan(var.b221) | var.b221 <= 0) {
      stop("Design is not feasible", call. = FALSE)
    }
    sqrt(var.b221)
  }

  dfa <- dfb <- J - 4
  df <- rbind(dfa, dfb)
  colnames(df) <- "df"
  rownames(df) <- c("a", "b")

  sea221 <- .se.a221(esa = esa, r2m2 = r2m2, p = p, J = J)
  seb221 <- .se.b221(esa = esa, esb = esb, escp = escp, rho2 = rho2, r22 = r22, r21 = r21, r2m2 = r2m2, p = p, n = n, J = J)

  ncpa <- esa/sea221
  ncpb <- esb/seb221
  ncp <- rbind(ncpa, ncpb)
  colnames(ncp) <- "ncp"
  rownames(ncp) <- c("a", "b")

  powera <- .power.fun(es = esa, alpha = alpha, sse = sea221, two.tailed = two.tailed, df = dfa)
  powerb <- .power.fun(es = esb, alpha = alpha, sse = seb221, two.tailed = two.tailed, df = dfb)
  power.sobel.ab <- .power.sobel(x = esa, y =esb, sex = sea221, sey = seb221, alpha = alpha, two.tailed = two.tailed)
  power.joint.ab <- .power.jt(x = esa, y =esb, sex = sea221, sey = seb221, alpha = alpha, dfx = dfa, dfy = dfb, two.tailed = two.tailed)
  power.mc.ab <- ifelse(mc, .power.mc(nsims = nsims, ndraws = ndraws, x = esa, y =esb, sex = sea221, sey = seb221, alpha = alpha, two.tailed = two.tailed), NA)
  power <- rbind(
    c(round(powera, 3), NA, NA, NA),
    c(round(powerb, 3), NA, NA, NA),
    c(NA, round(power.sobel.ab, 3), round(power.joint.ab, 3), round(power.mc.ab, 3))
  )
  colnames(power) <- c("t", "sobel", "joint", "mc")
  rownames(power) <- c("a", "b", "ab")

  power.out <- list(fun = "power.med221",
                    parms = list(esa=esa, esb=esb, escp=escp, two.tailed=two.tailed, alpha=alpha,
                                 mc=mc, nsims=nsims, ndraws=ndraws,
                                 rho2=rho2, r22=r22, r21=r21, r2m2=r2m2,
                                 p=p, n=n, J=J),
                    df = df,
                    ncp = ncp,
                    power = power)
  cat("Statistical power: \n")
  cat("------------------------------------ \n")
  print(power)
  cat("------------------------------------ \n")
  cat("Degrees of freedom for path a:", dfa,
      "\nDegrees of freedom for path b:", dfb,
      "\nStandardized standard error for path a:", round(sea221, 3),
      "\nStandardized standard error for path b:", round(seb221, 3),
      "\nType I error rate:", alpha,
      "\nTwo-tailed test:", two.tailed, "\n")
  class(power.out) <- c("power", "med221")
  return(invisible(power.out))
}
# example
# power.med221(esa=.3, esb=.1, escp=.1, rho2=.3, r22=.6, r21=.6, r2m2=.6, n=30, J=80, p=.1, mc=TRUE)

mrss.cra2r2 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                        n, J0=10, tol=.10,
                        rel1=1, rho2, g2=0, p=.50, r21=0, r22=0) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    df <- J0-g2-2
    if(df<= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    J1 <- (M/es)^2 * (rho2*(1-r22)/(p*(1-p)) +
                          (1-rho2)*(1-r21)/(p*(1-p)*n*rel1))
    if(abs(J1-J0)<tol){conv <- TRUE}
    J0 <- (J1+J0)/2
    i <- i+1
  }
  J <- ifelse(df>0,round(J0),NA)

  mrss.out <-  list(fun = "mrss.cra2r2",
                    parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                 n=n, J0=J0, tol=tol,
                                 rel1=rel1, rho2=rho2,
                                 p=p, r21=r21, r22=r22, g2=g2),
                    df = df,
                    ncp = M,
                    J = J)
  class(mrss.out) <- c("main", "mrss")
  cat("J =", J, "\n")
  return(invisible(mrss.out))
}
# example
# mrss.cra2r2(rho2=.20, n=4)
mrss.cra2 <- mrss.cra2r2

mrss.mod221 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                              n, J0=10, tol=.10, rho2, omegam2, r21=0, r2m2=0,
                              p=.50, q=NULL) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    if(omegam2 == 0 || r2m2 == 1) {
      df <- n*J0 - J0 - 2
    } else {
      df <- J0 - 2
    }
    if(df <= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    J1 <- ifelse(is.null(q),
                 (M/es)^2 * (rho2*omegam2*(1-r2m2)/(p*(1-p)) +
                   (1-rho2)*(1-r21)/(p*(1-p)*n)), # continuous mod
                 (M/es)^2 * (rho2*omegam2*(1-r2m2)/(p*(1-p)) +
                   (1-rho2)*(1-r21)/(p*(1-p)*q*(1-q)*n)) # binary mod
    )

    if(abs(J1-J0)<tol){conv <- TRUE}
    J0 <- (J1+J0)/2
    i <- i+1
  }
  J <- ifelse(df>0,round(J0),NA)

  mrss.out <-  list(fun = "mrss.mod221",
                    parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                 n=n, J0=J0, tol=tol, rho2=rho2, omegam2=omegam2,
                                 r21=r21, r2m2=r2m2, p=p, q=q),
                    df = df,
                    ncp = M,
                    J = J)

  if(omegam2 == 0 || r2m2 == 1) {
    df <- n*J - J - 2
    cat("\nModerator effect: Non-randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"),
        "\nJ =", J)
  } else {
    df <- J - 2
    cat("\nModerator effect: Randomly varying \nModerator type:",
        ifelse(is.null(q), "Continuous\n", "Binary\n"),
        "\nJ =", J)
  }

  class(mrss.out) <- c("mod221", "mrss")
  return(invisible(mrss.out))
}
# example
# mrss.mod221(rho2=.20, omegam2=0.2, q=.4, n=4)

mrss.mod222 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                             n, J0=10, tol=.10, rho2, r22=0,
                             p=.50, q=NULL) {

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    df <-  J0 - 5
    if(df<= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    J1 <- ifelse(is.null(q),
                 5 + (M/es)^2 * (rho2*(1-r22)/(p*(1-p)) +
                   (1-rho2)/(p*(1-p)*n)), # continuous mod
                 5 + (M/es)^2 * (rho2*(1-r22)/(p*(1-p)*q*(1-q)) +
                   (1-rho2)/(p*(1-p)*q*(1-q)*n)) # binary mod
    )

    if(abs(J1-J0)<tol){conv <- TRUE}
    J0 <- (J1+J0)/2
    i <- i+1
  }
  J <- ifelse(df>0,round(J0),NA)

  mrss.out <-  list(fun = "mrss.mod222",
                    parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                 n=n, J0=J0, tol=tol, rho2=rho2, r22=r22, p=p, q=q),
                    df = df,
                    ncp = M,
                    J = J)
  class(mrss.out) <- c("mod222", "mrss")
  cat(ifelse(is.null(q), "\nModerator type: Continuous\n", "\nModerator type: Binary\n"),
      "\nJ =", J)
  return(invisible(mrss.out))
}
# example
# mrss.mod222(rho2=.20,  q=.40, n=4)

mdes.bcra3f2 <- function(power=.80, alpha=.05, two.tailed = TRUE,
                         rho2, p=.50, g2=0, r21=0, r22=0,
                         n, J, K){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- K * (J - 2) - g2
  SSE = sqrt(rho2*(1-r22)/(p*(1-p)*J*K) +
               (1-rho2)*(1-r21)/(p*(1-p)*J*K*n))

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.bcra3f2",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2=rho2,
                                p=p, r21=r21, r22=r22, g2=g2,
                                n=n, J=J, K=K),
                   df=df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}

# example
# mdes.bcra3f2(rho2=.10, n=20, J=44, K=5)

power.bcra3f2 <- function(es=.25, alpha=.05, two.tailed=TRUE,
                          rho2, p=.50, g2=0, r21=0, r22=0,
                          n, J, K){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  df <- K * (J - 2) - g2
  SSE <- sqrt(rho2*(1-r22)/(p*(1-p)*J*K) +
                (1-rho2)*(1-r21)/(p*(1-p)*J*K*n))

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.bcra3f2",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                  rho2=rho2,
                                  p=p, r21=r21, r22=r22, g2=g2,
                                  n=n, J=J, K=K),
                     df=df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# example
# power.bcra3f2(rho2=.10, n=20, J=44, K=5)

mrss.bcra3f2 <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE,
                         n, K, J0=10, tol=.10,
                         rho2, p=.50, g2=0, r21=0, r22=0){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  i <- 0
  conv <- FALSE
  while(i<=100 & conv==FALSE){
    df <- K*(J0-2)-g2
    if(df<= 0 | is.infinite(df)){break}
    T1 <- ifelse(two.tailed==TRUE,abs(qt(alpha/2,df)),abs(qt(alpha,df)))
    T2 <- abs(qt(power,df))
    M <- ifelse(power>=.5,T1+T2,T1-T2)
    J1 <- (M/es)^2 * (rho2*(1-r22)/(p*(1-p)*K) +
                        (1-rho2)*(1-r21)/(p*(1-p)*K*n))
    J0 <- (J1+J0)/2
    i <- i+1
  }
  J <- ifelse(df>0,round(J0),NA)

  mrss.out <-  list(fun = "mrss.bcra3f2",
                    parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                                 n=n, K=K, J0=J0, tol=tol,
                                 rho2=rho2,
                                 p=p, r21=r21, r22=r22, g2=g2),
                    df=df,
                    ncp = M,
                    J = J)
  class(mrss.out) <- c("main", "mrss")
  cat("J =", J, "(per block)\n")
  return(invisible(mrss.out))
}
# example
# mrss.bcra3f2(rho2=.10, n=10, K=3)

mdes.cra2_pn <- function(power=.80, alpha=.05, two.tailed=TRUE, df=NULL,
                         rho2_trt=.20, rho_ic=0, p=.50, r21=0, n, J, ic_size=1){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(ic_size == 1 & rho_ic != 0) {
    rho_ic <- 0
    warning("Forcing 'rho_ic = 0'", call. = FALSE)
  } else  if(ic_size > 1 & rho_ic == 0) {
    warning("'rho_ic = 0'?", call. = FALSE)
  }

  # Satterthwaite (1946) approximation
  # assuming equal level 1 variance for treatment and control groups
  if(is.null(df)) {
    q <- 1 - p
    it <- n / ic_size
    xt <- rho2_trt + rho_ic / it + (1 - rho2_trt - rho_ic) / (it * ic_size)
    xc <- rho2_trt + (1 - rho2_trt - rho_ic) / n # n -> nc
    df <- (xc/q + xt/p)^2 / (xc^2 / (q^2 * (J*q - 1)) + xt^2 / (p^2 *(J*p - 1)))
  }

  deff_rand_ic <- 1 + (rho2_trt * (n - 1) + rho_ic * (1 - p) * (ic_size - 1)) / (1 - p * rho_ic)
  SSE <- sqrt(((1 - r21) / (J * n * p * (1 - p))) * ((1 - p * rho_ic) / (1 - rho_ic)) * deff_rand_ic )

  mdes <- .mdes.fun(power = power, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.mdes(effect = "main", power = power, alpha = alpha, sse = SSE,
                        df = round(df,3), two.tailed = two.tailed, mdes = mdes)
  mdes.out <- list(fun = "mdes.cra2_pn",
                   parms = list(power=power, alpha=alpha, two.tailed=two.tailed,
                                rho2_trt=rho2_trt, rho_ic=rho_ic, r21=r21, p=p, n=n, J=J, ic_size=ic_size),
                   df = df,
                   ncp = mdes[1]/SSE,
                   mdes = mdes)
  class(mdes.out) <- c("main", "mdes")
  return(invisible(mdes.out))
}
# constructed data example 4.1.2 (Lohr, Schochet, Sanders, 2014, p. 76 - 82)
# mdes.cra2_pn(rho2_trt=.15, rho_ic = .10, n=40, J = 70, ic_size = 10, r21=0, df= Inf)
# Satterthwaite df is slighlty overestimated (64.2 in Lohr et al., 67.7 below)
# mdes.cra2_pn(rho2_trt=.15, rho_ic = .10, n=40, J = 70, ic_size = 10, r21=0)

power.cra2_pn <- function(es=.25,alpha=.05, two.tailed=TRUE, df=NULL,
                          rho2_trt=.20, rho_ic=0, p=.50, r21=0, n, J, ic_size=1){

  user.parms <- as.list(match.call())
  .error.handler(user.parms)

  if(ic_size == 1 & rho_ic != 0) {
    rho_ic <- 0
    warning("Forcing 'rho_ic = 0'", call. = FALSE)
  } else  if(ic_size > 1 & rho_ic == 0) {
    warning("'rho_ic = 0'?", call. = FALSE)
  }

  # Satterthwaite (1946) approximation
  # assuming equal level 1 variance for treatment and control groups
  if(is.null(df)) {
    q <- 1 - p
    it <- n / ic_size
    xt <- rho2_trt + rho_ic / it + (1 - rho2_trt - rho_ic) / (it * ic_size)
    xc <- rho2_trt + (1 - rho2_trt - rho_ic) / n # n -> nc
    df <- (xc/q + xt/p)^2 / (xc^2 / (q^2 * (J*q - 1)) + xt^2 / (p^2 *(J*p - 1)))
  }

  deff_rand_ic <- 1 + (rho2_trt * (n - 1) + rho_ic * (1 - p) * (ic_size - 1)) / (1 - p * rho_ic)
  SSE <- sqrt(((1 - r21) / (J * n * p * (1 - p))) * ((1 - p * rho_ic) / (1 - rho_ic)) * deff_rand_ic )

  power <- .power.fun(es = es, alpha = alpha, sse = SSE, df = df, two.tailed = two.tailed)
  .summ.power(power = power, alpha = alpha, sse = SSE, df = round(df,3), two.tailed = two.tailed, es = es)
  power.out <-  list(fun = "power.cra2_pn",
                     parms = list(es=es, alpha=alpha, two.tailed=two.tailed,
                                  rho2_trt=rho2_trt, rho_ic=rho_ic, r21=r21, p=p, n=n, J=J, ic_size=ic_size),
                     df = df,
                     ncp = es/SSE,
                     power = power)
  class(power.out) <- c("main", "power")
  return(invisible(power.out))
}
# constructed data example 4.1.2 (Lohr, Schochet, Sanders, 2014, p. 76 - 82)
# power.cra2_pn(es=.30, rho2_trt=.15, rho_ic = .10, n=40, J = 70, ic_size = 10, r21=0, df= Inf)

mrss.cra2_pn  <- function(es=.25, power=.80, alpha=.05, two.tailed=TRUE, z.test=FALSE,
                          rho2_trt=.20, rho_ic=0, p=.50, r21=0, n, ic_size=1, J0=10, tol=.10){

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

    # Satterthwaite (1946) approximation
    # assuming equal level 1 variance for treatment and control groups
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

    deff_rand_ic <- 1 + (rho2_trt * (n - 1) + rho_ic * (1 - p) * (ic_size - 1)) / (1 - p * rho_ic)
    VAR <- ((1 - r21) / (n * p * (1 - p))) * ((1 - p * rho_ic) / (1 - rho_ic)) * deff_rand_ic

    J1 <- (M/es)^2 * VAR
    if(abs(J1-J0)<tol){conv <- TRUE}
    J0 <- (J1+J0)/2
    i <- i+1
  }

  n <- round(ifelse(df>0,round(n),NA))
  J <- round(ifelse(df>0,round(J0),NA))

  J.out <-  list(fun = "mrss.cra2_pn",
                 parms = list(es=es, power=power, alpha=alpha, two.tailed=two.tailed,
                              rho_ic = rho_ic, r21=r21, p=p, n=n, ic_size=ic_size,
                              J0=J0, tol=tol),
                 df=df,
                 ncp = M,
                 J = J)
  class(J.out) <- c("main", "mrss")
  cat("J =", J, "\n")
  return(invisible(J.out))
}
# constructed data example 4.1.2 (Lohr, Schochet, Sanders, 2014, p. 76 - 82)
# mrss.cra2_pn(es=.30, rho2_trt=.15, rho_ic = .10, n=40, ic_size = 10, r21=0, z.test = TRUE)
