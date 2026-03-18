# minimum detectable effect size
.mdes.fun <- function(power, alpha, sse, df, two.tailed){
  if(length(sse) == 0) {
    stop("Check minimum required arguments with no defaults", call. = FALSE)
  }
  if(length(sse) > 1 || !is.numeric(sse) || sse < 0 ||
     length(df) > 1 || !is.numeric(df) || df < 1) {
    stop("Design is not feasible", call. = FALSE)
  }

  t1 <- ifelse(two.tailed == TRUE, abs(qt(alpha / 2, df)), abs(qt(alpha, df)))
  t2 <- abs(qt(power, df))
  m <- ifelse(power >= 0.5, t1 + t2, t1 - t2)
  mdes <- m * sse
  lcl <- mdes * (1 - t1 / m)
  ucl <- mdes * (1 + t1 / m)
  mlu <- cbind(mdes, lcl, ucl)
  colnames(mlu) <- c("mdes", paste(100 * (1 - round(alpha, 2)), "% lcl", sep = ""),
                     paste(100 * (1 - round(alpha, 2)), "% ucl", sep = ""))
  return(invisible(mlu))
}

# statistical power
.power.fun <- function(es, alpha, sse, df, two.tailed){
  if(length(sse) == 0) {
    stop("Check minimum required arguments with no defaults", call. = FALSE)
  }
  if(length(sse) > 1 || !is.numeric(sse) || sse < 0 ||
     length(df) > 1 || !is.numeric(df) || df < 1) {
    stop("Design is not feasible", call. = FALSE)
  }
  lambda <- es/sse
  power <- ifelse(two.tailed == FALSE,
                  1 - pt(qt(alpha, df, lower.tail = FALSE), df, lambda),
                  1 - pt(qt(alpha / 2, df, lower.tail = FALSE), df, lambda) +
                    pt(-qt(alpha / 2, df, lower.tail = FALSE), df, lambda))
  return(invisible(power))
}

# power functions for indirect effects
.power.sobel <- function(x, y, sex, sey, alpha, two.tailed, df = 1e+8) {
  sesobel <- .se.sobel(x = x, y = y, sex = sex, sey = sey)
  power <- .power.fun(es = x*y, alpha = alpha, sse = sesobel, two.tailed = two.tailed, df = df)
  return(invisible(power))
}

.power.jt <- function(x, y, z = NULL, sex, sey, sez = NULL, alpha, two.tailed, dfx, dfy, dfz = NULL) {
  powerx <- .power.fun(es = x, alpha = alpha, sse = sex, two.tailed = two.tailed, df = dfx)
  powery <- .power.fun(es = y, alpha = alpha, sse = sey, two.tailed = two.tailed, df = dfy)
  power.jt <- powerx*powery
  if(!is.null(z) & !is.null(sez) & !is.null(dfz)) {
    powerz <- .power.fun(es = z, alpha = alpha, sse = sez, two.tailed = two.tailed, df = dfz)
    power.jt <- powerx*powery*powerz
  }
  return(invisible(power.jt))
}

.power.mc <- function(nsims, ndraws, x, y, z = NULL, sex, sey, sez = NULL, alpha, two.tailed) {
  rejmc <- NULL
  for (i in 1:nsims){
    xstar <- rnorm(1, x, sex)
    ystar <- rnorm(1, y, sey)
    rejmc <- c(rejmc, quantile(rnorm(ndraws, xstar, sex)*rnorm(ndraws, ystar, sey), probs = ifelse(two.tailed, alpha/2, alpha), na.rm = TRUE) > 0)

    if(!is.null(z) & !is.null(sez)) {
      zstar <- rnorm(1, z, sez)
      rejmc <- c(rejmc, quantile(rnorm(ndraws, xstar, sex)*rnorm(ndraws, ystar, sey)*rnorm(ndraws, zstar, sez),
                                 probs = ifelse(two.tailed, alpha/2, alpha), na.rm = TRUE) > 0)
    }

  }
  return(mean(rejmc))
}

# sobel standard error
.se.sobel <- function(x, y, sex, sey) {
  var.sobel <- (x^2 * sey^2  + y^2 * sex^2)
  if(is.nan(var.sobel) | var.sobel <= 0) {
    stop("Design is not feasible", call. = FALSE)
  }
  return(invisible(sqrt(var.sobel)))
}

# summarize mdes output
.summ.mdes <- function(effect, power, alpha, sse, df, two.tailed, mdes) {
  cat(ifelse(effect == "main",
             "\nMinimum detectable effect size: \n--------------------------------------- \n ",
             "\nMinimum detectable effect size difference: \n--------------------------------------- \n "
             ),
      round(mdes[1], 3), " ", 100 * (1 - round(alpha, 2)), "% CI [", round(mdes[2], 3),
      ",", round(mdes[3], 3), "]\n---------------------------------------\nDegrees of freedom: ", df,
      "\nStandardized standard error: ", round(sse, 3), "\nType I error rate: ", alpha,
      "\nType II error rate: ", round(1 - power, 3), "\nTwo-tailed test: ", two.tailed, "\n",
      sep = "")
}

# summarize power output
.summ.power <- function(es, alpha, sse, df, two.tailed, power) {
  cat("\nStatistical power: \n--------------------------------------- \n ",
      round(power, 3), "\n--------------------------------------- \nDegrees of freedom: ", df,
      "\nStandardized standard error: ", round(sse, 3), "\nType I error rate: ", alpha,
      "\nType II error rate: ", round(1 - power, 3), "\nTwo-tailed test: ", two.tailed, "\n",
      sep = "")
}



