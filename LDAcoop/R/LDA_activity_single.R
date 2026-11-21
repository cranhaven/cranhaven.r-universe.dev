#' @title LDA_activity_single
#'
#' @description calculation of clonogenic activity from data collected by a
#'   limiting dilution assay (LDA) experiment (i.e. numbers of: cells seeded,
#'   wells, positive wells).
#'
#' @param x numeric data.frame or matrix with three columns (cells,
#'   wells, positive wells)
#' @param name optional: experiment name (e.g. name of cell line)
#' @param treat optional: treatment (e.g. irradiation dose in Gy)
#'
#' @return list object with estimated activity, 95%-confidence interval,
#'   84%-confidence interval, estimated parameters, corresponding covariance
#'   matrix, fit-object and p-value for cooperativity-test
#'
#' @examples
#' x <- data.frame("cells" = c(10,50,100,250),
#'                 "wells" = rep(25,4),
#'                 "positive" = c(2,5,10,20))
#' act <- LDA_activity_single(x)
#' data(LDAdata)
#' cell.line <- unique(LDAdata$name)[1]
#' x <- subset.data.frame(
#'       LDAdata,
#'       subset = (name==cell.line) & (Group == 0))
#' LDA_activity_single(x[,4:6])
#' @importFrom stats "glm" "binomial" "predict" "qnorm" "pnorm"
#' @export
#'
LDA_activity_single <- function(x,
                         name = "cell line a",
                         treat = "no") {
  if (!(class(x)[1] %in% c("data.frame","matrix"))){
    stop("error: x must be of class data.frame or matrix")
  }
  x <- as.data.frame(x)
  if (ncol(x) != 3){
    stop("error: number of columns must be 3 (cells, wells, positive)")
  }
  colnames(x) <- c("cells","wells","positive")
  if (!is.numeric(x$cells) | !is.numeric(x$wells) | !is.numeric(x$positive)){
    stop("error: all elements of x must be numeric")
  }
  if (min(x) < 0){
    stop("error: x must be non-negative")
  }
  if (min(x$cells) == 0){
    stop("error: cells must be positive")
  }
  if (sum(x$positive) == 0){
    stop("error: no positive wells, check experiment")
  }
  if (min(x$wells - x$positive) < 0){
    stop("error: number of wells must not be smaller than
         number of positive wells")
  }
  if (sum(aggregate.data.frame(
    x = x$positive>0,
    by = list(positiv = x$cells),
    FUN = sum)[,2] > 0) < 2){
    stop("error: at least two cell numbers with positive wells required.
          Remove condition from analysis or add wells and/or cell numbers.")
  }


  single <- data.frame("cells" = rep(NA,sum(x$wells)),"positive" = NA)
  flnr <- 1
  for (i in seq_along(rownames(x))){
    single$cells[flnr:(flnr+x$wells[i]-1)] <- x$cells[i]
    single$positive[flnr:(flnr+x$wells[i]-1)] <-
      c(rep(1,x$positive[i]),rep(0,x$wells[i]-x$positive[i]))
    flnr <- flnr + x$wells[i]
  }

  X.glm <- data.frame("y" = single$positive,
                      "x" = log(single$cells))
  fit.mod <- suppressWarnings(glm(y ~ x,
                 family = binomial(link = "cloglog"),
                 data = X.glm))

  sum.fit <- summary(fit.mod)
  est <- sum.fit$coefficients
  Sig <- sum.fit$cov.unscaled

  x.est <- exp(-est[1,1]/est[2,1])

  hl10 <- log10(x.est)
  R5.min <- seq(hl10 - 10, hl10 -  4, 1/10  )[-1]
  R4.min <- seq(hl10 -  4, hl10 -  3, 1/10  )[-1]
  R3.min <- seq(hl10 -  3, hl10 -  2, 1/33  )[-1]
  R2.min <- seq(hl10 -  2, hl10 -  1, 1/100 )[-1]
  R1.min <- seq(hl10 -  1, hl10 -1/10,1/333 )[-1]
  R0     <- seq(hl10 -1/10,hl10 +1/10,1/1000)[-1]
  R1.max <- seq(hl10 +1/10,hl10 +  1, 1/333 )[-1]
  R2.max <- seq(hl10 +  1, hl10 +  2, 1/100 )[-1]
  R3.max <- seq(hl10 +  2, hl10 +  3, 1/33  )[-1]
  R4.max <- seq(hl10 +  3, hl10 +  4, 1/10  )[-1]
  R5.max <- seq(hl10 +  4, hl10 + 10, 1/10  )[-1]
  Svec <- 10^c(R5.min,R4.min,R3.min,R2.min,R1.min,
            R0,
            R1.max,R2.max,R3.max,R4.max,R5.max)
  rm(R5.min,R4.min,R3.min,R2.min,R1.min,R0,R1.max,R2.max,R3.max,R4.max,R5.max)

  new.data <- data.frame("x" = log(Svec))
  pred <- predict(object = fit.mod,
                  newdata = new.data,
                  type = "response",
                  se.fit = TRUE)

  # approximate 95% confidence interval
  d.c <-  exp(new.data$x)

  calc_act_CI <- function(pred, alpha){
    x.uc <- (1-(pred$fit+qnorm(1-alpha/2)*pred$se.fit))
    x.lc <- (1-(pred$fit+qnorm(alpha/2)*pred$se.fit))
    x.lc[x.lc<0] <- 0
    x.lc[x.lc>1] <- 1
    x.uc[x.uc<0] <- 0
    x.uc[x.uc>1] <- 1
    x.m <- 1-pred$fit
    x.lc <- log(x.lc)
    x.uc <- log(x.uc)
    x.m <- log(x.m)

    #if(max(x.uc)<(-1)){
    #  x.s1 <- min(d.c)
    #} else {
      x.r <- d.c[which(x.uc<(-1))[1]]
      x.l <- d.c[which(x.uc<(-1))[1]-1]
      y.r <- x.uc[which(x.uc<(-1))[1]]
      y.l <- x.uc[which(x.uc<(-1))[1]-1]
      x.s1 <- x.l + (x.r-x.l) * (1+y.l)/(y.l-y.r)
      rm(x.r,x.l,y.r,y.l)
    #}
    # upper curve: x.lc 'coming from right'
    #if(min(x.lc)>(-1)){
    #  x.s2 <- Inf
    #} else {
      x.lc <- rev(x.lc)
      d.c <- rev(d.c)
      x.r <- d.c[which(x.lc>(-1))[1]]
      x.l <- d.c[which(x.lc>(-1))[1]-1]
      y.r <- x.lc[which(x.lc>(-1))[1]]
      y.l <- x.lc[which(x.lc>(-1))[1]-1]
      x.s2 <- x.l - abs(x.r-x.l) * abs(y.l+1)/abs(y.l-y.r)
      rm(x.r,x.l,y.r,y.l)
    #}

    return(as.numeric(c(x.s1,x.s2)))
  }
  CI.95 <- calc_act_CI(pred,0.05)
  # approximate 83.5% confidence interval
  CI.84 <- calc_act_CI(pred,0.165)

  # non-linearity
  b <- est[2,1]
  if (b < 1){
    p.b <- 2 * pnorm(q = b,mean = 1,sd = est[2,2])
  } else {
    p.b <- 2 * (1-pnorm(q = b,mean = 1,sd = est[2,2]))
  }

  results <- list("name" = name,
                  "treatment" = treat,
                  "act" = x.est,
                  "CI" = CI.95,
                  "CI.appr.SF" = CI.84,
                  "est" = est,
                  "Sigma" = Sig,
                  "model" = fit.mod,
                  "p.lin.Model" = p.b)
  class(results) <- "LDA_activity_object"
  return(results)
}
