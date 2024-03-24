#' A right-censored example data
#'
#' A dataset containing the right-censored survival time and censor status for two groups of objects
#'
#' @format A data frame with 1217 rows and 3 variables:
#' \describe{
#'   \item{time}{right-censored time}
#'   \item{status}{object censored or not, with status=0 implies being censored.}
#'   \item{group}{Treatment group of the objects. By default, for a non-decreasing hazard ratio, S is the group on the numerator and T is the group on the denominator.}
#' }
"survData"

#' A dataframe containing computed chernoff distribution
#'
#' A dataframe containing computed chernoff distribution
#'
#' @format A data frame with 201 rows and 3 variables:
#' \describe{
#'   \item{xcoor}{domain on which chernoff distribution if computed}
#'   \item{DF}{Distribution function of the Chernoff distribution}
#'   \item{density}{Density of the Chernoff distribution}
#' }
"chernoff_realizations"

#' Estimate a non-decreasing hazard ratio function, together with a 100(1-\eqn{\alpha})\% confidence interval
#' @param time.grid A vector on which the hazard ratio function to be evaluated
#' @param S.data A dataframe containing observed survival time and censoring, it corresponds to the hazard function on the numerator
#' @param T.data A dataframe containing observed survival time and censoring, it corresponds to the hazard function on the demoninator
#' @param ci.lvl A number that specify the confidence level \eqn{\alpha}. Default is 0.05.
#' @import stats
#' @import KernSmooth
#' @import twostageTE
#' @import utils
#' @return \code{hr} The estimated hazard ratio
#' @return \code{tau} The estimated scaled parameter of the limiting Chernoff distribution
#' @return \code{ci.upper} and \code{ci.lower} are the upper bound and lower bound of the estimated confidence interval
#' @examples
#' # load the example data
#' data(survData)
#' # load the computed Chernoff distribution
#' data("chernoff_realizations")
#' # split the data into two groups S and T, make sure that the column of survival time
#' # is named as "time", and the column of censoring named as "status" (0 as being censored)
#' s.data <- survData[survData$group == 'S',]
#' t.data <- survData[survData$group == 'T',]
#' # define the evaluation grid on which the hazard ratio function is to be computed
#' t.grid <- seq(0, 10, 1)
#' # estimation and inference of the non-decreasing hazard ratio (defined as
#' #\eqn{\lambda_S}/\eqn{\lambda_T}, where \eqn{\lambda} is the hazard function) function
#' theta <- monotoneHR(t.grid, s.data, t.data)
#' @export

monotoneHR <- function(time.grid, S.data, T.data, ci.lvl=0.05){
  N <- dim(S.data)[1] + dim(T.data)[1]
  prop <- dim(S.data)[1] / N
  n.size <- N %/% 2

  Lambda.S <- NA.est(S.data)
  Lambda.T <- NA.est(T.data)

  Lambda.S.fn <- stepfun(Lambda.S$time, c(0, Lambda.S$cumhaz))
  Lambda.T.fn <- stepfun(Lambda.T$time, c(0, Lambda.T$cumhaz))

  mapped.t <- Lambda.T.fn(time.grid)

  #### inverse function of Lambda.T
  Lambda.T.inv <- sapply(Lambda.T$cumhaz, function(x) Lambda.T$time[min(which(Lambda.T$cumhaz >= x))])

  #### Lambda.S compose Lambda.T inverse
  LambdaS.LambdaT.inv <- Lambda.S.fn(Lambda.T.inv)
  logcm <- gcm.unique(Lambda.T$cumhaz, LambdaS.LambdaT.inv)

  #### left derivative of GCM
  left.deriv <- logcm$slope.knots
  left.deriv.time <- logcm$x.knots[2:length(logcm$y.knots)]

  smooth.x <- seq(0, max(Lambda.T$cumhaz), length.out = n.size^(2/3))
  #### monotone hazard ratio estimates
  smooth.y <- sapply(smooth.x, function(x) ifelse(x < min(left.deriv.time), 0, left.deriv[min(which(left.deriv.time >= x))]))

  # direct derivative estimation from primitive functions (with kernel smoothing)
  prim.drv.ker <- dpill(smooth.x, smooth.y)
  prim.drv.fit <- locpoly(smooth.x, smooth.y, bandwidth = prim.drv.ker, drv = 1)

  prim.drv.ker.indices <- unlist(sapply(mapped.t, function(x) which.min(abs(prim.drv.fit$x - x))))
  mapped.drv <- prim.drv.fit$y[prim.drv.ker.indices]

  pro.s <- sapply(time.grid, function(x) mean(S.data$time >= x))
  pro.t <- sapply(time.grid, function(x) mean(T.data$time >= x))

  theta.t <- sapply(mapped.t, function(x) ifelse(x < min(left.deriv.time), 0, left.deriv[min(which(left.deriv.time >= x))]))

  tau.hat <- (4 * mapped.drv * (theta.t / (prop*pro.s) + theta.t^2 / ((1- prop) * pro.t)))^(1/3)

  # confidence interval
  chnff.df <- (function(...)get(data(...,envir = new.env())))("chernoff_realizations")
  chernoff.cdf <- unique(c(rev(1 - chnff.df$DF), chnff.df$DF))
  chernoff.x <- unique(c(rev(-1 * chnff.df$xcoor), chnff.df$xcoor))

  inter.cher <- approx(chernoff.x, chernoff.cdf, xout = seq(-2, 2, 0.0001))

  ub <- 1 - ci.lvl / 2
  lb <- ci.lvl / 2
  x.ub <- inter.cher$x[which.min(abs(inter.cher$y - ub))]
  x.lb <- inter.cher$x[which.min(abs(inter.cher$y - lb))]

  ci.lower <- x.lb * tau.hat / N^(1/3) + theta.t
  ci.upper <- x.ub * tau.hat / N^(1/3) + theta.t

  return(list(
    tau = tau.hat,
    hr = theta.t,
    ci.upper = ci.upper,
    ci.lower = ci.lower
  ))
}


#' A function used to construct the Nelson-Aalen estimator.
#' @param surv.data A dataframe containing right-censored survival time and censor status.
#' @import survival
#' @return \code{est} The Nelson-Aalen object.
NA.est = function(surv.data){
  surv.model <- survfit(Surv(time, status) ~ 1, data=surv.data, ctype = 1)
  est = data.frame(surv.model$time, surv.model$cumhaz)
  if (all(est[1, ] != c(0, 0))) est <- rbind(c(0, 0), est)
  colnames(est) = c('time', 'cumhaz')
  return(est)
}

#' A function used to remove the repeated numbers for the computation of gcm/lcm.
#' @param x A vector corresponds to the independent variable.
#' @param y A vector corresponds to the dependent variable.
#' @import fdrtool
#' @return \code{logcm} GCM of the curve y~x.
gcm.unique <- function(x, y){
  x.unique <- unique(x)
  y.unique <- rep(NA, length(x.unique))
  for (i in 1:length(x.unique)){
    y.unique[i] <- y[max(which(x == x.unique[i]))]
  }
  #### GCM
  logcm <- gcmlcm(x.unique, y.unique, type = 'gcm')
  return(logcm)
}


person(c(person(given = "Yujian", family = "Wu", role = c("aut", "cre"), email = "yujianwu@umass.edu", comment = c(ORCID = "0000-0001-9163-6773")),
         c(person(given = "Ted", family = "Westling", role = c("ths"), email = "twestling@umass.edu", comment = c(ORCID = "0000-0002-3362-1378")))))
