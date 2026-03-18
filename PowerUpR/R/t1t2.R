# type I and type II error plot
.t1t2 <- function(ncp, df, alpha, two.tailed){

  # t-critical, power, beta
  talpha <- ifelse(two.tailed == FALSE,
                   qt(alpha, df, lower.tail = FALSE),
                   qt(alpha/2, df, lower.tail = FALSE)
  )
  power <- ifelse(two.tailed == FALSE,
                  1 - pt(talpha, df, ncp),
                  1 - pt(talpha, df, ncp) +
                    pt(-talpha, df, ncp)
  )
  beta = 1 - power

  # define functions
  funt0 <- function(x){
    dt(x, df = df, ncp = 0)
  } # central t
  funt1 <- function(x){
    dt(x, df = df, ncp = ncp)
  } #non-central t

  # plot central t distribution
  plot(funt0, xlim = c(-3,8), ylim = c(0, 0.5),
       yaxs = "i", xaxs = "i", bty = "l",
       sub = paste("Type I Error Rate = ", round(alpha ,digits = 3), ",",
                   "Type II Error Rate = ", round(beta, digits = 3), "\n",
                   "Non-centrality Parameter (NCP) =  ", round(ncp, digits = 3)),
       xlab = "", ylab = "")

  par(new = TRUE)

  # plot non-central t distribution
  plot(funt1, xlim = c(-3,8), ylim = c(0, 0.5),
       yaxs = "i", xaxs = "i", yaxt = "n", xaxt = "n",
       bty = "l", xlab = "", ylab = "")
  legend("topright",
         c(expression(alpha),
           expression(beta),
           ifelse(two.tailed == TRUE,
                  expression(t[alpha/2]),
                  expression(t[alpha])
           ),
           expression(NCP)),
         pch = c(19, 19, NA, NA), lty = c(NA, NA, 2, 2), cex = 1,
         col = c(adjustcolor(2, alpha.f = 0.3), adjustcolor(4, alpha.f = 0.3), 2, 4))

  # axes labels and subtitle
  title(ylab = "Probability Density", line = 2)
  title(xlab = paste0(expression(t), "(df = ", round(df, digits = 3), ")"), line = 2)

  # draw vertical lines
  abline(v = 0, lty = 2, col = 4) # mean of central t in dashed blue line
  abline(v = ncp, lty = 2, col = 4) # mean of non-central t in dashed blue line
  abline(v = talpha, lty = 2, col = 2) # t-critical in dashed red line

  # shaded area in red for alpha
  xalpha0 <- seq(from = talpha, to = 10, by = .001)
  yalpha0 <- rep(NA, length(xalpha0))
  for(i in 1:length(xalpha0)){
    yalpha0[i] <- funt0(xalpha0[i])
  }

  xalpha <- c(xalpha0, rev(xalpha0))
  yalpha <- c(yalpha0, rep(0, length(yalpha0)))
  polygon(x = xalpha, y = yalpha, col = adjustcolor(2, alpha.f = 0.3))

  # shaded area in light blue for beta
  xbeta0 <- seq(from = -3,to = talpha, by = .001)
  ybeta0 <- rep(NA, length(xbeta0))
  for(i in 1:length(xbeta0)){
    ybeta0[i] <- funt1(xbeta0[i])
  }
  xbeta <- c(xbeta0, rev(xbeta0))
  ybeta <- c(ybeta0, rep(0, length(ybeta0)))
  polygon(x = xbeta, y = ybeta, col = adjustcolor(4, alpha.f = 0.3))
}


# Type I  and Type II error plot wrapper
t1t2.error <- function(object){

  if(inherits(object, "mrss")){
    object <- mrss.to.power(object)
  }

  if(inherits(object, "med211")){
    ncpa <- object$ncp[1]
    ncpb1 <- object$ncp[2]
    ncpB <- object$ncp[3]
    dfa <- object$df[1]
    dfb1 <- object$df[2]
    dfB <- object$df[3]
    par(mfrow=c(3, 1))
    .t1t2(ncp = ncpa, df = dfa, two.tailed = object$parms$two.tailed, alpha =object$parms$alpha)
    title(main = "t-test for path 'a'")
    .t1t2(ncp = ncpb1, df = dfb1, two.tailed = object$parms$two.tailed, alpha =object$parms$alpha)
    title(main = "t-test for path 'b1'")
    .t1t2(ncp = ncpB, df = dfB, two.tailed = object$parms$two.tailed, alpha =object$parms$alpha)
    title(main = "t-test for path 'B'")
    par(mfrow=c(1, 1))
  } else if(inherits(object, "med221")){
    ncpa <- object$ncp[1]
    ncpb <- object$ncp[2]
    dfa <- object$df[1]
    dfb <- object$df[2]
    par(mfrow=c(2, 1))
    .t1t2(ncp = ncpa, df = dfa, two.tailed = object$parms$two.tailed, alpha =object$parms$alpha)
    title(main = "t-test for path 'a'")
    .t1t2(ncp = ncpb, df = dfb, two.tailed = object$parms$two.tailed, alpha =object$parms$alpha)
    title(main = "t-test for path 'b'")
  } else if(inherits(object, c("med311", "med321", "med331", "med_pn21", "med_pn31", "med_pn32"))){
    ncpa <- object$ncp[1]
    ncpB <- object$ncp[2]
    dfa <- object$df[1]
    dfB <- object$df[2]
    par(mfrow=c(2, 1))
    .t1t2(ncp = ncpa, df = dfa, two.tailed = object$parms$two.tailed, alpha =object$parms$alpha)
    title(main = "t-test for path 'a'")
    .t1t2(ncp = ncpB, df = dfB, two.tailed = object$parms$two.tailed, alpha =object$parms$alpha)
    title(main = "t-test for path 'B'")
  } else if(inherits(object, c("mod211", "mod212", "mod221", "mod222", "mod331", "mod332", "mod333"))){
    .t1t2(ncp = object$ncp,
          df = object$df,
          alpha = object$parms$alpha,
          two.tailed = object$parms$two.tailed)
    title(main = "t-test for moderator effect")
  } else {
    .t1t2(ncp = object$ncp,
          df = object$df,
          alpha = object$parms$alpha,
          two.tailed = object$parms$two.tailed)
    title(main = "t-test for treatment effect")
  }

  # change par() back to the default values
  par(mfrow=c(1, 1), mar=c(5.1, 4.1, 4.1, 2.1))

}

# examples
# design1 <- mdes.bcra4f3(rho3=.15, rho2=.15, n=10, J=4, L=27, K=4)
# design2 <- power.bcra4f3(rho3=.15, rho2=.15, n=10, J=4, L=27, K=4)
# t1t2.error(design1)
# t1t2.error(design2)

