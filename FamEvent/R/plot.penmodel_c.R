plot.penmodel_c <- function(x, agemax=80, print=TRUE, conf.int=FALSE, add.CIF=TRUE, MC=100,
                          col=c("blue","red","blue","red"), lty=c(1,1,2,2), 
                          xlab="Age at onset", ylab="Penetrance", ylim=NULL, ...){

  
  base.dist <- attr(x,"base.dist")
  frailty.dist <- attr(x, "frailty.dist")
  depend <- attr(x, "depend")
  agemin <- attr(x, "agemin")
  nbase <- attr(x, "nbase")
  cuts <- attr(x, "cuts")
  formula1 <- attr(x, "formula1")
  formula2 <- attr(x, "formula2")
  Y1 <- attr(x, "Y1")
  Y2 <- attr(x, "Y2")
  X1 <- attr(x, "X1")
  X2 <- attr(x, "X2")
  
  nk <- ifelse(is.null(frailty.dist)||frailty.dist=="none", 0, ifelse(frailty.dist%in%c("gamma","lognormal"), 2, 3))
  nb <- sum(nbase)
  nvar1 <- dim(X1)[2]
  nvar2 <- dim(X2)[2]
  
  est <- x$estimates
  base.est1 <- exp(est[1:nbase[1]])
  base.est2 <- exp(est[(nbase[1]+1):nb])
  if(base.dist[1]=="lognormal") base.est1[1] <- log(base.est1[1]) 
  if(base.dist[2]=="lognormal") base.est2[1] <- log(base.est2[1])
  base.est <- list(base.est1, base.est2)
  base.parms <- list(base.est1, base.est2)
  vbeta1 <- est[(nb+1):(nb+nvar1)]
  vbeta2 <- est[(nb+nvar1+1):(nb+nvar1+nvar2)]
  vbeta <- list(vbeta1, vbeta2)
  kappa <- exp(est[(nb+nvar1+nvar2+1):length(est)])
  age <- agemin:agemax
  ci1.11 <- penetrance_c(x, event=1, fixed=list(c(1,1), c(1,1)), age=age, CI=conf.int, MC=MC) #male-carrier
  ci1.01 <- penetrance_c(x, event=1, fixed=list(c(0,1), c(0,1)), age=age, CI=conf.int, MC=MC) #female-carrier
  ci1.10 <- penetrance_c(x, event=1, fixed=list(c(1,0), c(1,0)), age=age, CI=conf.int, MC=MC) #male-noncarrier
  ci1.00 <- penetrance_c(x, event=1, fixed=list(c(0,0), c(0,0)), age=age, CI=conf.int, MC=MC) #female-noncarrier

  ci2.11 <- penetrance_c(x, event=2, fixed=list(c(1,1), c(1,1)), age=age, CI=conf.int, MC=MC) #male-carrier
  ci2.01 <- penetrance_c(x, event=2, fixed=list(c(0,1), c(0,1)), age=age, CI=conf.int, MC=MC) #female-carrier
  ci2.10 <- penetrance_c(x, event=2, fixed=list(c(1,0), c(1,0)), age=age, CI=conf.int, MC=MC) #male-noncarrier
  ci2.00 <- penetrance_c(x, event=2, fixed=list(c(0,0), c(0,0)), age=age, CI=conf.int, MC=MC) #female-noncarrier
  
 variation <- ifelse(frailty.dist=="none" || is.null(frailty.dist), "none", "frailty")  

  if(add.CIF){
   data <- attr(x, "data")
   p <- data$proband==0
   ftime <- data$time[p]
   fstatus <- data$status[p]
   x1 <-X1[p,1] #gender
   x2 <-X1[p,2] #mgene
   group <- ifelse(x1==1&x2==1, "mc", ifelse(x1==1&x2==0, "mn", ifelse(x1==0&x2==1, "fc","fn")))
   xx <- cuminc(ftime, fstatus, group)
   col <- c("blue", "red", "blue", "red")
   
   par(mfrow=c(2,2))
   xpos <- "topleft"
   # event 1
   plot(age, ci1.11[,2], col=col[1], lty=1, type="l", ylab=ylab, xlab=xlab, 
        main="Event 1: Carriers", ylim=ylim)
   lines(age, ci1.01[,2], col=col[2], lty=1)
   lines(xx$`mc 1`$time, xx$`mc 1`$est, col=col[1], lty=1)   
   lines(xx$`fc 1`$time, xx$`fc 1`$est, col=col[2], lty=1)   
   legend(xpos, c("Male", "Female"), lty=1, col=c("blue", "red"), bty="n")
   if(conf.int){
      lines(age, ci1.11[,3], col=col[1], lty=3)
      lines(age, ci1.11[,4], col=col[1], lty=3)
      lines(age, ci1.01[,3], col=col[2], lty=3)
      lines(age, ci1.01[,4], col=col[2], lty=3)
   }
   
   plot(age, ci1.10[,2], col=col[3], lty=1, type="l", ylab=ylab, xlab=xlab, ylim=ylim, 
        main="Event 1: Noncarriers")
   lines(age, ci1.00[,2], col=col[4], lty=1)
   lines(xx$`mn 1`$time, xx$`mn 1`$est, col=col[3], lty=1)   
   lines(xx$`fn 1`$time, xx$`fn 1`$est, col=col[4], lty=1)   
   legend(xpos, c("Male", "Female"), lty=1, col=c("blue", "red"), bty="n")
   if(conf.int){
      lines(age, ci1.10[,3], col=col[3], lty=3)
      lines(age, ci1.10[,4], col=col[3], lty=3)
      lines(age, ci1.00[,3], col=col[4], lty=3)
      lines(age, ci1.00[,4], col=col[4], lty=3)
   }
   
   # event 2
   plot(age, ci2.11[,2], col=col[1], lty=1, type="l", ylab=ylab, xlab=xlab, ylim=ylim,
        main="Event 2: Carriers")
   lines(age, ci2.01[,2], col=col[2], lty=1)
   lines(xx$`mc 2`$time, xx$`mc 2`$est, col=col[1], lty=1)   
   lines(xx$`fc 2`$time, xx$`fc 2`$est, col=col[2], lty=1)   
   legend(xpos, c("Male", "Female"), lty=1, col=c("blue", "red"), bty="n")
   if(conf.int){
      lines(age, ci2.11[,3], col=col[1], lty=3)
      lines(age, ci2.11[,4], col=col[1], lty=3)
      lines(age, ci2.01[,3], col=col[2], lty=3)
      lines(age, ci2.01[,4], col=col[2], lty=3)
   }
   
   plot(age, ci2.10[,2], col=col[3], lty=1, type="l", ylab=ylab, xlab=xlab, ylim=ylim, 
        main="Event 2: Noncarriers")
   lines(age, ci2.00[,2], col=col[4], lty=1)
   lines(xx$`mn 2`$time, xx$`mn 2`$est, col=col[3], lty=1)   
   lines(xx$`fn 2`$time, xx$`fn 2`$est, col=col[4], lty=1)   
   legend(xpos, c("Male", "Female"), lty=1, col=c("blue", "red"), bty="n")
   if(conf.int){
      lines(age, ci2.10[,3], col=col[3], lty=3)
      lines(age, ci2.10[,4], col=col[3], lty=3)
      lines(age, ci2.00[,3], col=col[4], lty=3)
      lines(age, ci2.00[,4], col=col[4], lty=3)
   }
   
}   

   pen1 = cbind(ci1.11[ ,2], ci1.01[ ,2], ci1.10[ ,2], ci1.00[ ,2])
   pen2 = cbind(ci2.11[ ,2], ci2.01[ ,2], ci2.10[ ,2], ci2.00[ ,2])
   row.names(pen1) <- row.names(pen2) <- age
   colnames(pen1) <- colnames(pen2) <- c("Male-carrier", "Female-carrier", "Male-noncarrier", "Female-noncarrier")
   
   if(conf.int){
      lower1 = cbind(ci1.11[,3], ci1.01[,3], ci1.10[,3], ci1.00[,3])
      upper1 = cbind(ci1.11[,4], ci1.01[,4], ci1.10[,4], ci1.00[,4])
      colnames(upper1)<-colnames(lower1) <- c("Male-carrier", "Female-carrier","Male-noncarrier", "Female-noncarrier")
      lower2 = cbind(ci2.11[,3], ci2.01[,3], ci2.10[,3], ci2.00[,3])
      upper2 = cbind(ci2.11[,4], ci2.01[,4], ci2.10[,4], ci2.00[,4])
      colnames(upper2)<-colnames(lower2) <- c("Male-carrier", "Female-carrier","Male-noncarrier", "Female-noncarrier")
   }
   else{
      lower1 <- upper1 <- lower2 <- upper2 <- NULL
   }
   
   out <- list(coefficients=x$estimates, age=age, pen1=pen1, pen2=pen2, lower1=lower1, upper1=upper1, lower2=lower2, upper2=upper2)
   
 if(print){
   cat("Estimates: \n")
   print(x$estimates)
   cat("\nPenetrance (%) for event 1 by age 70: \n")
   print(pen1[age==70, ]*100)
   cat("\nPenetrance (%) for event 2 by age 70: \n")
   print(pen2[age==70, ]*100)
 }
 invisible(out)
}
