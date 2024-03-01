
plot.penmodel <- function(x, agemax=80, print=TRUE, mark.time=FALSE, conf.int=FALSE, add.KM=TRUE, MC=100,
                          col=c("blue","red","blue","red"), lty=c(1,1,2,2), 
                          add.legend=TRUE, add.title=TRUE, xpos="topleft", ypos=NULL, xlab="Age at onset", 
                          ylab="Penetrance", ylim=NULL, main=NULL, ...){

  
  base.dist <- attr(x,"base.dist")
#  frailty.dist <- attr(x, "frailty.dist")
#  depend <- attr(x, "depend")
  agemin <- attr(x, "agemin")
  nbase <- attr(x, "nbase")
  cuts <- attr(x, "cuts")
  formula <- attr(x, "formula")
  parms <- exp(x$estimates[1:nbase])
  if(base.dist=="lognormal") parms[1] <- x$estimates[1]
  vbeta <- x$estimates[-c(1:nbase)]
  #names(vbeta)
 penout <- penplot(base.parms=parms, vbeta=vbeta, cuts=cuts, base.dist=base.dist, variation="none", 
                   frailty.dist=NULL, depend=1, agemin=agemin, agemax=agemax, print=FALSE, col=col,lty=lty, 
                   add.legend=FALSE, add.title=add.title, x=xpos,y=ypos, xlab=xlab,ylab=ylab, ylim=ylim, main=main, ...)

 penest<-t(penout$pen)

 if(add.KM){
   data <- attr(x, "data")
   sfit <- survfit(formula, data=data[data$proband==0,])
   lines(sfit, fun="event", mark.time=TRUE, conf.int=FALSE, col=col[c(4,2,3,1)], lty=lty[c(4,2,3,1)], ...)
 }
 if(conf.int){
   if(add.KM) lines(sfit, fun="event", conf.int="only", col=col[c(4,2,3,1)], lty=3, ...)
   cat("Calculating ... \n")
   xx = penout$x.age
   ci1 <- penetrance(x, c(1,1), xx, CI=TRUE, MC=MC) #male-carrier
   ci2 <- penetrance(x, c(0,1), xx, CI=TRUE, MC=MC) #female-carrier
   ci3 <- penetrance(x, c(1,0), xx, CI=TRUE, MC=MC) #male-noncarrier
   ci4 <- penetrance(x, c(0,0), xx, CI=TRUE, MC=MC) #female-noncarrier
   
   lines(xx, ci1[,3], col=col[1], lty=3)
   lines(xx, ci1[,4], col=col[1], lty=3)
   lines(xx, ci2[,3], col=col[2], lty=3)
   lines(xx, ci2[,4], col=col[2], lty=3)
   lines(xx, ci3[,3], col=col[3], lty=3)
   lines(xx, ci3[,4], col=col[3], lty=3)
   lines(xx, ci4[,3], col=col[4], lty=3)
   lines(xx, ci4[,4], col=col[4], lty=3)
   
   lower = cbind(ci1[,3], ci2[,3], ci3[,3], ci4[,3])
   upper = cbind(ci1[,4], ci2[,4], ci3[,4], ci4[,4])
   colnames(upper)<-colnames(lower) <- c("male-carrier", "female-carrier","male-noncarrier", "female-noncarrier")

   if(add.legend) legend(xpos, ypos, c("male carrier", "female carrier", "male noncarrier", "female noncarrier", "95% CI for males", "95% CI for females"), bty="n", lty=c(lty,3,3), col=col)

     out <- list(coefficients=x$estimates, pen70=penout$pen70, x.age=xx, pen=t(penest), lower=lower, upper=upper)
 }
 else {
   if(add.legend) legend(xpos, ypos, c("male carrier", "female carrier", "male noncarrier", "female noncarrier"), bty="n", lty=lty, col=col)
   out <- list(coefficients=x$estimates, pen70=penout$pen70, x.age=penout$x.age, pen=t(penest))
 }
 if(print){
   cat("Estimates: \n")
   print(out$coefficients)
   cat("\nPenetrance (%) by age 70: \n")
   print(out$pen70)
  }
 invisible(out)
}
