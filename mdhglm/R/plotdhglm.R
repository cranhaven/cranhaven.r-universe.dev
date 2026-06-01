plotdhglm <-
function (OUTPUT,type="mean",random=NULL) {
    random=NULL
    par(mfrow=c(2,2))
    if (type=="mean") {
	    mu<-OUTPUT[7][[1]]
	    StudentResidual<-OUTPUT[1][[1]]
    }
    if (type=="phi") {
	    mu<-OUTPUT[4][[1]]
	    StudentResidual<-OUTPUT[3][[1]]
    }
    if (type=="lambda") {
	    mu<-OUTPUT[6][[1]]
	    StudentResidual<-OUTPUT[5][[1]]
    }
    if (type=="v") {
	    mu<-OUTPUT[24][[1]]
	    StudentResidual<-OUTPUT[5][[1]]
    }
    x<-mu
    y<-StudentResidual
    fit<- supsmu(x,y)
    plot(x, y, main="Residuals vs Fitted", xlab="scaled fitted values", ylab="Studentized Residual", cex=0.5) #plot data point
    lines(fit$x, fit$y) #plot smooth spline fit
    y<-abs(StudentResidual)
    fit<- supsmu(x,y)
    plot(x, y, main="|Residuals| vs Fitted",xlab="scaled fitted values", ylab="|Studentized Residual|", cex=0.5) #plot data point
    lines(fit$x, fit$y) #plot smooth spline fit
    qqnorm(StudentResidual,main="Normal Probability Plot"); qqline(StudentResidual) # Normal probability plot
    hist(StudentResidual)
}
