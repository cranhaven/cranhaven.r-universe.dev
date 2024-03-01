penplot <- function(base.parms, vbeta, cuts=NULL, variation="none", base.dist="Weibull", frailty.dist=NULL, depend=1, agemin=20, agemax=80, print=TRUE, col=c("blue","red","blue","red"), lty=c(1,1,2,2), add.legend=TRUE, add.title=TRUE, x="topleft", y=NULL, xlab="Age at onset", ylab="Penetrance", ylim=NULL, main=NULL, ...){

xage <- agemin:agemax

  if(agemin > 70) warning("agemin is set too high.")
  if(agemax < agemin) warning("agemax should be greater than agemin.")
t0 <- xage - agemin
cuts0 <- cuts - agemin
if(variation=="frailty"){
 if(!any(frailty.dist==c("lognormal",  "gamma")))  stop("Unrecognized frailty distribution; frailty.dist should be either \"gamma\" or \"lognormal\" ")
 else if(depend <= 0) stop("Invalid depend value; depend should be > 0.")
  
  if(length(vbeta)==2) xbeta <- c( vbeta %*% t(expand.grid(c(1,0),c(1,0)) ) )
  else if(length(vbeta)==3) xbeta <- c( vbeta %*% t(cbind(expand.grid(c(1,0),c(1,0)),c(1,0,0,0))) ) 
  else stop("vbeta should be a vector of length 2.")
  
} 
else if(variation=="secondgene") {
  if(length(vbeta)==3) xbeta <- c( vbeta %*% t(expand.grid(c(1,0),c(1,0), c(1,0))) )
  else if(length(vbeta)<3) stop("vbeta should include a second gene effect.")
  else stop("vbeta should be a vector of length 3.")
} 
else if(variation=="none") {
  if(!is.null(frailty.dist)) stop("frailty.dist should be NULL")
  if(length(vbeta)==2) xbeta <- c( vbeta %*% t(expand.grid(c(1,0),c(1,0)) ) )
  else if(length(vbeta)==3) xbeta <- c( vbeta %*% t(cbind(expand.grid(c(1,0),c(1,0)),c(1,0,0,0))) ) 
  else stop("vbeta should be a vector of length 2.")
}

s <- matrix(cumhaz(base.dist, t0, base.parms, cuts0))%*%exp(xbeta) #male-c, female-c, male-nc, female-nc

if(variation=="none" | variation=="secondgene") pen <- apply(s, 2, function(s) 1-exp(-s))
else if(variation=="frailty") pen <- apply(s, 2, function(dist,s,k) 1-laplace(dist,s,k), dist=frailty.dist, k=depend)
else stop("Unrecognized variation")
 
if(variation=="secondgene"){
par(mfrow=c(1,2))
  if(add.title){
    if(is.null(main)) main <- c(paste("Second gene carriers \n", base.dist, "baseline"),
                               paste("Second gene noncarriers \n", base.dist, "baseline"))
  }
  else main <- ""
  if(is.null(ylim)) ylim <- c(0, max(pen))
	plot(xage, pen[,1], ylab=ylab, xlab=xlab, type="l", lty=lty[1], col=col[1], ylim=ylim, main=main[1], ...)
  lines(xage, pen[,2], lty=lty[2], col=col[2], ...)
  lines(xage, pen[,3], lty=lty[3], col=col[3], ...)
  lines(xage, pen[,4], lty=lty[4], col=col[4], ...)
  if(add.legend) legend(x, y, c("male carrier", "female carrier", "male noncarrier", "female noncarrier"), bty="n", lty=lty, col=col)

  if(length(col)==4) col <- rep(col,2)
  if(length(lty)==4) lty <- rep(lty,2)
  
  plot(xage,pen[,5], ylab=ylab, xlab=xlab, type="l", lty=lty[5], col=col[5], ylim=ylim, main=main[2], ...)
  lines(xage, pen[,6], lty=lty[6], col=col[6], ...)
  lines(xage, pen[,7], lty=lty[7], col=col[7], ...)
  lines(xage, pen[,8], lty=lty[8], col=col[8], ...)
  if(add.legend) legend(x, y, c("male carrier", "female carrier", "male noncarrier", "female noncarrier"), bty="n", lty=lty, col=col)
  
}
else if(variation=="frailty"){
  par(mfrow=c(1,1))
  if(add.title){ 
    if(is.null(main)) main <- paste("Penetrance curves \n", base.dist, "baseline and ", frailty.dist,"frailty")
  }
  else main <- ""
  
  if(is.null(ylim)) ylim <- c(0, max(pen))
  plot(xage,pen[,1], ylab=ylab, xlab=xlab, type="l", lty=lty[1], col=col[1], ylim=ylim, main=main, ...)
  lines(xage, pen[,2], lty=lty[2], col=col[2], ...)
  lines(xage, pen[,3], lty=lty[3], col=col[3], ...)
  lines(xage, pen[,4], lty=lty[4], col=col[4], ...)
  if(add.legend) legend(x, y, c("male carrier", "female carrier", "male noncarrier", "female noncarrier"), bty="n", lty=lty, col=col)
}
else {
  par(mfrow=c(1,1))
  if(add.title){
    if(is.null(main)) main <- paste("Penetrance curves \n", base.dist, "baseline")
  }
  else main <- ""
  if(is.null(ylim)) ylim <- c(0, max(pen))
  plot(xage, pen[,1], ylab=ylab, xlab=xlab, type="l", lty=lty[1], col=col[1], main=main, ylim=ylim, ...)
  #plot(xage, pen[,1], type="l", lty=lty[1], col=col[1], ...)
  lines(xage, pen[,2], lty=lty[2], col=col[2], ...)
  lines(xage, pen[,3], lty=lty[3], col=col[3], ...)
  lines(xage, pen[,4], lty=lty[4], col=col[4], ...)
  if(add.legend) legend(x, y, c("male carrier", "female carrier", "male noncarrier", "female noncarrier"), bty="n", lty=lty, col=col)
}

pen70=pen[xage==70, 1:4]
names(pen70)<-c("male-carrier","female-carrier","male-noncarr","female-noncarr")

if(variation=="secondgene"){ 
  pen2 = pen[xage==70, 5:8]
  pen70 = rbind(pen70, pen2)
  row.names(pen70)<-c("secondgene=1","secondgene=0")
  colnames(pen) <- rep(c("male-carrier","female-carrier","male-noncarr","female-noncarr"),2)
  pen <- list('secondgene=1'=pen[,1:4], 'secondgene=0'=pen[,5:8])
  }
else colnames(pen) <- c("male-carrier","female-carrier","male-noncarr","female-noncarr")

if(print){
  if(variation=="frailty") cat("Call:", frailty.dist, "frailty with", base.dist, "baseline \n") 
  else  cat("Call:", base.dist, "baseline\n") 
cat("Penetrance by age 70:\n")
print(pen70)
}
invisible(list(pen70=pen70, x.age=xage, pen=pen))
}
