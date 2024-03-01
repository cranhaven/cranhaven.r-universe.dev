penplot_c <- function(event, base.parms, vbeta, cuts=NULL, variation="none", base.dist="Weibull", 
                      frailty.dist=NULL, depend=c(1,1), agemin=20, agemax=80, print=TRUE, 
                      col=c("blue","red","blue","red"), lty=c(1,1,2,2), add.legend=TRUE, 
                      add.title=TRUE, x="topleft", y=NULL, xlab="Age at onset", ylab="Penetrance",
                      ylim=NULL, main=NULL, ...){

xage <- agemin:agemax

  if(agemin > 70) warning("agemin is set too high.")
  if(agemax < agemin) warning("agemax should be greater than agemin.")
  if(length(base.dist)==1) base.dist <- rep(base.dist,2)

t0 <- xage - agemin

vbeta1 <- vbeta[[1]]
vbeta2 <- vbeta[[2]]
base.parms1 <- base.parms[[1]]
base.parms2 <- base.parms[[2]]

if(variation == "none" & !is.null(frailty.dist)) stop("frailty.dist should be NULL when variation is none.")
if(variation == "frailty" & !any(frailty.dist==c("lognormal",  "gamma", "cgamma", "clognormal")))  stop("Unrecognized frailty distribution; frailty.dist should be either \"gamma\" or \"lognormal\" ")
else if(variation=="frailty" & any(depend <= 0)) stop("Invalid depend value; depend should be > 0.")

if(variation%in%c("frailty", "none")){
  if(length(vbeta1)==2) x1 <- expand.grid(c(1,0),c(1,0)) 
  else if(length(vbeta1)==3) x1 <- cbind(expand.grid(c(1,0),c(1,0)),c(1,0,0,0))  
  else stop("vbeta[[1]] should be a vector of length 2 or 3.")
  
  if(length(vbeta2)==2) x2 <- expand.grid(c(1,0),c(1,0)) 
  else if(length(vbeta2)==3) x2 <- cbind(expand.grid(c(1,0),c(1,0)),c(1,0,0,0)) 
  else stop("vbeta[[2]] should be a vector of length 2 or 3.")
} 
else if(variation=="secondgene") {
  if(length(vbeta1)==3 & length(vbeta2==3)) x1 <- x2 <- expand.grid(c(1,0),c(1,0), c(1,0)) 
  else stop("Each vbeta should be a vector of length 3.")
} 
else stop("Unrecognized variation")

if(variation=="none" | variation=="secondgene") frailty.dist <- "none"

pen <- matrix(0, ncol=dim(x1)[1], nrow=length(xage))
row.names(pen) <- xage

for(i in 1:dim(x1)[1]){
  pen[, i]<-penf_c(event=event, base.est=base.parms, vbeta=vbeta, kappa=depend, 
                   x=list(x1[i,], x2[i,]), age=xage, base.dist=base.dist, frailty.dist=frailty.dist, agemin=agemin, cuts=cuts)
}  

 
if(variation=="secondgene"){
par(mfrow=c(1,2))
  if(add.title){
    if(is.null(main)) main <- c("Second gene carriers ", "Second gene noncarriers")
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
  
  plot(xage, pen[,5], ylab=ylab, xlab=xlab, type="l", lty=lty[5], col=col[5], ylim=ylim, main=main[2], ...)
  lines(xage, pen[,6], lty=lty[6], col=col[6], ...)
  lines(xage, pen[,7], lty=lty[7], col=col[7], ...)
  lines(xage, pen[,8], lty=lty[8], col=col[8], ...)
  if(add.legend) legend(x, y, c("male carrier", "female carrier", "male noncarrier", "female noncarrier"), bty="n", lty=lty, col=col)
  
}
else if(variation=="frailty"){
  par(mfrow=c(1,1))
  if(add.title){ 
    if(is.null(main)) main <- paste("Penetrance curves for event", event)
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
    if(is.null(main)) main <- paste("Penetrance curves for event", event)
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
  f <- match(frailty.dist, c("gamma", "lognormal", "cgamma", "clognormal"))
  fname <- c("Gamma", "Log-normal", "Correlated Gamma", "Correlated log-normal")[f]
  
  if(variation=="frailty") cat("Call:", fname, "frailties with", 
                               base.dist[1], "for event 1 and", base.dist[2], 
                               "for event 2 baselines \n") 
  else  cat("Call:", base.dist[1], "for event 1 and", base.dist[2], 
            "for event 2 baselines \n") 
cat("Penetrance for event", event, "by age 70:\n")
print(pen70)
}
invisible(list(pen70=pen70, x.age=xage, pen=pen))
}
