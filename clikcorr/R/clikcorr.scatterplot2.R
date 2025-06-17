splot2 <-
function(data, lower1, upper1, lower2, upper2, pch=21, bg="cyan", xlab=lower1, ylab=lower2, ...){
data.colnames <- colnames(data)
pL1 <- match(lower1, data.colnames)
pL2 <- match(lower2, data.colnames)
pU1 <- match(upper1, data.colnames)
pU2 <- match(upper2, data.colnames)

M <- cbind(data[,pL1], data[,pU1], data[,pL2], data[,pU2])

xLB=min(c(M[,1], M[,2])[which(!is.na(c(M[,1], M[,2])))])
xUB=max(c(M[,1], M[,2])[which(!is.na(c(M[,1], M[,2])))])
yLB=min(c(M[,3], M[,4])[which(!is.na(c(M[,3], M[,4])))])
yUB=max(c(M[,3], M[,4])[which(!is.na(c(M[,3], M[,4])))])

Ar_x <- (xUB-xLB)/20
Ar_y <- (yUB-yLB)/20

xUB=xUB+3*Ar_x
yUB=yUB+3*Ar_y
xLB=xLB-1.5*Ar_x
yLB=yLB-1.5*Ar_y


Moo <- M[which(!is.na(M[,1]) & M[,1]==M[,2] & !is.na(M[,3]) & M[,3]==M[,4]), ]

ellipsis.args <- list(...)

plot(0,xlim=c(xLB, xUB), ylim=c(yLB, yUB), pch='', ylab=ylab, xlab=xlab, ...)

#plot(Moo[,1], Moo[,3], xlim=c(xLB, xUB), ylim=c(yLB, yUB), pch=pch, bg="#40E0D066", col="black", ...)

abline(h=yUB, lty=2, col="grey", xpd=FALSE)
abline(v=xUB, lty=2, col="grey", xpd=FALSE)
text((xLB+xUB)/2, yUB-Ar_y, paste(lower2,"missing"), col="grey")
text(xUB-Ar_x,(yLB+yUB)/2, paste(lower1,"missing"), srt=-90, col="grey")


n = dim(M)[1]

for(i in 1:n){
   if(is.na(M[i,1]) & is.na(M[i,2])){
	if(is.na(M[i,3]) & !is.na(M[i,4])){
		arrows(xUB, M[i,4], xUB, M[i,4]-Ar_y, length=0.06, code=2, col= "darkred", ...)}
	if(!is.na(M[i,3]) & is.na(M[i,4])){ 
		arrows(xUB, M[i,3], xUB, M[i,3]+Ar_y, length=0.06, code=2, col= "darkred", ...)}
	if(!is.na(M[i,3]) & !is.na(M[i,4]) & M[i,3]!=M[i,4]){ 
		lines(x=c(xUB, xUB), y=c(M[i,3], M[i,4]), col= "darkred", ...)}
	if(!is.na(M[i,3]) & !is.na(M[i,4]) & M[i,3]==M[i,4]){ 
    		text(xUB, M[i,3], "x",col= "darkred", font=2, ...)}
	if(is.na(M[i,3]) & is.na(M[i,4])){
    		set.seed(runif(1, min=0, max=100)) 
    		x0 <- xUB+0.5*runif(1, min=0, max=Ar_x)
    		y0 <- yUB+0.5*runif(1, min=0, max=Ar_y)
    		text(x0, y0, "x",col= "darkred", font=2, ...)}
  }

  if(!is.na(M[i,1]) & !is.na(M[i,2]) & M[i,1]!=M[i,2]){	
	if(is.na(M[i,3]) & !is.na(M[i,4])){ 
		arrows((M[i,1]+M[i,2])/2, M[i,4], (M[i,1]+M[i,2])/2, M[i,4]-Ar_y, length=0.06, code=2,  col= "darkblue", ...)
		lines(x=c(M[i,1], M[i,2]), y=c(M[i,4], M[i,4]), col= "darkblue", ...)}
	if(!is.na(M[i,3]) & is.na(M[i,4])){ 
		arrows((M[i,1]+M[i,2])/2, M[i,3], (M[i,1]+M[i,2])/2, M[i,3]+Ar_y, length=0.06, code=2, col= "darkblue", ...)
		lines(x=c(M[i,1], M[i,2]), y=c(M[i,3], M[i,3]), col= "darkblue", ...)}
	if(!is.na(M[i,3]) & !is.na(M[i,4]) & M[i,3]!=M[i,4]){ 
		rect(M[i,1], M[i,3], M[i,2], M[i,4], col= "lightblue", ...)}
	if(!is.na(M[i,3]) & !is.na(M[i,4]) & M[i,3]==M[i,4]){ 
		lines(x=c(M[i,1], M[i,2]), y=c(M[i,3], M[i,3]), col= "darkblue", ...)}
	if(is.na(M[i,3]) & is.na(M[i,4])){
		lines(x=c(M[i,1], M[i,1]), y=c(yUB, yUB), col= "darkred", ...)}
  }

  if(is.na(M[i,1]) & !is.na(M[i,2])){
	if(is.na(M[i,3]) & !is.na(M[i,4])){ 
		arrows(M[i,2], M[i,4], M[i,2]-Ar_x, M[i,4]-Ar_y, length=0.06, code=2, col= "darkblue", ...)}
  	if(!is.na(M[i,3]) & is.na(M[i,4])){ 	
		arrows(M[i,2], M[i,3], M[i,2]-Ar_x, M[i,3]+Ar_y, length=0.06, code=2, col= "darkblue", ...)}
  	if(!is.na(M[i,3]) & !is.na(M[i,4]) & M[i,3]!=M[i,4]){ 
		arrows(M[i,2], (M[i,3]+M[i,4])/2, M[i,2]-Ar_x, (M[i,3]+M[i,4])/2, length=0.06,  code=2, col= "darkblue", ...)
		lines(x=c(M[i,2], M[i,2]), y=c(M[i,3], M[i,4]), col= "darkblue", ...)}
  	if(!is.na(M[i,3]) & !is.na(M[i,4]) & M[i,3]==M[i,4]){ 
		arrows(M[i,2], M[i,3], M[i,2]-Ar_x, M[i,3], length=0.06,  code=2, col= "darkblue", ...)}
	if(is.na(M[i,3]) & is.na(M[i,4])){
		arrows(M[i,2], yUB, M[i,2]-Ar_x, yUB, length=0.06,   code=2, col= "darkred", ...)}
  }
  
  if(!is.na(M[i,1]) & is.na(M[i,2])){
	if(is.na(M[i,3]) & !is.na(M[i,4])){ 
		arrows(M[i,1], M[i,4], M[i,1]+Ar_x, M[i,4]-Ar_y, length=0.06, code=2, col= "darkblue", ...)}
  	if(!is.na(M[i,3]) & is.na(M[i,4])){ 
		arrows(M[i,1], M[i,3], M[i,1]+Ar_x, M[i,3]+Ar_y, length=0.06, code=2, col= "darkblue", ...)}
  	if(!is.na(M[i,3]) & !is.na(M[i,4]) & M[i,3]!=M[i,4]){ 
		arrows(M[i,1], (M[i,3]+M[i,4])/2, M[i,1]+Ar_x, (M[i,3]+M[i,4])/2, length=0.06,  code=2, col= "darkblue", ...)
		lines(x=c(M[i,1], M[i,1]), y=c(M[i,3], M[i,4]), col= "darkblue", ...)}
  	if(!is.na(M[i,3]) & !is.na(M[i,4]) & M[i,3]==M[i,4]){ 
		arrows(M[i,1], M[i,3], M[i,1]+Ar_x, M[i,3], length=0.06,  code=2, col= "darkblue", ...)}
	if(is.na(M[i,3]) & is.na(M[i,4])){
		arrows(M[i,1], yUB, M[i,1]+Ar_x, yUB, length=0.06,   code=2, col= "darkred", ...)}
  }

  if(!is.na(M[i,1]) & !is.na(M[i,2]) & M[i,1]==M[i,2]){	
	if(is.na(M[i,3]) & !is.na(M[i,4])){ 
		arrows(M[i,1], M[i,4], M[i,1], M[i,4]-Ar_y, length=0.06,  code=2, col= "darkblue", ...)}
	if(!is.na(M[i,3]) & is.na(M[i,4])){ 
		arrows(M[i,1], M[i,3], M[i,1], M[i,3]+Ar_y, length=0.06,  code=2, col= "darkblue", ...)}
	if(!is.na(M[i,3]) & !is.na(M[i,4]) & M[i,3]!=M[i,4]){ 
		lines(x=c(M[i,1], M[i,1]), y=c(M[i,3], M[i,4]), col= "darkblue", ...)}
	if(is.na(M[i,3]) & is.na(M[i,4])){
		text(M[i,1], yUB, "x",col= "darkred", font=2, ...)}
  }

}

if(!is.null(dim(Moo))){
points(Moo[,1], Moo[,3], xlim=c(xLB, xUB), ylim=c(yLB, yUB), pch=pch, bg=bg, col="black", ...)
}

}
