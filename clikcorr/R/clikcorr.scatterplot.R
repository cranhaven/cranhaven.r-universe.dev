splot <-
function(data, lower.list, upper.list, ti=ifelse(length(lower.list)>2,paste("Scatter plots of", lower.list[1], "to", lower.list[length(lower.list)]), paste("Scatter plot of", lower.list[1], "and", lower.list[2])), legend=TRUE,  cex=1.5, ...){
data.colnames <- colnames(data)
lower.list.len<- length(lower.list)
upper.list.len<- length(upper.list)
if(lower.list.len!=upper.list.len){stop(sprintf("the lower and upper lists are not equal in length!!"))}
else if(lower.list.len <2){stop(sprintf("need at least two variables for scatter plot!!"))}

if(lower.list.len==2){
	if(legend){
	par(mfrow=c(2, 1), oma=c(0,0,1,1), mar = c(2.1, 2.1, 0.1, 0.1))
	splot2(data, lower.list[1], upper.list[1], lower.list[2], upper.list[2], cex=cex, ...)
	title(main=ti, cex=4, font=2)
	legendplot()}
	else{
	splot2(data, lower.list[1], upper.list[1], lower.list[2], upper.list[2], cex=cex, ...)
	title(main=ti, cex=4, font=2)}
}
if(lower.list.len>2){
	par(mfrow=c(lower.list.len-1, lower.list.len-1), oma=c(3,3,1,1), mar = c(1.1, 1.1, 0.1, 0.1), mgp=c(2,0.7,0), xpd=NA)
	for(i in 2:lower.list.len){
		for(j in 1:(lower.list.len-1)){
			if(j<i){
			if(j==1 & i<lower.list.len){
			splot2(data, lower.list[j], upper.list[j], lower.list[i], upper.list[i], xlab="", ylab=lower.list[i], xaxt='n', cex=cex, ...)}
			else if(j>1 & i==lower.list.len){
			splot2(data, lower.list[j], upper.list[j], lower.list[i], upper.list[i], xlab=lower.list[j], ylab="", yaxt='n', cex=cex, ...)}
			else if(j>1 & i<lower.list.len){
			splot2(data, lower.list[j], upper.list[j], lower.list[i], upper.list[i], xlab="", xaxt='n',  ylab="", yaxt='n', cex=cex, ...)}	
			else{
			splot2(data, lower.list[j], upper.list[j], lower.list[i], upper.list[i], xlab=lower.list[j], ylab=lower.list[i], cex=cex, ...)}		
			}
			else{
				if(legend){
				if(i==2 & j==2) {legendplot()}			
				else {plot(1, type="n", axes=FALSE, xlab="", ylab="")} }
				else{plot(1, type="n", axes=FALSE, xlab="", ylab="")}
			}
		}
	}
	title(main=ti, cex=4, font=2, outer=TRUE)
}

}
