isinthecore <-
function(characteristic_function,allocation,game=c("profit","cost")){

cS<-characteristic_function
if (game=="profit"){cS<-cS*(-1)}


n<-log(length(cS),2)
if (n!=round(n)){
	cS<-c(0,cS)
	n<-log(length(cS),2)
}

coa<-coalitions(n)[[1]]

count<-0
for (i in 1:(2^n-1)){
	if(sum(allocation[which(coa[i+1,]==1)])<=cS[i+1]){
		count<-count+1
	} else {
		print ("The allocation is not in the core")
	}
}

if (count==(2^n-1)){print ("The allocation is in the core")}
return()}
