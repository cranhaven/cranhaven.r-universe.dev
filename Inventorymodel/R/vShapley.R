vShapley <-
function(n,game){

f=factorial
coa<-coalitions(n)[[1]][-1,]
coaux<-coa
for (j in 1:(2^n-1)){
	for (i in 1:n){
		s<-length(which(coaux[j,]!=0))
		if (coa[j,i]==0){
			coa[j,i]=-(f(s)*f(n-s-1))/f(n)*game[j]
		} else {
			coa[j,i]=(f(s-1)*f(n-s))/f(n)*game[j]
		}
	}	
}
phi<-apply(coa,2,sum)

return(phi)}
