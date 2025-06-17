Shapley_value <-
function(characteristic_function,game=c("profit","cost")){

cS<-characteristic_function

if (game=="profit"){cS<-cS*(-1)}

n<-log(length(cS),2)
if (n!=round(n)){
	cS<-c(0,cS)
	n<-log(length(cS),2)
}
v<-cS[-1]
f=factorial
coa<-coalitions(n)[[1]][-1,]
coaux<-coa
for (j in 1:(2^n-1)){
	for (i in 1:n){
		s<-length(which(coaux[j,]!=0))
		if (coa[j,i]==0){
			coa[j,i]=-(f(s)*f(n-s-1))/f(n)*v[j]
		} else {
			coa[j,i]=(f(s-1)*f(n-s))/f(n)*v[j]
		}
	}	
}
shapley<-apply(coa,2,sum)

if (game=="profit"){shapley<-shapley*(-1)}


shapley<-matrix(shapley,ncol=n)
colnames(shapley)<-1:n
rownames(shapley)<-" "
print("Shapley Value")
return(shapley)}
