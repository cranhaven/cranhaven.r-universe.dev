nucleolus <-
function(characteristic_function,game=c("profit","cost")){

cS<-characteristic_function

if (game=="profit"){cS<-cS*(-1)}

n<-log(length(cS),2)
if (n!=round(n)){
	cS<-c(0,cS)
	n<-log(length(cS),2)
}


coa<-coalitions(n)[[1]]
coa2<-which(apply(coa,1,sum)==2)
coa<-cbind(coa,cS)[-1,]
nucleolus <- make.lp(0, n+1)
lp.control(nucleolus, sense="max")
obj<-rep(0,n+1);obj[n+1]<--1
set.objfn(nucleolus ,obj)
for (i in 1:(2^n-1)){
	R<-rep(0,n+1)
	R[1:n]<-coa[i,(1:n)]
	if (i<(2^n-1)){
			R[n+1]=-1
			add.constraint(nucleolus, R, "<=", coa[i,n+1])
	}
	if (i==(2^n-1)){add.constraint(nucleolus, R, "=", coa[i,n+1])}	
}

S=solve(nucleolus)

get.primal.solution(nucleolus)
nucleolus<-matrix(get.variables(nucleolus)[1:n],ncol=n)
colnames(nucleolus)<-1:n
rownames(nucleolus)<-" "
print("Nucleolus")
if (game=="profit"){nucleolus<-nucleolus*(-1)}

return(nucleolus)}
