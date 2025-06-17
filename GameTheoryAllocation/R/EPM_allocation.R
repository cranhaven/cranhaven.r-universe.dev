EPM_allocation <-
function(characteristic_function,r=NA,info=NA,game=c("profit","cost")){

cS<-characteristic_function
if (game=="profit"){cS<-cS*(-1)}

if (is.na(r)==TRUE){
	r=0.001
}

n<-log(length(cS),2)
if (n!=round(n)){
	cS<-c(0,cS)
	n<-log(length(cS),2)
}


epsilon=0
S=100
cont<-0
while(S!=0){
  
cont<-cont+1
n2<-factorial(n)/(factorial(n-2)*factorial(2))

coa<-coalitions(n)[[1]]
coa2<-which(apply(coa,1,sum)==2)
coa<-cbind(coa,cS)


EPM <- make.lp(0, n+1)
mat<-matrix(0,ncol=n+1)
obj<-rep(0,n+1);obj[1]<-1
set.objfn(EPM,obj)
for (i in 1:(n2+2^n-1)){
	R<-rep(0,n+1)
	if (i <=n2){
		R[1]<-1
		R_index<-which(coa[n+1+i,]==1)
		cost_index<-cS[R_index+1]
		R[R_index[1]+1]<--1/cost_index[1]
		R[R_index[2]+1]<-1/cost_index[2]
		add.constraint(EPM, R, ">=", 0)
		mat<-rbind(mat,R)
	}
	if (i > n2){
		j=i-n2+1
		R[2:(n+1)]<-coa[j,1:n]
		if (j<2^n){add.constraint(EPM, R, "<=", coa[j,n+1]+epsilon)}
		if (j==2^n){add.constraint(EPM, R, "=", coa[j,n+1])}
		mat<-rbind(mat,R)
	}
}

S=solve(EPM)
S0<-S
if (S!=0){
	epsilon=epsilon+r
}
if (cont>25){S=0;S0<-100}
}

if(S0!=100){
print ("EPM_allocation")
if (is.na(info)==F){
	if (epsilon==0){
		print ("The cost game has a non-empty core")
	} else { 
		print ("The cost game has an empty core")
		print (paste("EPM_allocation is in the epsilon-core, epsilon=",epsilon))
	}
}
EPM_allocation<-matrix(get.variables(EPM)[2:(n+1)],ncol=n)
colnames(EPM_allocation)<-1:n
rownames(EPM_allocation)<-" "
if (game=="profit"){EPM_allocation<-EPM_allocation*(-1)}
return(EPM_allocation)
} else {
  print("This problem has not a feasible solution. Review the input information.")
}
}
