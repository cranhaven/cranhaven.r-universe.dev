
coalitions<-function(n){ 
coa<-matrix(0,ncol=n,nrow=2^n-1)
coadef<-coa
classic<-c()
njug<-rep(0,2^n-1)
primerjug<-rep(0,2^n-1)
for (i in 1:2^n-1){
	xdigito<-array(0,dim=(n+1))
  	sw<-"TRUE"
  	a1<-i
  	ij<-0
  	while(ij<(n+1)){
    		ij<-ij+1
    		xdigito[ij]<-a1-as.integer(a1/2)*2
    		a1<-as.integer(a1/2)
  	}
  
	coa[i,]<-xdigito[1:n]
	njug[i]<-length(which(coa[i,]>0))
      primerjug[i]<-which(coa[i,]>0)[1]
}
 
index=1	
for (j in 1:n){
	jjugadores<-which(njug==j)
	for (k in 1:length(jjugadores)){
		coadef[index,]<-coa[jjugadores[k],]
		classici<-which(coadef[index,]>0)
		classic[index]<-paste(paste("'{",paste0(classici,collapse=",")),"}'")
		index=index+1
	}
}
coadef<-rbind(rep(0,n),coadef)
classic<-c("0",classic)
coalitions<-classic
rownames(coadef)<-c()
colnames(coadef)<-c()
sol<-list(coadef,classic)
names(sol)<-c("Binary","Classic")
return(sol)}
