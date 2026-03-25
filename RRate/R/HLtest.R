#####Hosmer-Lemeshow test#########

HLtest<-function(x,p,g=10,null='all',boot=1000,info=T,dir='.'){
	calHLtest<-function(x,p,g=10){
		n<-length(x)
		sortedP<-sort(p)
		breaks<-sortedP[ceiling(seq(1,n,length.out=g+1))]
	#	label<-cut(p,breaks=breaks,labels=F,include.lowest=T)
		orderP<-order(p)
		label<-rep(0,n)
		label[orderP]<-ceiling((1:n)/ceiling(n/g))
		gTotal<-rep(0,g)
		gObs<-rep(0,g)
		gExp<-rep(0,g)
		for(i in 1:n){
			gTotal[label[i]]<-gTotal[label[i]]+1
			gExp[label[i]]<-gExp[label[i]]+p[i]
			if(x[i]==1) gObs[label[i]]<-gObs[label[i]]+1
		}
		nanVal<-function(x) ifelse(is.nan(x),0,x)
		H<-sum(nanVal((gObs-gExp)^2/(gExp*(1-gExp/gTotal))))
		return(H)
	}
	H<-calHLtest(x,p,g)
	if(info==T) cat('Hosmer-Lemeshow Test\nTest statistic H =',H,'\n')
	pval_chi2<-NaN
	pval_boot<-NaN
	df<-g-1
	if((null=='chi2')||(null=='all')){
		pval_chi2<-1-pchisq(H,df=df)
		cat('p-value (chi-squared):',pval_chi2,'\n')
	}
	if((null=='boot')||(null=='all')){
		Hdist<-rep(0,boot)
		for(i in 1:boot){
			randX<-rbinom(length(x),size=1,prob=p)
			Hdist[i]<-calHLtest(randX,p,g)
		}
		if(info==T){
			pdf(paste(dir,'/HL_H0.pdf',sep=''))
			hobj<-hist(Hdist,main='HL-test statistics under H0',xlab='H',ylab='Density',freq=F)
			plotSeq<-seq(0,hobj$breaks[length(hobj$breaks)],1)
			lines(plotSeq, dchisq(plotSeq,df=df),col='red',type='l')
			dev.off()
		}
		pval_boot<-sum(Hdist>=H)/boot
		if(info==T){
			if(pval_boot!=0)
				cat('p-value (bootstrap):',pval_boot,'\n')
			else
				cat('p-value (bootstrap) <=',1/boot,'\n')
		}
	}
	return(list(H=H,pval_chi2=pval_chi2,pval_boot=pval_boot))
}
