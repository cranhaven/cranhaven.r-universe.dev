# TODO: Estimating the Replication Rate
# 
# Author: wjiangaa
###############################################################################

#Woolf's method to estimate standard error of log(ORhat) 
SEest<-function(n0,n1,fU,fA){
	return(sqrt(1/(2*n0*fU*(1-fU))+(1/(2*n1*fA*(1-fA)))))
}

#Positive probability for log odds ratio test
posProb<-function(MU,SE,zalpha2){
	return(pnorm(-zalpha2-MU/SE)+pnorm(-zalpha2+MU/SE))
}

#Positive probability for log odds ratio test in the replication study
posProbR<-function(MU,SE2,zalphaR2,z){
	return(pnorm(-zalphaR2+sign(z)*MU/SE2))
}

# Infer the proportion of nonassociated SNPs (NULL hypotheses) using Storey's Method [2003]
# See Storey, J. D., & Tibshirani, R. (2003). 
# Statistical significance for genomewide studies. 
# Proceedings of the National Academy of Sciences, 100(16), 9440-9445.
# Input: P values-pvals[vector]
#        show plot estimation process-showEst[bool, T]
#        show result info-info[bool, T]
#        Output directory for inference plot when showEst=T-outputDir[str, "output"]
# Output: The proportion of nonassociated SNPs (spline)-pi0_1[scalar]
#         The proportion of nonassociated SNPs (lambda=0.5)-pi0_2[scalar]
snpNullS<-function(pval, showEst=TRUE, info=TRUE, outputDir="output"){
#	browser()
	m<-length(pval)
	pvalA<-sort(pval) #Sorting pvals with ascending order
#	lambda<-seq(0.05,0.95,0.05)
	lambda<-seq(0.05,0.8,0.05)
	idx<-1
	pi0HatF<-rep(0,length(lambda))
	for(i in 1:length(lambda)){
		for(idx in idx:m) if(pvalA[idx]>=lambda[i]) break
		pi0HatF[i]<-(m-idx+1)/(m*(1-lambda[i]))
	}
#	library('splines')
	fHat <- lm(pi0HatF ~ ns(lambda, df = 3))
	pi0Pred<- predict(fHat,data.frame(lambda=lambda))
#	pi0<-min(pi0Pred[length(lambda)],1)
	pi0<-min(predict(fHat,data.frame(lambda=1)),1)

#	fHat <- smooth.spline(lambda, pi0HatF, df = 3)
#	pi0Pred <- predict(fHat, x = lambda)$y
##	pi0<-min(pi0Pred[length(lambda)],1)
#	pi0<-min(predict(fHat, x = 1)$y,1)

	if(showEst==TRUE){
		dir.create(outputDir,showWarnings=F)
		pdf(paste(outputDir,"pi0infer.pdf",sep="/"))
		plot(lambda,pi0HatF, xlab = "lambda", ylab = "pi0_hat",main="Estimating pi0")
		lines(lambda, pi0Pred)
		dev.off()
	}
	
#	pi0_2<-min(pi0HatF[51],1)
	if(info) 
		cat("Estimated pi0 (spline):",pi0,"\n")
	return(list(pi0=pi0,pi0Full=pi0HatF,pi0Pred=pi0Pred))
}

#Estimate the variance of log(OR), i.e. sigma0^2
snpMUvar<-function(z,pi0, SE){
	m<-length(z)
	if(pi0==1) return(0)
	return(max(((sum(z^2)-m*pi0)/(1-pi0)-m)/sum(1/SE^2),0))
}

#local fdr based on two parameters model
lfdr2<-function(z,SE,sigma02, pi0){
	f0<-dnorm(z)
	f1<-dnorm(z,0,sqrt(1+sigma02/(SE^2)))
	return(pi0*f0/(pi0*f0+(1-pi0)*f1))
}

#replication rate /true non-replication rate/ average RR
repRate<-function(MUhat,SE, SE2,sigma02,pi0,zalphaR2, output=TRUE, dir='output'){ #num is the number of sampling points
	alphaR<-2*(1-pnorm(zalphaR2))
	lambda<-sigma02/(SE^2+sigma02)
	zstar<-lambda*MUhat/SE2
	sigmaStar2<-1+lambda*(SE^2)/(SE2^2)
	#replication predictive power
	repRateH1<-pnorm((-zalphaR2+sign(MUhat)*zstar)/sqrt(sigmaStar2))

	lfdr<-lfdr2(MUhat/SE,SE,sigma02,pi0)
	RR<-lfdr*alphaR/2+(1-lfdr)*repRateH1
	TNR<-ifelse(RR==1, 0, lfdr*(1-alphaR/2)/(1-RR))
#	prob1<-dnorm((zR-zstar)/sqrt(sigmaStar2))
#	fir<-(1-lfdr)*prob1/(lfdr*dnorm(zR)+(1-lfdr)*prob1)
	avgRR<-mean(RR)
	
	if(output==T){
		dir.create(dir,showWarnings=F)
		write.table(cbind(RR,TNR,lfdr,repRateH1),paste(dir,'rate.txt',sep='/'),
				col.names=c('RR','TNR','lfdr','Pred.Power'),row.names=F,sep='\t',quote=F)
		write.table(avgRR,paste(dir,'summary.txt',sep='/'),
				col.names='Avg.RR',row.names=F,sep='\t',quote=F)
	}
	
	return(list(RR=RR,TNR=TNR,lfdr=lfdr,predPower=repRateH1, GRR=avgRR))
}

#Bootstrap to estimate pi0 and sigma0^2
snpBoot<-function(z,SE,num=100,pval=FALSE){ #,pvalEst=1){ #num is the resampling numbers
	pi0Boot<-rep(0,num)
	sigma02Boot<-rep(0,num)
	if(is.logical(pval)==T && pval==F) 
		pval<-2*(1-pnorm(abs(z)))
	for(i in 1:num){
		repeat{
			idx<-sample.int(length(z),replace=T)
			tmpZ<-z[idx]
			tmpSE<-SE[idx]		
			tmpPval<-pval[idx]
	#		pi0Boot[i]<-switch(pvalEst,snpNullS(tmpPval,showEst=F,info=F)$pi0_1,snpNullS(tmpPval,showEst=F,info=F)$pi0_2)
	#		pi0Boot[i]<-propTrueNull(tmpPval)
			pi0Boot[i]<-snpNullS(tmpPval,showEst=F,info=F)$pi0
			if(pi0Boot[i]!=1) break
		}
		sigma02Boot[i]<-snpMUvar(tmpZ,pi0Boot[i], tmpSE)
#		cat(i,pi0Boot[i],sigma02Boot[i],'\n')
	}
#	pi0BootCI<-quantile(pi0Boot,c(0.025,1-0.025))
#	sigma02BootCI<-quantile(sigma02Boot,c(0.025,1-0.025))
#	idx<-(pi0BootCI[1]<=pi0Boot) & (pi0Boot<=pi0BootCI[2]) & (sigma02BootCI[1]<=sigma02Boot) & (sigma02Boot<sigma02BootCI[2])
	return(list(pi0=pi0Boot,sigma02=sigma02Boot))
}

quantile<-function(x,q,na.rm=TRUE){
	result<-rep(0,length(q))
	if(na.rm) x.sorted<-sort(x[!is.na(x)])
	n<-length(x)
	for(i in 1:length(q)){
		result[i]<-x.sorted[floor(n*q[i])]
	}
	return(result)
}

#Pipeline to estimate RR/TNR/avgRR and their CI
repRateEst<-function(MUhat,SE, SE2,zalpha2,zalphaR2,boot=100,output=TRUE,idx=TRUE, dir='output',info=TRUE){#,pvalEst=1){
	#Point Estimation
	z<-MUhat/SE
#	if(is.logical(pval)==T && pval==F) 
	pval<-2*(1-pnorm(abs(z)))

	pi0hat<-snpNullS(pval,outputDir=dir,showEst=output,info=info)$pi0
	if(info) cat('Estimated Pi0:',pi0hat,'\n')
	sigma02hat<-snpMUvar(z,pi0hat,SE)
	if(info) cat('Estimated sigma0^2:',sigma02hat,'\n')
	sigIdx<-(pval<=2*(1-pnorm(zalpha2))) & idx
#	browser()
	repResult<-repRate(MUhat[sigIdx],SE[sigIdx],SE2[sigIdx],sigma02hat,pi0hat,zalphaR2,output=F)
	
	bootResult<-snpBoot(z,SE,boot,pval)#,pvalEst=pvalEst)
	bootRRdist<-matrix(0,nrow=sum(sigIdx),ncol=boot)
	bootTNRdist<-matrix(0,nrow=sum(sigIdx),ncol=boot)
	bootlfdrDist<-matrix(0,nrow=sum(sigIdx),ncol=boot)
	bootPredPowerDist<-matrix(0,nrow=sum(sigIdx),ncol=boot)
	bootAvgRRdist<-rep(0,boot)
	for(i in 1:boot){
		tmpRepResult<-repRate(MUhat[sigIdx],SE[sigIdx],SE2[sigIdx],
				bootResult$sigma02[i],bootResult$pi0[i],zalphaR2,output=F)
		bootRRdist[,i]<-tmpRepResult$RR
		bootTNRdist[,i]<-tmpRepResult$TNR
		bootlfdrDist[,i]<-tmpRepResult$lfdr
		bootPredPowerDist[,i]<-tmpRepResult$predPower
		bootAvgRRdist[i]<-tmpRepResult$GRR
	}
	bootRRlow<-rep(0,sum(sigIdx))
	bootRRhigh<-rep(0,sum(sigIdx))
	bootTNRlow<-rep(0,sum(sigIdx))
	bootTNRhigh<-rep(0,sum(sigIdx))
	bootlfdrLow<-rep(0,sum(sigIdx))
	bootlfdrHigh<-rep(0,sum(sigIdx))
	bootPredPowerLow<-rep(0,sum(sigIdx))
	bootPredPowerHigh<-rep(0,sum(sigIdx))

	#percentile CI
	for(i in 1:sum(sigIdx)){
		bootRR<-quantile(bootRRdist[i,],c(0.025,1-0.025))
		bootRRlow[i]<-bootRR[1]
		bootRRhigh[i]<-bootRR[2]
		bootTNR<-quantile(bootTNRdist[i,],c(0.025,1-0.025),na.rm = T)
		bootTNRlow[i]<-bootTNR[1] 
		bootTNRhigh[i]<-bootTNR[2]
		bootlfdr<-quantile(bootlfdrDist[i,],c(0.025,1-0.025))
		bootlfdrLow[i]<-bootlfdr[1] 
		bootlfdrHigh[i]<-bootlfdr[2]
		bootPredPower<-quantile(bootPredPowerDist[i,],c(0.025,1-0.025))
		bootPredPowerLow[i]<-bootPredPower[1]
		bootPredPowerHigh[i]<-bootPredPower[2] 
	}
	avgBootRR<-quantile(bootAvgRRdist,c(0.025,1-0.025))
	avgBootRRlow<-avgBootRR[1]
	avgBootRRhigh<-avgBootRR[2]
	
	if(output==T){
		dir.create(dir,showWarnings=F)
		write.table(cbind((1:length(z))[sigIdx], z[sigIdx], pval[sigIdx], repResult$RR, bootRRlow , bootRRhigh),
				paste(dir,'rate.txt',sep='/'), col.names=c('Index','Z','P','RR','RR95%low','RR95%upper'),row.names=F,sep='\t',quote=F)
		write.table(cbind(repResult$GRR,avgBootRRlow,avgBootRRhigh),
				paste(dir,'summary.txt',sep='/'), col.names=c('Avg.RR','95%low','95%upper'),row.names=F,sep='\t',quote=F)
	}
	
	return(list(idx=(1:length(z))[sigIdx], pi0=pi0hat, sigma02=sigma02hat,
				RR=repResult$RR, RRlow=bootRRlow, RRhigh=bootRRhigh, #RRdist=bootRRdist,
				lfdr=repResult$lfdr, lfdrLow=bootlfdrLow, lfdrHigh=bootlfdrHigh, #lfdrDist=bootlfdrDist,
				predPower=repResult$predPower, predPowerLow=bootPredPowerLow, predPowerHigh=bootPredPowerHigh, #predPowerDist=bootPredPowerDist,
				GRR=repResult$GRR,GRRlow=avgBootRRlow,GRRhigh=avgBootRRhigh))#, avgRRdist=bootAvgRRdist))
}

#Bisection to find positive x such that fun(x)=y, fun is a monotonic increasing function
incSearch<-function(fun,y,init=1000,tol=0.01){
	x<-init
	lowSign<-FALSE
	highSign<-FALSE
	repeat{
		currY<-fun(x)
		if(currY<y){
			lowX<-x
			x<-2*x
			lowSign<-TRUE
		}else if(currY>y){
			highX<-x
			x<-x/2
			highSign<-TRUE
		}else return(x)
		if(lowSign && highSign) break
	}
	while(highX-lowX>tol){
		x<-(lowX+highX)/2
		currY<-fun(x)
		if(currY<y){
			lowX<-x
		}else if(currY>y){
			highX<-x
		}else return(x)
	}
	return(x)
}

#Sample size determination for the replication study (Control to case ratio is the same with the original study)
repSampleSizeRR<-function(GRR, n, MUhat,SE,zalpha2,zalphaR2,idx=TRUE){
	z<-MUhat/SE
	pval<-2*(1-pnorm(abs(z)))
	pi0hat<-snpNullS(pval,showEst=FALSE,info=FALSE)$pi0
	#pi0hat<-propTrueNull(pval)
	sigma02hat<-snpMUvar(z,pi0hat,SE)
	sigIdx<-(abs(z)>=zalpha2)& idx
	GRRest<-function(n2){
		SE2<-SE/sqrt(n2/n)
		return(repRate(MUhat[sigIdx],SE[sigIdx], SE2[sigIdx],sigma02hat,pi0hat,zalphaR2, output=F)$GRR)
	}
	n2<-incSearch(GRRest,GRR)
	return(round(n2))
}

#Sample size determination for the replication study (Different control to case ratio)
repSampleSizeRR2<-function(GRR,CCR2, MUhat,SE,fU,fA,zalpha2,zalphaR2, idx=TRUE){
	z<-MUhat/SE
	pval<-2*(1-pnorm(abs(z)))
#	pi0hat<-propTrueNull(pval)
	pi0hat<-snpNullS(pval,showEst=FALSE,info=FALSE)$pi0
	sigma02hat<-snpMUvar(z,pi0hat,SE)
	sigIdx<-(abs(z)>=zalpha2)& idx
	GRRest<-function(n2){
		caseNum<-round(n2/(CCR2+1))
		SE2<-SEest(n2-caseNum,caseNum,fU,fA)
		return(repRate(MUhat[sigIdx],SE[sigIdx],SE2[sigIdx],sigma02hat,pi0hat,zalphaR2, output=F)$GRR)
	}
	n2<-incSearch(GRRest,GRR)
	return(round(n2))
}

