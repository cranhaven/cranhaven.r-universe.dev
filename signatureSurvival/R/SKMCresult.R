SKMCresult <-
function(data,mol, time="month", status="status",quant=c("No",-0.2,0.2))	{


	####################################
	# data is survprotein object
	# protein is single or combined proteins (in one column)
	#file is pdf file for graphs
	#
	################################
  qt<-quant[1]
  lw<-as.numeric(quant[2])
  hg<-as.numeric(quant[3])
  
  
#	library(survival)
#	rm(list=ls(all=TRUE))
#    library(ISwR)

  colnms<-tolower(colnames(data))
  mth<-is.element(colnms,tolower(time))
  sts<-is.element(colnms,tolower(status))
data$PFS = as.vector(data[,mth])
data$PFS= as.numeric(data[,mth])

data = data [!is.na (data[,"PFS"] ), ]
data$PFS= data$PFS
temp.PFS= data$PFS
data$status
data = data [!is.na (data[,sts] ), ]
dim(data)
molename = names(data)

#pdf(file, width=12, height=12)
res1=c()
########################################################
pn=ncol(data)
#data$PFS [data$PFS > 6]=6
PFS<-data$PFS
status<-data[,sts]

if(is.character(mol)){
  kind<-is.element(colnms,tolower(mol)) 
  ml<-which(kind==TRUE)
}else if(is.numeric(mol)){
  ml<-mol
}

#	for (mol in 7:pn) {
p.val1 =c()
#	mol=7
 # plt<-par(mfrow=c(3,3))
#	for (jj in seq(10,90,10)){
	molecule <- molename[ml]
#      print(molecule)
	data1 <- cbind(data[,ml], PFS,status)
      colnames(data1)[1]<-molecule
      data1<-as.data.frame(data1)
      dat<-apply(data1,2,na.omit)
      dat1<-apply(dat,2,as.numeric)
      data1<-as.data.frame(dat1)

#     data1 <- data[!is.na(data[,molecule]),c(molecule,"PFS", "status")]
	data1$Conc <- "Null"
#	cutoff = median(data1[,molecule], jj/100, na.rm=T )

     zv<-(data1[,molecule]-mean(data1[,molecule]))/sqrt(var(data1[,molecule]))
	#zv<-data1[,molecule]
      cutoff = median(data1[,molecule])
      data1$zv<-zv
      if(qt=="yes"||qt=="y"||qt=="YES"||qt=="Y"){
        data1$Conc[data1$zv>=quantile(zv, hg)] <- "High"
        data1$Conc[data1$zv<=quantile(zv, lw)] <- "Low"
      }else{
        data1$Conc[data1$zv<= lw]<-"Low"
        data1$Conc[data1$zv>= hg]<-"High"
      }      
      
#       data1$Conc [ data1[,molecule]>  cutoff]="High"
 #print(data1)
      data1<-subset(data1,data1$Conc!="Null")

#######

	## Creating the survival object
	msurv <- Surv(data1$PFS, data1$status)
	data1$PFS
	data1$status
	msurv
	##END

	data1$grp <-0
	#data1$grp [ data1$Conc == "Low"] = 0
	data1$grp [ data1$Conc == "High"] <- 1


	hr = coxph(formula = Surv(PFS, status==1) ~ grp, data = data1)
	res = c ( molecule ,summary(hr)$conf.int[1:4], summary(hr)$sctest[3])
  #    print(summary(hr))
	##The Kaplan-Meier estimator
	mfit <- survfit(msurv ~1)
	options(survfit.print.mean = TRUE)
	mfit

	summary(mfit)
	#plot(mfit, conf.int = FALSE)
	###END

	######### Group Comparison
	mfit.byGroups <- survfit(Surv(PFS, status == 1) ~ Conc, data = data1)
	mfit.byGroups
#	print(summary(mfit.byGroups))
#	if ( summary(hr)$sctest[3]<1)
	#{
#	plot(mfit.byGroups, conf.int = FALSE, lty = 1:2, lwd=1.5, cex.axis=1.5 )
#	plt<-plot(mfit.byGroups, conf.int = FALSE, lty=1, col=c("red","blue"), lwd=3.5, cex.axis=1.8 )

	#	mtext (jj)
		p.val = as.numeric (summary(hr)$coeff[5])
    zscore= as.numeric (summary(hr)$coeff[4])
		hr1 = as.numeric (summary(hr)$coeff[1])
		hr2 = as.numeric (summary(hr)$coeff[2])
		se = as.numeric (summary(hr)$coeff[3])
#		text(max(PFS)-35,0.97, paste ("HR=", round(hr1,2)) , cex=1.5, adj=0)

    res1<-matrix(NA,1,5)
		res1[1,] = c(hr1,hr2,se,zscore,p.val)
    colnames(res1)<-c("hazard risk","hazard rate","standard error","z-value","p-value")
return(res1)

}
