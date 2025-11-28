UKMplot <-
function(data,mol,HR="hazard risk",time="month",status="status", sml="hv",
         quant=c("No",-0.2,0.2),plotmethod="plot",adjx)	{


	####################################
	# data is survprotein object
	# protein is single or combined proteins (in one column)
	#file is pdf file for graphs
	#
	################################

  HR<-tolower(HR)
  if(HR=="hazard risk"||HR=="hazard_risk"){
    HR<-"hrisk"
  }else{
    HR="hrate"
  }  
  
  qt<-quant[1]
  lw<-as.numeric(quant[2])
  hg<-as.numeric(quant[3])  

  colnms<-tolower(colnames(data))
  mth<-is.element(colnms,tolower(time))
  sts<-is.element(colnms, tolower(status))
  data$PFS = as.vector(data[,mth])
  data$PFS= as.numeric(data[,mth])

data = data [!is.na (data[,"PFS"] ), ]
data = data [!is.na (data[,sts] ), ]
data$PFS= data$PFS
temp.PFS= data$PFS
data$stats
data$stats = data [!is.na (data[,sts] ), ]
#print(dim(data))
molename = names(data)

#pdf(file, width=12, height=12)
res1=c()
########################################################
pn=ncol(data)
#data$PFS [data$PFS > 6]=6
month<-data$PFS
status<-data$stats
age<-data$age
sex<-data$sex


if(is.character(mol)){
  kind<-is.element(colnms,tolower(mol)) 
  ml<-which(kind==TRUE)
}else if(is.numeric(mol)){
  ml<-mol
}
#	for (mol in 7:pn) {
p.val1 =c()
#	mol=7
#plt<-par(mar=c(4,5,3,1),oma=c(1,2,1,1))
#	for (jj in seq(10,90,10)){
	molecule <- molename[ml]
#      print(molecule)
	data1 <- cbind(data[,ml], month,status,age,sex)
      colnames(data1)[1]<-molecule

      data1<-as.data.frame(data1)

#      data1<-as.character(data1)
 #     print(data1)

#      dat1<-apply(data1,2,as.numeric)
      data1<-as.data.frame(data1)

#     data1 <- data[!is.na(data[,molecule]),c(molecule,"PFS", "status")]
	data1$Conc <- "Null"
#	cutoff = median(data1[,molecule], jj/100, na.rm=T )

	zv<-(data1[,molecule]-mean(data1[,molecule]))/sqrt(var(data1[,molecule]))

      cutoff = median(data1[,molecule])
      data1$zv<-zv
#       data1$Conc[data1$zv<= -0.2]="Low"
#	     data1$Conc [ data1$zv >=  0.2]="High"
#       data1$Conc [ data1[,molecule]>  cutoff]="High"
# print(data1)
	     if(qt=="yes"||qt=="Y"||qt=="YES"){
	       data1$Conc[data1$zv>=quantile(zv, hg)] <- "High"
	       data1$Conc[data1$zv<=quantile(zv, lw)] <- "Low"
	     }else{
	       data1$Conc[data1$zv<= lw]<-"Low"
	       data1$Conc[data1$zv>= hg]<-"High"
	     }
      data1<-subset(data1,data1$Conc!="Null")

#######

	## Creating the survival object
	msurv <- Surv(data1$month, data1$status)
	data1$PFS
	data1$status
	msurv
	##END

	data1$grp <-0
	#data1$grp [ data1$Conc == "Low"] = 0
	data1$grp [ data1$Conc == "High"] <- 1

	hr = coxph(formula = Surv(month, status==1) ~  grp, data = data1)
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
	mfit.byGroups <- survfit(Surv(month, status == 1) ~ Conc, data = data1)
	mfit.byGroups
#	print(summary(mfit.byGroups))
	if (plotmethod=="plot"){
	#{
#	plot(mfit.byGroups, conf.int = FALSE, lty = 1:2, lwd=1.5, cex.axis=1.5 )
#	plt<-plot(mfit.byGroups, conf.int = FALSE, lty=1, col=c("red","blue"), lwd=3.5, cex.axis=1.8 )
	plt<-plot(mfit.byGroups, conf.int = FALSE, lty=1, col=c("red","blue"), lwd=3.5, cex.axis=1.5,xlab=
         time,ylab="survival probability", cex.lab=1.5)
		box(lwd=1.8)
		low.n = sum(data1$Conc == "Low")
		high.n = sum(data1$Conc == "High")
		legend(.1, .3, c(paste ("Low(n=",low.n ,")" ), paste ("High(n=",high.n ,")" )), col=c("blue","red"),
		bty = "n", cex= 2.0, lty = 1, lwd=3.5)
		title(paste("Gene",ml,":",molecule))
	#	mtext (jj)
		p.val = as.numeric (summary(hr)$coeff[5])
	#	if(p.val>1){p.val=1}
		if(HR=="hrisk"){
		  hr1 = as.numeric (summary(hr)$coeff[1,1])
		}else{
		  hr1 = as.numeric (summary(hr)$coeff[1,2])  
		}
#		text(max(PFS)-35,0.97, paste ("HR=", round(hr1,2)) , cex=1.5, adj=0)
		text(max(month)-adjx,0.97, paste ("HR = ", round(hr1,3)) , cex=1.8, adj=0)
		if(p.val<1e-05){
		  text(max(month)-adjx,0.89, paste ("p  < 0.00001") , cex=1.8, adj=0)
		}else if((p.val<1e-03)&(p.val>=1e-05)){
		text(max(month)-adjx,0.89, paste ("p  = ", round(p.val,5)) , cex=1.8, adj=0)
		}else if((p.val<1e-02)&(p.val>=1e-03)){
		text(max(month)-adjx,0.89, paste ("p  = ", round(p.val,4)) , cex=1.8, adj=0)   
		}else{
		text(max(month)-adjx,0.89, paste ("p  = ", round(p.val,3)) , cex=1.8, adj=0)  
     }
		coeffs <- coef(summary(hr))
#		print(coeffs)
		res1 = rbind(res1, res)
#	}
#}

	}else if(plotmethod=="ggsurvplot"){
	  
	plt <- ggsurvplot(mfit.byGroups, 
	                  # survfit object with calculated statistics.
		  data = data1,             # data used to fit survival curves.
		  # risk.table = TRUE,       # show risk table.
		  pval = TRUE,             # show p-value of log-rank test.
		  pval.method = T,
		  # test.for.trend = T,
		  conf.int = FALSE,         # show confidence intervals for
		  # point estimates of survival curves.
		  title = paste("Gene",ml,":",molecule),
		  palette = c("red", "navyblue"),
		  xlim = c(0,max(month)),         # present narrower X axis, but not affect
		  # survival estimates.
		  xlab = time,   # customize X axis label.
	#	  break.time.by = 20,     # break X axis in time intervals by 500.
		  ggtheme = theme_bw(), # Change ggplot2 theme
		  
		  risk.table = "abs_pct",
		  risk.table.y.text.col = T,# colour risk table text annotations.
		  risk.table.height = 0.25, # the height of the risk table
		  risk.table.y.text = FALSE,# show bars instead of names in text annotations
		  # in legend of risk table.
		  risk.table.fontsize = 2.5,
		  ncensor.plot = FALSE,      # plot the number of censored subjects at time t
		  #ncensor.plot.height = 0.25,
		  conf.int.style = "step",  # customize style of confidence intervals
		  surv.median.line = sml,  # add the median survival pointer.
		  legend.labs = c("High", "Low")    # change legend labels.	
		  
		)
	}	
		

#dev.off()
#write.csv(res1, file2, row.names=F)
return(plt)

}
