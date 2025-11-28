MVKMresult <-
function(data,X,mol, status = "status", time ="month",quant=c("No",-0.2,0.2))	{

	#################################################################################
  # This function performs multivariate survival analysis
	# data is survprotein object
	# protein is single or combined proteins (in one column)
	# output file is results: z-value and p-values of gene, sex and age
	#
	##################################################################################

#	library(survival)
#	rm(list=ls(all=TRUE))
#    library(ISwR)
  qt<-quant[1]
  lw<-as.numeric(quant[2])
  hg<-as.numeric(quant[3])
  
  
  colnms<-tolower(colnames(data))
  mth<-is.element(colnms,tolower(time))
  sts<-is.element(colnms,tolower(status))
  data$PFS = as.vector(data[,mth])
  data$PFS= as.numeric(data[,mth])


  data = data [!is.na (data[,"PFS"] ), ]
  data$PFS= data$PFS
  temp.PFS= data$PFS
  data$status
  data = data [!is.na (data[,"status"] ), ]
  dim(data)
  molename = names(data)

  #pdf(file, width=12, height=12)
  res1=c()
  ########################################################
  pn=ncol(data)
  #data$PFS [data$PFS > 6]=6
  month<-data$PFS
  status<-data$status
 # age<-data$age
 # sex<-data$sex

 Y<-colnames(data)
 if(!is.null(X)){
   if(length(X)==1){
     j<-is.element(Y,X)
     X1<-data[,j]
   }else if(length(X)==2){
     j<-is.element(Y,X[1])
     X1<-data[,j]
     j<-is.element(Y,X[2])
     X2<-data[,j]
   }else if(length(X)==3){
     j<-is.element(Y,X[1])
     X1<-data[,j]
     j<-is.element(Y,X[2])
     X2<-data[,j]
     j<-is.element(Y,X[3])
     X3<-data[,j]
   }else if(length(X)==4){
     j<-is.element(Y,X[1])
     X1<-data[,j]
     j<-is.element(Y,X[2])
     X2<-data[,j]
     j<-is.element(Y,X[3])
     X3<-data[,j]
     j<-is.element(Y,X[4])
     X4<-data[,j]
   }else if(length(X)==5){
     j<-is.element(Y,X[1])
     X1<-data[,j]
     j<-is.element(Y,X[2])
     X2<-data[,j]
     j<-is.element(Y,X[3])
     X3<-data[,j]
     j<-is.element(Y,X[4])
     X4<-data[,j]
     j<-is.element(Y,X[5])
     X5<-data[,j]
   }else if(length(X)==6){
     j<-is.element(Y,X[1])
     X1<-data[,j]
     j<-is.element(Y,X[2])
     X2<-data[,j]
     j<-is.element(Y,X[3])
     X3<-data[,j]
     j<-is.element(Y,X[4])
     X4<-data[,j]
     j<-is.element(Y,X[5])
     X5<-data[,j]
     j<-is.element(Y,X[6])
     X6<-data[,j]
   }else if(length(X)==7){
     j<-is.element(Y,X[1])
     X1<-data[,j]
     j<-is.element(Y,X[2])
     X2<-data[,j]
     j<-is.element(Y,X[3])
     X3<-data[,j]
     j<-is.element(Y,X[4])
     X4<-data[,j]
     j<-is.element(Y,X[5])
     X5<-data[,j]
     j<-is.element(Y,X[6])
     X6<-data[,j]
     j<-is.element(Y,X[7])
     X7<-data[,j]
   }else if(length(X)==8){
     j<-is.element(Y,X[1])
     X1<-data[,j]
     j<-is.element(Y,X[2])
     X2<-data[,j]
     j<-is.element(Y,X[3])
     X3<-data[,j]
     j<-is.element(Y,X[4])
     X4<-data[,j]
     j<-is.element(Y,X[5])
     X5<-data[,j]
     j<-is.element(Y,X[6])
     X6<-data[,j]
     j<-is.element(Y,X[7])
     X7<-data[,j]
     j<-is.element(Y,X[8])
     X8<-data[,j]
   }else if(length(X)==9){
     j<-is.element(Y,X[1])
     X1<-data[,j]
     j<-is.element(Y,X[2])
     X2<-data[,j]
     j<-is.element(Y,X[3])
     X3<-data[,j]
     j<-is.element(Y,X[4])
     X4<-data[,j]
     j<-is.element(Y,X[5])
     X5<-data[,j]
     j<-is.element(Y,X[6])
     X6<-data[,j]
     j<-is.element(Y,X[7])
     X7<-data[,j]
     j<-is.element(Y,X[8])
     X8<-data[,j]
     j<-is.element(Y,X[9])
     X9<-data[,j]
   }else if(length(X)==10){
     j<-is.element(Y,X[1])
     X1<-data[,j]
     j<-is.element(Y,X[2])
     X2<-data[,j]
     j<-is.element(Y,X[3])
     X3<-data[,j]
     j<-is.element(Y,X[4])
     X4<-data[,j]
     j<-is.element(Y,X[5])
     X5<-data[,j]
     j<-is.element(Y,X[6])
     X6<-data[,j]
     j<-is.element(Y,X[7])
     X7<-data[,j]
     j<-is.element(Y,X[8])
     X8<-data[,j]
     j<-is.element(Y,X[9])
     X9<-data[,j]
     j<-is.element(Y,X[10])
     X10<-data[,j]
   }
 }else{
   stop("no covariates are given")
 }

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

  if(length(X)==1){
  data1 <- cbind(data[,ml], month,status,X1)
  }else if(length(X)==2){
  data1 <- cbind(data[,ml], month,status,X1,X2)
  }else if(length(X)==3){
  data1 <- cbind(data[,ml], month,status,X1,X2,X3)
  }else if(length(X)==4){
  data1 <- cbind(data[,ml], month,status,X1,X2,X3,X4)
  }else if(length(X)==5){
  data1 <- cbind(data[,ml], month,status,X1,X2,X3,X4,X5)
  }else if(length(X)==6){
  data1 <- cbind(data[,ml], month,status,X1,X2,X3,X4,X5,X6)
  }else if(length(X)==7){
  data1 <- cbind(data[,ml], month,status,X1,X2,X3,X4,X5,X6,X7)
  }else if(length(X)==8){
  data1 <- cbind(data[,ml], month,status,X1,X2,X3,X4,X5,X6,X7,X8)
  }else if(length(X)==9){
  data1 <- cbind(data[,ml], month,status,X1,X2,X3,X4,X5,X6,X7,X8,X9)
  }else if(length(X)==10){
  data1 <- cbind(data[,ml], month,status,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10)
  }
  
  colnames(data1)[1]<-molecule
  data1<-as.data.frame(data1)

  #dat<-apply(data1,2,na.omit)
  #dat1<-apply(dat,2,as.numeric)
  #data1<-as.data.frame(dat)
  #     data1 <- data[!is.na(data[,molecule]),c(molecule,"PFS", "status")]
  data1$Conc <- "Null"
  #	cutoff = median(data1[,molecule], jj/100, na.rm=T )

  zv<-(data1[,molecule]-mean(data1[,molecule]))/sqrt(var(data1[,molecule]))

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
  # print(data1)
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


  if(length(X)==1){
    hr = coxph(formula = Surv(month, status==1) ~  grp+X1, data = data1)
  }else if(length(X)==2){
    hr = coxph(formula = Surv(month, status==1) ~  grp+X1+X2, data = data1)
  }else if(length(X)==3){
    hr = coxph(formula = Surv(month, status==1) ~  grp+X1+X2+X3, data = data1)
  }else if(length(X)==4){
    hr = coxph(formula = Surv(month, status==1) ~  grp+X1+X2+X3+X4, data = data1)
  }else if(length(X)==5){
    hr = coxph(formula = Surv(month, status==1) ~  grp+X1+X2+X3+X4+X5, data = data1)
  }else if(length(X)==6){
    hr = coxph(formula = Surv(month, status==1) ~  grp+X1+X2+X3+X4+X5+X6, data = data1)
  }else if(length(X)==7){
    hr = coxph(formula = Surv(month, status==1) ~  grp+X1+X2+X3+X4+X5+X6+X7, data = data1)
  }else if(length(X)==8){
    hr = coxph(formula = Surv(month, status==1) ~  grp+X1+X2+X3+X4+X5+X6+X7+X8, data = data1)
  }else if(length(X)==9){
    hr = coxph(formula = Surv(month, status==1) ~  grp+X1+X2+X3+X4+X5+X6+X7+X8+X9, data = data1)
  }else if(length(X)==10){
    hr = coxph(formula = Surv(month, status==1) ~  grp+X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data = data1)
  }

  res = c(molecule,summary(hr)$conf.int[1:4], summary(hr)$sctest[3])
  #    print(summary(hr))
  ##The Kaplan-Meier estimator
  mfit <- survfit(msurv ~1)
  options(survfit.print.mean = TRUE)
  mfit

  summary(mfit)
  #plot(mfit, conf.int = FALSE)
  ###END

  ######### Group Comparison

	#	mtext (jj)
  	coeffs <- coef(summary(hr))
 # 	print(coeffs)
if(length(X)==1){
   gene.p<-as.matrix(coeffs[1,5])
	  X1.p<-as.matrix(coeffs[2,5])

	  gene.z<-as.matrix(coeffs[1,4])
	  X1.z<-as.matrix(coeffs[2,4])

	  gene.hr1<-as.matrix(coeffs[1,1])
	  gene.hr2<-as.matrix(coeffs[1,2])
	  X1.hr1<-as.matrix(coeffs[2,1])
	  X1.hr2<-as.matrix(coeffs[2,2])
	  gene.se<-as.matrix(coeffs[1,3])
	  X1.se<-as.matrix(coeffs[2,3])
	  res = c(gene.hr1,gene.hr2,gene.se, gene.z, gene.p, X1.hr1,X1.hr2,X1.se,X1.z,
	          X1.p)
	  
} else if (length(X) == 2) {
  gene.hr1 <- as.matrix(coeffs[1, 1])
  gene.hr2 <- as.matrix(coeffs[1, 2])
  gene.se <- as.matrix(coeffs[1, 3])
  gene.z <- as.matrix(coeffs[1, 4])
  gene.p <- as.matrix(coeffs[1, 5])
  
  X1.hr1 <- as.matrix(coeffs[2, 1])
  X1.hr2 <- as.matrix(coeffs[2, 2])
  X1.se <- as.matrix(coeffs[2, 3])
  X1.z <- as.matrix(coeffs[2, 4])
  X1.p <- as.matrix(coeffs[2, 5])
  
  X2.hr1 <- as.matrix(coeffs[3, 1])
  X2.hr2 <- as.matrix(coeffs[3, 2])
  X2.se <- as.matrix(coeffs[3, 3])
  X2.z <- as.matrix(coeffs[3, 4])
  X2.p <- as.matrix(coeffs[3, 5])
  
  res = c(
    gene.hr1,
    gene.hr2,
    gene.se,
    gene.z,
    gene.p,
    X1.hr1,
    X1.hr2,
    X1.se,
    X1.z,
    X1.p,
    X2.hr1,
    X2.hr2,
    X2.se,
    X2.z,
    X2.p
  )
} else if (length(X) == 3) {
  gene.hr1 <- as.matrix(coeffs[1, 1])
  gene.hr2 <- as.matrix(coeffs[1, 2])
  gene.se <- as.matrix(coeffs[1, 3])
  gene.z <- as.matrix(coeffs[1, 4])
  gene.p <- as.matrix(coeffs[1, 5])
  
  X1.hr1 <- as.matrix(coeffs[2, 1])
  X1.hr2 <- as.matrix(coeffs[2, 2])
  X1.se <- as.matrix(coeffs[2, 3])
  X1.z <- as.matrix(coeffs[2, 4])
  X1.p <- as.matrix(coeffs[2, 5])
  
  X2.hr1 <- as.matrix(coeffs[3, 1])
  X2.hr2 <- as.matrix(coeffs[3, 2])
  X2.se <- as.matrix(coeffs[3, 3])
  X2.z <- as.matrix(coeffs[3, 4])
  X2.p <- as.matrix(coeffs[3, 5])
  
  X3.hr1 <- as.matrix(coeffs[4, 1])
  X3.hr2 <- as.matrix(coeffs[4, 2])
  X3.se <- as.matrix(coeffs[4, 3])
  X3.z <- as.matrix(coeffs[4, 4])
  X3.p <- as.matrix(coeffs[4, 5])
  
  res = c(
    gene.hr1,
    gene.hr2,
    gene.se,
    gene.z,
    gene.p,
    X1.hr1,
    X1.hr2,
    X1.se,
    X1.z,
    X1.p,
    X2.hr1,
    X2.hr2,
    X2.se,
    X2.z,
    X2.p,
    X3.hr1,
    X3.hr2,
    X3.se,
    X3.z,
    X3.p
  )
  
  
} else if (length(X) == 4) {
  gene.hr1 <- as.matrix(coeffs[1, 1])
  gene.hr2 <- as.matrix(coeffs[1, 2])
  gene.se <- as.matrix(coeffs[1, 3])
  gene.z <- as.matrix(coeffs[1, 4])
  gene.p <- as.matrix(coeffs[1, 5])
  
  X1.hr1 <- as.matrix(coeffs[2, 1])
  X1.hr2 <- as.matrix(coeffs[2, 2])
  X1.se <- as.matrix(coeffs[, 2, 3])
  X1.z <- as.matrix(coeffs[2, 4])
  X1.p <- as.matrix(coeffs[2, 5])
  
  X2.hr1 <- as.matrix(coeffs[3, 1])
  X2.hr2 <- as.matrix(coeffs[3, 2])
  X2.se <- as.matrix(coeffs[3, 3])
  X2.z <- as.matrix(coeffs[3, 4])
  X2.p <- as.matrix(coeffs[3, 5])
  
  X3.hr1 <- as.matrix(coeffs[4, 1])
  X3.hr2 <- as.matrix(coeffs[4, 2])
  X3.se <- as.matrix(coeffs[4, 2])
  X3.z <- as.matrix(coeffs[4, 4])
  X3.p <- as.matrix(coeffs[4, 5])
  
  X4.hr1 <- as.matrix(coeffs[5, 1])
  X4.hr2 <- as.matrix(coeffs[5, 2])
  X4.se <- as.matrix(coeffs[5, 3])
  X4.z <- as.matrix(coeffs[5, 4])
  X4.p <- as.matrix(coeffs[5, 5])
  res = c(
    gene.hr1,
    gene.hr2,
    gene.se,
    gene.z,
    gene.p,
    X1.hr1,
    X1.hr2,
    X1.se,
    X1.z,
    X1.p,
    X2.hr1,
    X2.hr2,
    X2.se,
    X2.z,
    X2.p,
    X3.hr1,
    X3.hr2,
    X3.se,
    X3.z,
    X3.p,
    X4.hr1,
    X4.hr2,
    X4.se,
    X4.z,
    X4.p
  )
  
} else if (length(X) == 5) {
  gene.hr1 <- as.matrix(coeffs[1, 1])
  gene.hr2 <- as.matrix(coeffs[1, 2])
  gene.se <- as.matrix(coeffs[1, 3])
  gene.z <- as.matrix(coeffs[1, 4])
  gene.p <- as.matrix(coeffs[1, 5])
  
  X1.hr1 <- as.matrix(coeffs[2, 1])
  X1.hr2 <- as.matrix(coeffs[2, 2])
  X1.se <- as.matrix(coeffs[2, 3])
  X1.z <- as.matrix(coeffs[2, 4])
  X1.p <- as.matrix(coeffs[2, 5])
  
  X2.hr1 <- as.matrix(coeffs[3, 1])
  X2.hr2 <- as.matrix(coeffs[3, 2])
  X2.se <- as.matrix(coeffs[3, 3])
  X2.z <- as.matrix(coeffs[3, 4])
  X2.p <- as.matrix(coeffs[3, 5])
  
  X3.hr1 <- as.matrix(coeffs[4, 1])
  X3.hr2 <- as.matrix(coeffs[4, 2])
  X3.se <- as.matrix(coeffs[4, 3])
  X3.z <- as.matrix(coeffs[4, 4])
  X3.p <- as.matrix(coeffs[4, 5])
  
  X4.hr1 <- as.matrix(coeffs[5, 1])
  X4.hr2 <- as.matrix(coeffs[5, 2])
  X4.se <- as.matrix(coeffs[5, 3])
  X4.z <- as.matrix(coeffs[5, 4])
  X4.p <- as.matrix(coeffs[5, 5])
  
  X5.hr1 <- as.matrix(coeffs[6, 1])
  X5.hr2 <- as.matrix(coeffs[6, 2])
  X5.se <- as.matrix(coeffs[6, 3])
  X5.z <- as.matrix(coeffs[6, 4])
  X5.p <- as.matrix(coeffs[6, 5])
  res = c(
    gene.hr1,
    gene.hr2,
    gene.se,
    gene.z,
    gene.p,
    X1.hr1,
    X1.hr2,
    X1.se,
    X1.z,
    X1.p,
    X2.hr1,
    X2.hr2,
    X2.se,
    X2.z,
    X2.p,
    X3.hr1,
    X3.hr2,
    X3.se,
    X3.z,
    X3.p,
    X4.hr1,
    X4.hr2,
    X4.se,
    X4.z,
    X4.p,
    X5.hr1,
    X5.hr2,
    X5.se,
    X5.z,
    X5.p
  )
  
} else if (length(X) == 6) {
  gene.hr1 <- as.matrix(coeffs[1, 1])
  gene.hr2 <- as.matrix(coeffs[1, 2])
  gene.se <- as.matrix(coeffs[2, 3])
  gene.z <- as.matrix(coeffs[1, 4])
  gene.p <- as.matrix(coeffs[1, 5])
  
  X1.hr1 <- as.matrix(coeffs[2, 1])
  X1.hr2 <- as.matrix(coeffs[2, 2])
  X1.se <- as.matrix(coeffs[2, 3])
  X1.z <- as.matrix(coeffs[2, 4])
  X1.p <- as.matrix(coeffs[2, 5])
  
  X2.hr1 <- as.matrix(coeffs[3, 1])
  X2.hr2 <- as.matrix(coeffs[3, 2])
  X3.se <- as.matrix(coeffs[3, 3])
  X2.z <- as.matrix(coeffs[3, 4])
  X2.p <- as.matrix(coeffs[3, 5])
  
  X3.hr1 <- as.matrix(coeffs[4, 1])
  X3.hr2 <- as.matrix(coeffs[4, 2])
  X3.se <- as.matrix(coeffs[4, 3])
  X3.z <- as.matrix(coeffs[4, 4])
  X3.p <- as.matrix(coeffs[4, 5])
  
  X4.hr1 <- as.matrix(coeffs[5, 1])
  X4.hr2 <- as.matrix(coeffs[5, 2])
  X4.se <- as.matrix(coeffs[5, 3])
  X4.z <- as.matrix(coeffs[5, 4])
  X4.p <- as.matrix(coeffs[5, 5])
  
  X5.hr1 <- as.matrix(coeffs[6, 1])
  X5.hr2 <- as.matrix(coeffs[6, 2])
  X5.se <- as.matrix(coeffs[6, 3])
  X5.z <- as.matrix(coeffs[6, 4])
  X5.p <- as.matrix(coeffs[6, 5])
  
  X6.hr1 <- as.matrix(coeffs[7, 1])
  X6.hr2 <- as.matrix(coeffs[7, 2])
  X6.se <- as.matrix(coeffs[7, 3])
  X6.z <- as.matrix(coeffs[7, 4])
  X6.p <- as.matrix(coeffs[7, 5])
  res = c(
    gene.hr1,
    gene.hr2,
    gene.se,
    gene.z,
    gene.p,
    X1.hr1,
    X1.hr2,
    X1.se,
    X1.z,
    X1.p,
    X2.hr1,
    X2.hr2,
    X2.se,
    X2.z,
    X2.p,
    X3.hr1,
    X3.hr2,
    X3.se,
    X3.z,
    X3.p,
    X4.hr1,
    X4.hr2,
    X4.se,
    X4.z,
    X4.p,
    X5.hr1,
    X5.hr2,
    X5.se,
    X5.z,
    X5.p,
    X6.hr1,
    X6.hr2,
    X6.se,
    X6.z,
    X6.p
  )
  
} else if (length(X) == 7) {
  gene.hr1 <- as.matrix(coeffs[1, 1])
  gene.hr2 <- as.matrix(coeffs[1, 2])
  gene.se <- as.matrix(coeffs[1, 3])
  gene.z <-
    as.matrix(coeffs[1, 4])
  gene.p <- as.matrix(coeffs[1, 5])
  
  X1.hr1 <- as.matrix(coeffs[2, 1])
  X1.hr2 <- as.matrix(coeffs[2, 2])
  X1.se <- as.matrix(coeffs[2, 3])
  X1.z <- as.matrix(coeffs[2, 4])
  X1.p <- as.matrix(coeffs[2, 5])
  
  X2.hr1 <- as.matrix(coeffs[3, 1])
  X2.hr2 <- as.matrix(coeffs[3, 2])
  X2.se <- as.matrix(coeffs[3, 3])
  X2.z <- as.matrix(coeffs[3, 4])
  X2.p <- as.matrix(coeffs[3, 5])
  
  X3.hr1 <- as.matrix(coeffs[4, 1])
  X3.hr2 <- as.matrix(coeffs[4, 2])
  X3.se <- as.matrix(coeffs[4, 3])
  X3.z <- as.matrix(coeffs[4, 4])
  X3.p <- as.matrix(coeffs[4, 5])
  
  X4.hr1 <- as.matrix(coeffs[5, 1])
  X4.hr2 <- as.matrix(coeffs[5, 2])
  X4.se <- as.matrix(coeffs[5, 3])
  X4.z <- as.matrix(coeffs[5, 4])
  X4.p <- as.matrix(coeffs[5, 5])
  
  X5.hr1 <- as.matrix(coeffs[6, 1])
  X5.hr2 <- as.matrix(coeffs[6, 2])
  X5.se <- as.matrix(coeffs[6, 3])
  X5.z <- as.matrix(coeffs[6, 4])
  X5.p <- as.matrix(coeffs[6, 5])
  
  X6.hr1 <- as.matrix(coeffs[7, 1])
  X6.hr2 <- as.matrix(coeffs[7, 2])
  X6.se <- as.matrix(coeffs[7, 3])
  X6.z <- as.matrix(coeffs[7, 4])
  X6.p <- as.matrix(coeffs[7, 5])
  
  X7.hr1 <- as.matrix(coeffs[8, 1])
  X7.hr2 <- as.matrix(coeffs[8, 2])
  X7.se <- as.matrix(coeffs[8, 3])
  X7.z <- as.matrix(coeffs[8, 4])
  X7.p <- as.matrix(coeffs[8, 5])
  
  res = c(
    gene.hr1,
    gene.hr2,
    gene.se,
    gene.z,
    gene.p,
    X1.hr1,
    X1.hr2,
    X1.se,
    X1.z,
    X1.p,
    X2.hr1,
    X2.hr2,
    X2.se,
    X2.z,
    X2.p,
    X3.hr1,
    X3.hr2,
    X3.se,
    X3.z,
    X3.p,
    X4.hr1,
    X4.hr2,
    X4.se,
    X4.z,
    X4.p,
    X5.hr1,
    X5.hr2,
    X5.se,
    X5.z,
    X5.p,
    X6.hr1,
    X6.hr2,
    X6.se,
    X6.z,
    X6.p,
    X7.hr1,
    X7.hr2,
    X7.se,
    X7.z,
    X7.p
  )
  
} else if (length(X) == 8) {
  gene.hr1 <- as.matrix(coeffs[1, 1])
  gene.hr2 <- as.matrix(coeffs[1, 2])
  gene.se <- as.matrix(coeffs[1, 3])
  gene.z <-
    as.matrix(coeffs[1, 4])
  gene.p <- as.matrix(coeffs[1, 5])
  
  X1.hr1 <- as.matrix(coeffs[2, 1])
  X1.hr2 <- as.matrix(coeffs[2, 2])
  X1.se <- as.matrix(coeffs[2, 3])
  X1.z <- as.matrix(coeffs[2, 4])
  X1.P <- as.matrix(coeffs[2, 5])
  
  X2.hr1 <- as.matrix(coeffs[3, 1])
  X2.hr2 <- as.matrix(coeffs[3, 2])
  X2.se <- as.matrix(coeffs[3, 3])
  X2.z <- as.matrix(coeffs[3, 4])
  X2.P <- as.matrix(coeffs[3, 5])
  
  X3.hr1 <- as.matrix(coeffs[4, 1])
  X3.hr2 <- as.matrix(coeffs[4, 2])
  X3.se <- as.matrix(coeffs[4, 3])
  X3.z <- as.matrix(coeffs[4, 4])
  X3.P <- as.matrix(coeffs[4, 5])
  
  X4.hr1 <- as.matrix(coeffs[5, 1])
  X4.hr2 <- as.matrix(coeffs[5, 2])
  X5.se <- as.matrix(coeffs[5, 3])
  X4.z <- as.matrix(coeffs[5, 4])
  X4.p <- as.matrix(coeffs[5, 5])
  
  X5.hr1 <- as.matrix(coeffs[6, 1])
  X5.hr2 <- as.matrix(coeffs[6, 2])
  X5.se <- as.matrix(coeffs[6, 3])
  X5.z <- as.matrix(coeffs[6, 4])
  X5.p <- as.matrix(coeffs[6, 5])
  
  X6.hr1 <- as.matrix(coeffs[7, 1])
  X6.hr2 <- as.matrix(coeffs[7, 2])
  X6.se <- as.matrix(coeffs[7, 3])
  X6.z <- as.matrix(coeffs[7, 4])
  X6.p <- as.matrix(coeffs[7, 5])
  
  X7.hr1 <- as.matrix(coeffs[8, 1])
  X7.hr2 <- as.matrix(coeffs[8, 2])
  X7.se <- as.matrix(coeffs[8, 3])
  X7.z <- as.matrix(coeffs[8, 4])
  X7.p <- as.matrix(coeffs[8, 5])
  
  X8.hr1 <- as.matrix(coeffs[9, 1])
  X8.hr2 <- as.matrix(coeffs[9, 2])
  X8.se <- as.matrix(coeffs[9, 3])
  X8.z <- as.matrix(coeffs[9, 4])
  X8.p <- as.matrix(coeffs[9, 5])
  
  res = c(
    gene.hr1,
    gene.hr2,
    gene.se,
    gene.z,
    gene.p,
    X1.hr1,
    X1.hr2,
    X1.se,
    X1.z,
    X1.p,
    X2.hr1,
    X2.hr2,
    X2.se,
    X2.z,
    X2.p,
    X3.hr1,
    X3.hr2,
    X3.se,
    X3.z,
    X3.p,
    X4.hr1,
    X4.hr2,
    X4.se,
    X4.z,
    X4.p,
    X5.hr1,
    X5.hr2,
    X5.se,
    X5.z,
    X5.p,
    X6.hr1,
    X6.hr2,
    X6.se,
    X6.z,
    X6.p,
    X7.hr1,
    X7.hr2,
    X7.se,
    X7.z,
    X7.p,
    X8.hr1,
    X8.hr2,
    X8.se,
    X8.z,
    X8.p
  )
  
  
} else if (length(X) == 9) {
  gene.hr1 <- as.matrix(coeffs[1, 1])
  gene.hr2 <- as.matrix(coeffs[1, 2])
  gene.se <- as.matrix(coeffs[1, 3])
  gene.z <- as.matrix(coeffs[1, 4])
  gene.p <- as.matrix(coeffs[1, 5])
  
  X1.hr1 <- as.matrix(coeffs[2, 1])
  X1.hr2 <- as.matrix(coeffs[2, 2])
  X1.se <- as.matrix(coeffs[2, 3])
  X1.z <- as.matrix(coeffs[2, 4])
  X1.P <- as.matrix(coeffs[2, 5])
  
  X2.hr1 <- as.matrix(coeffs[3, 1])
  X2.hr2 <- as.matrix(coeffs[3, 2])
  X2.se <- as.matrix(coeffs[3, 3])
  X2.z <- as.matrix(coeffs[3, 4])
  X2.P <- as.matrix(coeffs[3, 5])
  
  X3.hr1 <- as.matrix(coeffs[4, 1])
  X3.hr2 <- as.matrix(coeffs[4, 2])
  X3.se <- as.matrix(coeffs[4, 3])
  X3.z <- as.matrix(coeffs[4, 4])
  X3.P <- as.matrix(coeffs[4, 5])
  
  X4.hr1 <- as.matrix(coeffs[5, 1])
  X4.hr2 <- as.matrix(coeffs[5, 2])
  X5.se <- as.matrix(coeffs[5, 3])
  X4.z <- as.matrix(coeffs[5, 4])
  X4.p <- as.matrix(coeffs[5, 5])
  
  X5.hr1 <- as.matrix(coeffs[6, 1])
  X5.hr2 <- as.matrix(coeffs[6, 2])
  X5.se <- as.matrix(coeffs[6, 3])
  X5.z <- as.matrix(coeffs[6, 4])
  X5.p <- as.matrix(coeffs[6, 5])
  
  X6.hr1 <- as.matrix(coeffs[7, 1])
  X6.hr2 <- as.matrix(coeffs[7, 2])
  X6.se <- as.matrix(coeffs[7, 3])
  X6.z <- as.matrix(coeffs[7, 4])
  X6.p <- as.matrix(coeffs[7, 5])
  
  X7.hr1 <- as.matrix(coeffs[8, 1])
  X7.hr2 <- as.matrix(coeffs[8, 2])
  X7.se <- as.matrix(coeffs[8, 3])
  X7.z <- as.matrix(coeffs[8, 4])
  X7.p <- as.matrix(coeffs[8, 5])
  
  X8.hr1 <- as.matrix(coeffs[9, 1])
  X8.hr2 <- as.matrix(coeffs[9, 2])
  X8.se <- as.matrix(coeffs[9, 3])
  X8.z <- as.matrix(coeffs[9, 4])
  X8.p <- as.matrix(coeffs[9, 5])
  
  X9.hr1 <- as.matrix(coeffs[10, 1])
  X9.hr2 <- as.matrix(coeffs[10, 2])
  X9.se <- as.matrix(coeffs[10, 3])
  X9.z <- as.matrix(coeffs[10, 4])
  X9.p <- as.matrix(coeffs[10, 5])
  
  res = c(
    gene.hr1,
    gene.hr2,
    gene.se,
    gene.z,
    gene.p,
    X1.hr1,
    X1.hr2,
    X1.se,
    X1.z,
    X1.p,
    X2.hr1,
    X2.hr2,
    X2.se,
    X2.z,
    X2.p,
    X3.hr1,
    X3.hr2,
    X3.se,
    X3.z,
    X3.p,
    X4.hr1,
    X4.hr2,
    X4.se,
    X4.z,
    X4.p,
    X5.hr1,
    X5.hr2,
    X5.se,
    X5.z,
    X5.p,
    X6.hr1,
    X6.hr2,
    X6.se,
    X6.z,
    X6.p,
    X7.hr1,
    X7.hr2,
    X7.se,
    X7.z,
    X7.p,
    X8.hr1,
    X8.hr2,
    X8.se,
    X8.z,
    X8.p,
    X9.hr1,
    X9.hr2,
    X9.se,
    X9.z,
    X9.p
  )
  
} else if (length(X) == 10) {
  gene.hr1 <- as.matrix(coeffs[1, 1])
  gene.hr2 <- as.matrix(coeffs[1, 2])
  gene.se <- as.matrix(coeffs[1, 3])
  gene.z <- as.matrix(coeffs[1, 4])
  gene.p <- as.matrix(coeffs[1, 5])
  
  X1.hr1 <- as.matrix(coeffs[2, 1])
  X1.hr2 <- as.matrix(coeffs[2, 2])
  X1.se <- as.matrix(coeffs[2, 3])
  X1.z <- as.matrix(coeffs[2, 4])
  X1.P <- as.matrix(coeffs[2, 5])
  
  X2.hr1 <- as.matrix(coeffs[3, 1])
  X2.hr2 <- as.matrix(coeffs[3, 2])
  X2.se <- as.matrix(coeffs[3, 3])
  X2.z <- as.matrix(coeffs[3, 4])
  X2.P <- as.matrix(coeffs[3, 5])
  
  X3.hr1 <- as.matrix(coeffs[4, 1])
  X3.hr2 <- as.matrix(coeffs[4, 2])
  X3.se <- as.matrix(coeffs[4, 3])
  X3.z <- as.matrix(coeffs[4, 4])
  X3.P <- as.matrix(coeffs[4, 5])
  
  X4.hr1 <- as.matrix(coeffs[5, 1])
  X4.hr2 <- as.matrix(coeffs[5, 2])
  X5.se <- as.matrix(coeffs[5, 3])
  X4.z <- as.matrix(coeffs[5, 4])
  X4.p <- as.matrix(coeffs[5, 5])
  
  X5.hr1 <- as.matrix(coeffs[6, 1])
  X5.hr2 <- as.matrix(coeffs[6, 2])
  X5.se <- as.matrix(coeffs[6, 3])
  X5.z <- as.matrix(coeffs[6, 4])
  X5.p <- as.matrix(coeffs[6, 5])
  
  X6.hr1 <- as.matrix(coeffs[7, 1])
  X6.hr2 <- as.matrix(coeffs[7, 2])
  X6.se <- as.matrix(coeffs[7, 3])
  X6.z <- as.matrix(coeffs[7, 4])
  X6.p <- as.matrix(coeffs[7, 5])
  
  X7.hr1 <- as.matrix(coeffs[8, 1])
  X7.hr2 <- as.matrix(coeffs[8, 2])
  X7.se <- as.matrix(coeffs[8, 3])
  X7.z <- as.matrix(coeffs[8, 4])
  X7.p <- as.matrix(coeffs[8, 5])
  
  X8.hr1 <- as.matrix(coeffs[9, 1])
  X8.hr2 <- as.matrix(coeffs[9, 2])
  X8.se <- as.matrix(coeffs[9, 3])
  X8.z <- as.matrix(coeffs[9, 4])
  X8.p <- as.matrix(coeffs[9, 5])
  
  X9.hr1 <- as.matrix(coeffs[10, 1])
  X9.hr2 <- as.matrix(coeffs[10, 2])
  X9.se <- as.matrix(coeffs[10, 3])
  X9.z <- as.matrix(coeffs[10, 4])
  X9.p <- as.matrix(coeffs[10, 5])
  
  X10.hr1 <- as.matrix(coeffs[11, 1])
  X10.hr2 <- as.matrix(coeffs[11, 2])
  X10.se <- as.matrix(coeffs[11, 3])
  X10.z <- as.matrix(coeffs[11, 4])
  X10.p <- as.matrix(coeffs[11, 5])
  res = c(
    gene.hr1,
    gene.hr2,
    gene.se,
    gene.z,
    gene.p,
    X1.hr1,
    X1.hr2,
    X1.se,
    X1.z,
    X1.p,
    X2.hr1,
    X2.hr2,
    X2.se,
    X2.z,
    X2.p,
    X3.hr1,
    X3.hr2,
    X3.se,
    X3.z,
    X3.p,
    X4.hr1,
    X4.hr2,
    X4.se,
    X4.z,
    X4.p,
    X5.hr1,
    X5.hr2,
    X5.se,
    X5.z,
    X5.p,
    X6.hr1,
    X6.hr2,
    X6.se,
    X6.z,
    X6.p,
    X7.hr1,
    X7.hr2,
    X7.se,
    X7.z,
    X7.p,
    X8.hr1,
    X8.hr2,
    X8.se,
    X8.z,
    X8.p,
    X9.hr1,
    X9.hr2,
    X9.se,
    X9.z,
    X9.p,
    X10.hr1,
    X10.hr2,
    X10.se,
    X10.z,
    X10.p
  )
  
}
	
return(res)

}
