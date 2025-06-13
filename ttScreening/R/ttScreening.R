ttScreening<- function(y=y,formula,imp.var,data,B.values=FALSE,iterations=100,sva.method=c("two-step","irw"),
					cv.cutoff=50,n.sv=NULL,train.alpha=0.05,test.alpha=0.1, 
					FDR.alpha=0.05,Bon.alpha=0.05,percent=(2/3),linear= c("robust","ls"),
					vfilter = NULL, B = 5, numSVmethod = "be",rowname=NULL,maxit=20){

	
	'%ni%'<-Negate('%in%')
	m <- model.frame(formula, data)
	X <- model.matrix(formula, m)
	ifelse(is.null(rowname),rownames(y) <- seq(1:nrow(y)),rownames(y) <- rownames(y))	
	if(length(rownames(X)) != ncol(y)){print(paste(abs(ncol(y)-length(rownames(X)))," subject(s) excluded from analysis due to missing data in the prediction formla",sep=''));
		x.remove<-as.character(1:ncol(y))[which(as.character(1:ncol(y)) %ni% rownames(X))]}else{x.remove<-"NONE"}
	x.omit<-which(rownames(data) %ni% attributes(X)$dimnames[[1]])
	if(length(x.omit)< 1){y=na.omit(y)}else{y=na.omit(y[,-x.omit])}
	if(is.null(imp.var)){imp.var<-length(attr(terms(formula,keep.order=TRUE),"term.labels"))}

	imp.var.location<-match(attr(terms(formula,keep.order=FALSE),"term.labels"), attr(terms(formula,keep.order=TRUE),"term.labels"))
	remove.imp.var<-which(imp.var.location %in% imp.var)
	final.imp.var<-which(attributes(X)$assign %in% remove.imp.var)

	
	if(B.values==TRUE){edata <- as.matrix(log2(y/(1-y)))}else{edata<-as.matrix(y)}

	train.length<-test.length<-TT.output<-FDR.output<-Bon.output<-final.temp<-freq.temp<-cpg.select<-NULL
	selection<-matrix(rep(0,nrow(edata)*iterations),nrow=nrow(edata),ncol=iterations)
	pvalue.matrix<-matrix(rep(NA,nrow(edata)*iterations),nrow=nrow(edata),ncol=iterations)
	
	mod<-X
	mod0<-X[,-final.imp.var]

	if(is.null(n.sv) || n.sv > 0){
	svobj = sva2(dat=as.matrix(edata),mod,mod0,n.sv=n.sv,method=sva.method);n.sv.temp<-svobj$n.sv}else{n.sv.temp=0}
	if(n.sv.temp >0){
		temp=data.frame(svobj$sv)
		location<-NULL
			if(n.sv.temp > 0){
			for (w in 1:n.sv.temp){
				if(length(unique(temp[,w])) == 2){
				location<-c(location,w)}else{location<-location}
			}
			}

		if(is.null(location)){
			svobj$sv<-temp
			print(paste(ncol(svobj$sv),"surrogate variables used in the analysis"))
		}else {
			svobj$sv<- as.numeric(temp[,-location])
			print(paste(length(location),"surrogate variables were not included in model fitting due to their lack of information"))
			print(paste(ncol(svobj$sv),"surrogate variables used in the analysis"))
			svobj$n.sv<-ncol(svobj$sv)
		}

		if(n.sv.temp == 0 | is.null(svobj$n.sv) == TRUE){
			modSv = mod
			sv.output="NULL"
			svobj$n.sv <- 0
		}else{
			modSv = data.frame(mod,as.matrix(svobj$sv))
			colnames(modSv)<-c(colnames(mod),paste("SV_",colnames(as.matrix(svobj$sv)),sep=''))
			sv.output=data.frame(svobj$sv)
		}
	}else{
		modSv = mod
		sv.output='No surrogate variables requested or 0 used in analysis'
		print("No surrogate variables used in the analysis")

	}


	#TT#
	if(linear=='robust'){print("warning: robust regression may slow down screening process")}
	x <- 1:ncol(edata)
	length <- round(percent * ncol(edata))
	TT.warnings=NULL
	for (i in 1:iterations){
		set.seed(i)		
		iter<-0
		repeat{
			iter<-iter+1
			lmfit=lmfit.test=train.temp=test.temp=errors=warnings=NULL
        		train <- sample(x, size=length, replace=FALSE)
	  		test <- x[-train]

				capture.output({lmfit<-tryCatch.W.E(eBayes(lmFit(edata[,train], design=modSv[train,], method=linear,maxit=maxit))$p.value)},file='NUL')
				if(is.matrix(lmfit$value)==FALSE){train.temp=NULL;errors=c(errors,lmfit$value)}else{warnings=c(warnings,lmfit$warning);train.temp<-which(apply(as.matrix(lmfit$value[,final.imp.var]),1,min)<= train.alpha)}			
				train.length<-c(train.length,length(train.temp))	#number of sites selected at training level
				
				if(length(train.temp) == 1 && train.temp != 0){capture.output({lmfit.test<-tryCatch.W.E(matrix(summary(glm(matrix(edata[train.temp,test],ncol=1)~modSv[test,-1],family=gaussian))$coefficients[,4],nrow=1,ncol(modSv)))},file='NUL')}
				if(length(train.temp)> 1){capture.output({lmfit.test<-tryCatch.W.E(eBayes(lmFit(edata[train.temp,test], design=modSv[test,], method=linear))$p.value)},file='NUL')}
				if(length(train.temp) == 0){lmfit.test=NULL;test.temp=NULL;train.temp=NULL} 
				
				if(any(class(lmfit.test$value)%in% c("simpleError","error","condition"))){errors=c(errors,lmfit.test$value)}else{warnings=c(warnings,lmfit.test$warning);
				if(is.null(train.temp)==FALSE && length(train.temp) > 1){test.temp<-train.temp[(apply(as.matrix(lmfit.test$value[,final.imp.var]),1,min)<= test.alpha)];test.pvalue<-apply(as.matrix(lmfit.test$value[,final.imp.var]),1,min)};
				if(is.null(train.temp)==FALSE && length(train.temp)==1){test.temp<-train.temp[min(lmfit.test$value[,final.imp.var])<= test.alpha];test.pvalue<-min(lmfit.test$value[,final.imp.var])}}

			if(is.null(errors)){break.indicator<-'nobreak';break}
			if(iter==10 & is.null(errors)==FALSE){break.indicator<-'break';break}
		}

			if(break.indicator == 'break'){print(c('TT unavailable due to errors:',unique(c(errors))));break}
			if(length(test.temp) < 1){selection[,i]<- 0;test.length<-c(test.length,0)}else{selection[test.temp,i]<- 1;pvalue.matrix[test.temp,i]<- test.pvalue[(test.pvalue <= test.alpha)];test.length<-c(test.length,length(test.temp))}


	}

		
		cutoff <- ((cv.cutoff/100)*iterations)
		cpg.select<-rowSums(selection)
		freq.temp<-rowSums(selection)[rowSums(selection) >= cutoff]
		if(break.indicator=='break'){final.temp=NULL}else{final.temp<-which(rowSums(selection) >= cutoff);if(is.null(warnings)==F)print(c('TT.warnings:',unique(warnings)))}


	#Full Data#	
		capture.output({lmfit<-tryCatch.W.E(eBayes(lmFit(edata, design=modSv, method=linear, maxit=maxit)))},file='NUL')
		if(any(class(lmfit) %ni% c("simpleError","error","condition"))){
		lmfit.min<-apply(as.matrix(lmfit$value$p.value[,final.imp.var]),1,min);if(is.null(lmfit$warnings)==FALSE){print(c('Complete Data Warnings:',lmfit$warning))}
		}else{
		print(c('Complete Data Errors:',lmfit$value))
		}

	#TT#
		if(length(final.temp)<1){TT.output<- t(rep("NA",(ncol(modSv)*2 + 2)))
			}else{
			TT.output<-data.frame(rownames(edata)[final.temp],freq.temp,matrix(lmfit$value$coefficients[final.temp,],nrow=length(final.temp),ncol=ncol(modSv)), matrix(lmfit$value$p.value[final.temp,],nrow=length(final.temp) , ncol=ncol(modSv)))
			}

			colnames(TT.output)<-c("Row_Ind_Select","Selection Prop",paste("Coeff",colnames(modSv)),paste("Pvalue",colnames(modSv)))

	#FDR/Bon#
		lmFitFDR.rob<-which(p.adjust(lmfit.min,method="fdr")<= FDR.alpha)
		lmFitBon.rob<-which(p.adjust(lmfit.min,method="bonferroni")<= Bon.alpha)

		if(length(lmFitFDR.rob)<1){FDR.output<- t(rep("NA",(ncol(modSv)*2 + 1)))
			}else{
			FDR.output<-data.frame(rownames(edata)[lmFitFDR.rob], matrix(lmfit$value$coefficients[lmFitFDR.rob,],nrow=length(lmFitFDR.rob),ncol=ncol(modSv)), matrix(lmfit$value$p.value[lmFitFDR.rob,],nrow=length(lmFitFDR.rob) , ncol=ncol(modSv)))
			}

		if(length(lmFitBon.rob)<1){Bon.output <- t(rep("NA",(ncol(modSv)*2 + 1)))
			}else{
			Bon.output<-data.frame(rownames(edata)[lmFitBon.rob], matrix(lmfit$value$coefficients[lmFitBon.rob,],nrow=length(lmFitBon.rob),ncol=ncol(modSv)), matrix(lmfit$value$p.value[lmFitBon.rob,],nrow=length(lmFitBon.rob) , ncol=ncol(modSv)))
			}

		colnames(FDR.output)<-colnames(Bon.output)<-c("Row_Ind_Select",paste("Coeff",colnames(modSv)),paste("Pvalue",colnames(modSv)))

output=list(TT.cpg=rownames(edata)[final.temp],train.cpg=train.length,test.cpg=test.length, SV.output=sv.output,
			selection=selection,pvalue.matrix=pvalue.matrix,TT.output=TT.output, FDR.output=FDR.output, Bon.output=Bon.output,
			FDR.cpg=rownames(edata)[lmFitFDR.rob],Bon.cpg=rownames(edata)[lmFitBon.rob],sub.remove=x.remove)

output
}
