# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


.add.frame <- function(input="plotdiagTab",type,frame.name,argument.names="",arguments="",initial.values=c(),title="",border=FALSE,entry.width="2",argument.values=c(),argument.types=c(),from=c(),to=c(),by=c(),length=c(),button.name="",button.function="",button.data="",arg.frames=c(),button.otherarg="",button.biclust="",save=TRUE,show=TRUE,button.width="12",button.data.transf="matrix" ,new.frames=new.frames){
	
	
	# Entry Fields
	if(type=="entryfields"){
		
		new <-  list(type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types)
		
		if(input=="clusterTab"){
			new.frames$clusterTab[[length(new.frames$clusterTab)+1]] <- new
			return(new.frames)
		}
		if(input=="plotdiagTab"){
			new.frames$plotdiagTab[[length(new.frames$plotdiagTab)+1]] <- new
			new.frames <- .order.button.frames(new.frames)
			return(new.frames)
		}
	   
	   
	}
	
	# Radio buttons
	if(type=="radiobuttons"){
		
		new <-  list(type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,argument.values=argument.values,argument.types=argument.types)
		
		if(input=="clusterTab"){
			new.frames$clusterTab[[length(new.frames$clusterTab)+1]] <- new
			return(new.frames)
		}
		if(input=="plotdiagTab"){
			new.frames$plotdiagTab[[length(new.frames$plotdiagTab)+1]] <- new
			new.frames <- .order.button.frames(new.frames)  # Make sure the button frames are the last ones in the list .
			return(new.frames)
		}
	}
	
	# Check Boxes
	if(type=="checkboxes"){
		new <- list(type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border)
		
		if(input=="clusterTab"){
			new.frames$clusterTab[[length(new.frames$clusterTab)+1]] <- new
			return(new.frames)
		}
		if(input=="plotdiagTab"){
			new.frames$plotdiagTab[[length(new.frames$plotdiagTab)+1]] <- new
			new.frames <- .order.button.frames(new.frames)
			return(new.frames)
		}
		
	}
	
	# Slider Values
	if(type=="valuesliders"){
		new <- list(type=type,title=title,border=border,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,from=from,to=to,by=by,length=length)
		
		
		if(input=="clusterTab"){
			new.frames$clusterTab[[length(new.frames$clusterTab)+1]] <- new
			return(new.frames)
		}
		if(input=="plotdiagTab"){
			new.frames$plotdiagTab[[length(new.frames$plotdiagTab)+1]] <- new
			new.frames <- .order.button.frames(new.frames)
			return(new.frames)
		}
	}
	
	# Spin Boxes
	if(type=="spinboxes"){
		new <- list(type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,from=from,to=to,by=by,entry.width=entry.width,title=title,border=border)
		
		if(input=="clusterTab"){
			new.frames$clusterTab[[length(new.frames$clusterTab)+1]] <- new
			return(new.frames)
		}
		if(input=="plotdiagTab"){
			new.frames$plotdiagTab[[length(new.frames$plotdiagTab)+1]] <- new
			new.frames <- .order.button.frames(new.frames)
			return(new.frames)
		}
	}
	
	
	### SPECIFIC INPUT FOR PLOTDIAG FRAME ###
	if(input=="plotdiagTab"){
		# Manual Buttons
		
		if(type=="buttons"){
			new <- list(frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.otherarg=button.otherarg,arg.frames=arg.frames,button.biclust=button.biclust,title="",border=FALSE,save=save,show=show,button.width=button.width,button.data.transf=button.data.transf)
			new.frames$plotdiagTab[[length(new.frames$plotdiagTab)+1]] <- new
			
			new.frames <- .order.button.frames(new.frames)
			return(new.frames)
		}
		
		
		
		
	}
	
	
	### SPECIFIC INPUT FOR ENSEMBLE METHODS ###
	
	
	# TO BE ADDED IN FUTURE
	
	
	
	
}



.order.button.frames <- function(new.frames){
	
	boolean <- sapply(new.frames$plotdiagTab,FUN=function(x){x$type=="buttons"})
	new.frames$plotdiagTab <- new.frames$plotdiagTab[order(boolean)]
	
	return(new.frames)
}


#.as.var <- function(x){ return(eval.parent( as.name(x)  ,n=1))}



.find.frame <- function(x,frame.name){
	temp.names <- (lapply(x,FUN=function(d){return(d$frame.name)}))
	
	find.boolean <- temp.names==frame.name
	
	if(sum(find.boolean)==1){
		return(which(find.boolean))
	}
	
	else if(sum(find.boolean)>1){ 
		stop(paste("'",frame.name,"' is used for multiple frames!",sep=""),call.=FALSE)
	}
	else{
		stop(paste("'",frame.name,"' is not recognised as a framename. Check your 'grid.config' matrix.",sep=""),call.=FALSE)

	}
	
	
}


.eval.command <- function(x){return(eval.parent(parse(text=x),n=1))}


.combine.rows <- function(input,rows,title,border,grid.rows,grid.config){
	
	all.grid.rows <- grid.rows
	all.grid.config <- grid.config
	
	
	eval(parse(text=paste("grid.rows <- all.grid.rows$",input,sep="")))
	eval(parse(text=paste("grid.config <- all.grid.config$",input,sep="")))
	
	# The names of the frames involved in this combined row are extracted. This information is needed in the template function.
	name.frames <- as.vector(grid.config[rows,])
	name.frames <- name.frames[!is.na(name.frames)]
	
	new <- list(rows=rows,title=title,border=border,name.frames=name.frames)
	grid.rows[[length(grid.rows)+1]] <- new
	
	# In order to keep the grid.rows object correct. Sort the list, based on the rows inside an element:
	# This will ensure making a correct grid, even if the rows were combined like first 3 & 4, then 1 & 2
	
	grid.rows <- grid.rows[order( unlist(lapply(grid.rows,FUN=function(x){return(min(x$rows))})) )]
	
	
	eval(parse(text=paste("all.grid.rows$",input," <- grid.rows",sep="")))
	
	return(all.grid.rows)
}


.make.correct.frame <- function(title,border,window){
	
	if(title!="" & border==TRUE){
		
		return(ttklabelframe(window,text=gettextRcmdr(title)))
		
	}
	else{
		
		if(border==TRUE){relief<-"groove"} else {relief <- "flat"}
		
		return(tkframe(window,relief=relief,borderwidth=2))
		
	}
	
}


.update.biclustering.object <- function(object,where="all",ENVIR=environment(),ANALYSIS=""){
	
	biclustering.objects <- .GetEnvBiclustGUI("biclustering.objects")
	#if(!("biclustering.objects" %in% ls(envir=.GlobalEnv))){
	if(is.null(biclustering.objects)){
		biclustering.objects <- list()
		
		biclustering.objects$all <- c()
		biclustering.objects$bcdiag <- c()
		biclustering.objects$superbiclust <- c()
		biclustering.objects$dataconnect <- data.frame(result=character(),data=character(),stringsAsFactors=FALSE)
		biclustering.objects$ENVIR <- list()
		biclustering.objects$ANALYSIS <- list()
		
		#assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
		.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
	}
	
	if(where=="dataconnect"){
		biclustering.objects$dataconnect <- rbind(biclustering.objects$dataconnect,data.frame(result=object,data=ActiveDataSet()))
		# Check for double entries of a result
		temp.check <- biclustering.objects$dataconnect$result==object
		if(sum(temp.check)>1){
			biclustering.objects$dataconnect <- biclustering.objects$dataconnect[-which(temp.check==TRUE)[1],]
		}
		.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
		
	}
	
	if(where=="all"){
		#cat("UPDATEALL")
		biclustering.objects$all <- unique(c(biclustering.objects$all,object))
		biclustering.objects$dataconnect <- rbind(biclustering.objects$dataconnect,data.frame(result=object,data=ActiveDataSet()))
#		if(dim(biclustering.objects$dataconnect)[1]==1){
#			biclustering.objects$dataconnect$result <- as.character(biclustering.objects$dataconnect$result)
#			biclustering.objects$dataconnect$data <- as.character(biclustering.objects$dataconnect$data)
#		}
		
		# Check for double entries of a result
		temp.check <- biclustering.objects$dataconnect$result==object
		if(sum(temp.check)>1){
			biclustering.objects$dataconnect <- biclustering.objects$dataconnect[-which(temp.check==TRUE)[1],]
		}
		
		
#		assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
		.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
		
	}
	
	if(where=="bcdiag"){
		biclustering.objects$bcdiag <- unique(c(biclustering.objects$bcdiag,object))
#		assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
		.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
		
	}
	
	if(where=="superbiclust"){
		biclustering.objects$superbiclust <- unique(c(biclustering.objects$superbiclust,object))
#		assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
		.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
	
	}
	if(where=="envir"){
		if(object %in% names(biclustering.objects$ENVIR)){
			index.env <- which(object==names(biclustering.objects$ENVIR))
			temp.env <- biclustering.objects$ENVIR[[index.env]]
			rm(list=ls(temp.env),envir=temp.env)
		}
		else{
			index.env <- length(biclustering.objects$ENVIR)+1
		}
		biclustering.objects$ENVIR[[index.env]] <- ENVIR
		names(biclustering.objects$ENVIR)[index.env] <- object
#		assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
		.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
	
		
		
	}
	
	if(where=="analysis"){
		if(object %in% names(biclustering.objects$ANALYSIS)){
			index.analysis <- which(object==names(biclustering.objects$ANALYSIS))
		}
		else{
			index.analysis <- length(biclustering.objects$ANALYSIS)+1
		}
		
		biclustering.objects$ANALYSIS[[index.analysis]] <- ANALYSIS
		names(biclustering.objects$ANALYSIS)[index.analysis] <- object
		.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
	}
	
}

.initialize.new.frames <- function(){
	new.frames <- list()
	new.frames$clusterTab <- list()
	new.frames$plotdiagTab <- list()
	return(new.frames)
}

.initialize.grid.config <- function(){
	grid.config <- list()
	grid.config$clusterTab <- list()
	grid.config$plotdiagTab <- list()
	return(grid.config)
	
}

.initialize.grid.rows <- function(){
	grid.rows <- list()
	grid.rows$clusterTab <- list()
	grid.rows$plotdiagTab <- list()
	return(grid.rows)
	
}

.grid.matrix <- function(input,data,grid.config=grid.config,...){
	temp <- matrix(data=data,...)
	
	if(input=="clusterTab"){
		grid.config$clusterTab <- temp
		return(grid.config)
	}
	if(input=="plotdiagTab"){
		grid.config$plotdiagTab <- temp
		return(grid.config)
	}
	
}


.build.command.argument <- function(current.frame,command){
	
	if(current.frame$type=="entryfields"){
		
		number.entries <- length(current.frame$arguments)
		arguments <- current.frame$arguments
		
		for(j in 1:number.entries){
			
			if(current.frame$argument.types[j]=="num"){
				add.command <- if(tclvalue(current.frame$entry.vars[[j]])==""){""} else {paste(",",arguments[j],"=",tclvalue(current.frame$entry.vars[[j]]),sep="")}
			}
			if(current.frame$argument.types[j]=="char"){
				add.command <- if(tclvalue(current.frame$entry.vars[[j]])==""){""} else {paste(",",arguments[j],"='",tclvalue(current.frame$entry.vars[[j]]),"'",sep="")}
				
			}
			
			command <- paste(command,add.command,sep="")
			
		}
		return(command)
	}
	
	if(current.frame$type=="radiobuttons"){
		
				
		temp <- (tclvalue(current.frame$radioVar))
		
		if(grepl("BUTTONSTART",temp,fixed=TRUE)){
			temp <- gsub("BUTTONSTART","",temp,fixed=TRUE)
		}
		
		if(current.frame$argument.types=="char"){
			add.command <- paste( ",",current.frame$arguments,"='",temp,"'",sep=""   )
		}
		if(current.frame$argument.types=="num"){
			add.command <- paste( ",",current.frame$arguments,"=",temp,sep=""   )
		}	
			
		command <- paste(command,add.command,sep="")
		return(command)
	}
	
	if(current.frame$type=="checkboxes"){
		
		number.checks <- length(current.frame$arguments)
		arguments <- current.frame$arguments
		
		for(j in 1:number.checks){
			
#			temp.command <- paste("temp.var <- as.character(tclvalue(",arguments[[j]],"Variable))" ,sep="")
#			.eval.command(temp.command)
			temp.var <- as.character(tclvalue(current.frame$checkVar[[j]]))
			
			if(temp.var=="1"){check.var <- TRUE} else {check.var <- FALSE}
			
			add.command <- paste(",",arguments[j],"=",check.var,sep="")
			
			command <- paste(command,add.command,sep="")
			
		}
		return(command)
	}
	
	if(current.frame$type=="valuesliders"){
		number.sliders <- length(current.frame$arguments)
		arguments <- current.frame$arguments
		
		for(j in 1:number.sliders){
			
			add.command <- paste(",",arguments[j],"=",tclvalue(current.frame$slider.vars[[j]]),sep="")
			
			command <- paste(command,add.command,sep="")
			
		}
		return(command)
		
	}
	
	if(current.frame$type=="spinboxes"){
		number.spins <- length(current.frame$arguments)
		arguments <- current.frame$arguments
		
		for(j in 1:number.spins){
			
			add.command <- paste(",",arguments[j],"=",tclvalue(current.frame$spin.vars[[j]]),sep="")
			command <- paste(command,add.command,sep="")
			
		}
		return(command)
	}
	
	
}



.transform.vector2text <- function(x){
	if(length(x)==0){return("c()")}
	
	out <- "c("
	for(i.arg in 1:length(x)){
		out <- paste(out,"'",x[i.arg],"'",sep="")
		if(i.arg!=length(x)){out <- paste(out,",",sep="")}
	}
	out <- paste(out,")",sep="")
	return(out)
}



.build.button.function <- function(function.command,arg.names,button_result,new.frames,save){ 
	
	
	for(i.frame in arg.names){
	
		boolean <- sapply(new.frames$plotdiagTab,FUN=function(x){x$frame.name==i.frame})
		temp.index <- which(boolean==TRUE)
		if(sum(boolean) ==0){stop(paste("'",i.frame,"' is not defined in 'new.frames' object",sep=""))}
		if(sum(boolean)>1){stop(paste("'",i.frame,"' is defined multiple times in 'new.frames' object",sep=""))}
		if(sum(boolean)==1){
			current.arg.frame <- new.frames$plotdiagTab[[temp.index]]
		
			function.command <- .build.command.argument(current.arg.frame,function.command)
		}
	}

	function.command <- paste(function.command,")" ,sep="")
	function.command <- gsub("\\(,","\\(",function.command) # Fixing the case when no data or otherarg is used (function(,x=1))
	
	
	if(save==TRUE){function.command <- paste(button_result," <- ",function.command,sep="")}
	#cat(function.command)
	
	return(function.command)
}


.give.doublequote <- function(x){return(paste("\"",x,"\"",sep=""))}


Setwd <- function (x=TRUE) 
{
	wd <- tclvalue(tkchooseDirectory(initialdir = getwd(), parent = CommanderWindow()))
	if (wd != "") 
		doItAndPrint(paste("setwd(\"", wd, "\")", sep = ""))
}


.output.sparse.txt <- function(X,file){
	file <- paste(file,".txt",sep="")
	
	nrow <- dim(X)[1]
	ncol <- dim(X)[2]
	
	write(nrow,file=file,ncolumns=ncol,append=FALSE,sep=" ")
	write(ncol,file=file,ncolumns=ncol,append=TRUE,sep=" ")
	
	for(i.row in 1:nrow){
		ind <- which(X[i.row,]!=0)-1
		num <- length(ind)
		val <- X[i.row,ind+1]
		
		write(num,file=file,ncolumns=ncol,append=TRUE,sep=" ")
		write(ind,file=file,ncolumns=ncol,append=TRUE,sep=" ")
		write(val,file=file,ncolumns=ncol,append=TRUE,sep=" ")
	}
}



.is.binary.matrix <-function(x) {identical(as.vector(x),as.numeric(as.logical(x)))}


.binary.activematrix.check <- function(){
	
	x <- get(ActiveDataSet(),envir=.GlobalEnv)
	
	if(!.is.binary.matrix(as.matrix(x))){
		warning.command <- "warning('The current Active Data Set is not in binary format! Use the binarize option or a different data set.',call.=FALSE)"
		justDoIt(warning.command)
	}
}

.rcmdr.warning <- function(x){
	warning.command <- paste("warning('",x,"',call.=FALSE)",sep="")
	justDoIt(warning.command)
}


robust.fuse.support <- function(robust.list,RowxNumber,NumberxCol){
	
	to.delete <- c()
	for(i.index in 1:length(robust.list)){
		robust.info <- robust.list[[i.index]]$robust.inside
		
		new.rowxnumber <- RowxNumber[,robust.info[1]] 
		new.numberxcol <- NumberxCol[robust.info[1],] 
		
		for(i.index2 in 2:length(robust.info) ){
			
			new.rowxnumber <- new.rowxnumber | RowxNumber[,robust.info[i.index2]]
			new.numberxcol <- new.numberxcol | NumberxCol[robust.info[i.index2],]
			
		}
		RowxNumber[,robust.info[1]] <- new.rowxnumber 
		NumberxCol[robust.info[1],] <- new.numberxcol 
		
		
		to.delete <- c(to.delete, robust.info[-1])
		
	}
	
	RowxNumber <- RowxNumber[,-to.delete]
	NumberxCol <- NumberxCol[-to.delete,]
	
	return(list(RowxNumber=RowxNumber,NumberxCol=NumberxCol))
}




.makesearchdata <- function(){
	
	biclustGUI_biclusteringsearchdata <- .GetEnvBiclustGUI("biclustGUI_biclusteringsearchdata")
	
	if(is.null(biclustGUI_biclusteringsearchdata)){
		method_data <- data.frame()
				
		#Plaid
		method_data <- rbind(method_data,c("Plaid","Coherent Values","Additive","biclustplaid_WIN()","Plaid"))
		colnames(method_data) <- c("name","type","discovery","window","saveobject")
		for(i in 1:dim(method_data)[2]){method_data[,i] <- as.character(method_data[,i])}
		
		#CC
		method_data <- rbind(method_data,c("CC","Coherent Values","Additive","biclustCC_WIN()","CC"))
		
		#XMotifs
		method_data <- rbind(method_data,c("XMotifs","Coherent Evolution","NA","biclustXMotif_WIN()","XMotifs"))
		
		#Spectral
		method_data <- rbind(method_data,c("Spectral","Coherent Values","Multiplicative","biclustspectral_WIN()","Spectral"))
		
		#QuestMotif
		method_data <- rbind(method_data,c("QuestMotif","Coherent Evolution","NA","biclustquest_WIN()","Questmotif"))
		
		#Bimax
		method_data <- rbind(method_data,c("Bimax","Constant","NA","biclustbimax_WIN()","Bimax"))
		
		#Laplace Prior
		method_data <- rbind(method_data,c("Laplace Prior","Coherent Values","Multiplicative","fabialaplace_WIN()","FabiaLaplacePrior"))
		
		#Post-Projection
		method_data <- rbind(method_data,c("Post-Projection","Coherent Values","Multiplicative","fabiapostprojection_WIN()","FabiaPostProjection"))
		
		#Sparseness Projection
		method_data <- rbind(method_data,c("Sparseness Projection","Coherent Values","Multiplicative","fabiasparsenessprojection_WIN()","FabiaSparsenessProjection"))
		
		#SPARSE
		method_data <- rbind(method_data,c("SPARSE","Coherent Values","Multiplicative","fabiaSPARSE_WIN()","FabiaSPARSE"))
		
		#ISA # PLACEHOLDER
		method_data <- rbind(method_data,c("ISA","Coherent Evolution","NA","isadefault_WIN()","ISA"))
		
		#iBBiG # PLACEHOLDER
		method_data <- rbind(method_data,c("iBBiG","Constant","NA","iBBiG_WIN()","IBBIG"))
		
		#rQubic # PLACEHOLDER
		method_data <- rbind(method_data,c("Rqubic","Coherent Evolution","NA","rqubic_WINDOW()","Rqubic"))
		
		#BicARE # PLACEHOLDER
		method_data <- rbind(method_data,c("BicARE","Coherent Values","Additive","bicare_WINDOW()","BICARE"))
		
		#SSVD # PLACEHOLDER
		method_data <- rbind(method_data,c("SSVD","Coherent Values","Multiplicative","ssvd_WIN()","SSVD"))
		
		#S4VD # PLACEHOLDER
		method_data <- rbind(method_data,c("S4VD","Coherent Values","Multiplicative","s4vd_WIN()","S4VD"))
		
		# BiBit
		method_data <- rbind(method_data,c("BiBit","Constant","NA","bibit_WIN()","BiBit"))
		
		
		# Assigning to Global Variable
#		assign("biclustGUI_biclusteringsearchdata", method_data, envir = .GlobalEnv)
		.AssignEnvBiclustGUI("biclustGUI_biclusteringsearchdata",method_data)

		
	}
	
}


#.isISA <- function(x){
#	if(class(x)=="list"){
#		
#		if(length(names(x))==4){
#			if(all(names(x)==c("rows","columns","seeddata","rundata"))){
#				return(TRUE)
#			}
#			else{
#				return(FALSE)
#			}
#		}
#		else{
#			return(FALSE)
#		}
#	}
#	else{
#		return(FALSE)
#	}
#}


.makeResultList <- function(){
	globalVars <- ls(envir=.GlobalEnv)
	if(length(globalVars)==0){return(globalVars)}
	
	select <- sapply(globalVars,FUN=.isbiclustGUIresult)
	return(globalVars[select])
}

.isListofBiclustGUIresults <- function(x,asis=FALSE){
	
	if(class(x)=="character" & !asis){
		eval(parse(text=paste("x <- ",x,sep="")))
	}
	
	if(class(x)=="list"){
		return(all(unlist(lapply(x,FUN=.isbiclustGUIresult,asis=TRUE))))
	}else{
		return(FALSE)
	}
}

.makeSuperbiclustResultList <- function(){ # Same as .makeResultList, but should also recognise list of results..
	globalVars <- ls(envir=.GlobalEnv)
	if(length(globalVars)==0){return(globalVars)}
	
	select <- rep(FALSE,length(globalVars))
	
	for(i in 1:length(globalVars)){
		x <- globalVars[i]
		eval(parse(text=paste0("x <- ",x)))
		
		if(.isbiclustGUIresult(x,asis=TRUE)){
			select[i] <- TRUE
		}else{
			select[i] <- .isListofBiclustGUIresults(x,asis=TRUE)
		}

	}
	return(globalVars[select])
}

as.ExprSet <- function(x){
	datamatrix <- as.matrix(x)
	out <- new("ExpressionSet",exprs=datamatrix)
	return(out)
}





.putbefore <- function(colnames,pre){
	for(i.names in 1:length(colnames)){
		colnames[i.names] <- paste0(pre,colnames[i.names])
	}
	return(colnames)
}


.correctdataforresult <- function(result){
	
	# Is there an active dataset?
	if(!activeDataSetP()){
		justDoIt("warning('Please select an Active Dataset',call.=FALSE)")
		return(FALSE)
	}
	else{
	
		resultname <- deparse(substitute(result))
		
		biclustering.objects <- .GetEnvBiclustGUI("biclustering.objects")
#		if(!("biclustering.objects" %in% ls(envir=.GlobalEnv))){
		if(is.null(biclustering.objects)){
			biclustering.objects <- list()
			
			biclustering.objects$all <- character()
			biclustering.objects$bcdiag <- c()
			biclustering.objects$superbiclust <- c()
			biclustering.objects$dataconnect <- data.frame(result=character(),data=character(),stringsAsFactors=FALSE)
			
#			assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
			.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
			
		}
		
		dataconnect <- biclustering.objects$dataconnect
	
		## Result is in object, but maybe wrong, use correct active dataset
		if(resultname %in% dataconnect$result){
			correct_data <- dataconnect$data[which(dataconnect$result==resultname)]
			# Dataset is available
			if(correct_data %in% listDataSets()){
				# Already using correct dataset
				if(correct_data==ActiveDataSet()){
					# Nothing happens
					return(TRUE)
				}
				# Not yet using correct dataset
				else{
					justDoIt(paste0("warning('Active Dataset is changed to ",correct_data,"',call.=FALSE)"))
					justDoIt(paste0("activeDataSet('",correct_data,"')"))
					return(TRUE)
				}
			}
			# Dataset is not available
			else{
				justDoIt(paste0("warning('The correct dataset, ",correct_data,", is not loaded in in R-Commander. Please load it.',call.=FALSE)"))
				return(FALSE)
			}
		}
		
		## Result is not in object, was not done in this session. Current active data will be used, but dimensions will be checked
		else{
			justDoIt(paste0("warning('Result was not obtained in this session. Corresponding data could not be determined, therefore the Active Dataset will be used.',call.=FALSE)"))
			
			### NEED TO HAVE BICLUST OBJECT HERE, CHECK IF THIS ALSO WORKS WITH OTHER IMPLEMENTATIONS APART FROM EXTRACTING
			### SIMPLY TRANSFORM RESULT WITH .2BICLUST!! (MEMORY?) FABIA THRESH DOES NOT MATTER HERE
			result.biclust <- .tobiclust(result)
			# Do the dimensions of the active dataset correspond with the biclust result?
			matrixdata <- as.matrix(get(ActiveDataSet(),envir=.GlobalEnv))
			
			nrow <- dim(matrixdata)[1]
			ncol <- dim(matrixdata)[2]
			nrow_biclust <- dim(result.biclust@RowxNumber)[1]
			ncol_biclust <- dim(result.biclust@NumberxCol)[2]
			if(!(nrow==nrow_biclust & ncol==ncol_biclust  )){
				justDoIt("warning('Dimensions of biclustering result and active dataset do not agree. Please select the correct Active Dataset',call.=FALSE)")
				return(FALSE)
			}
			else{
				return(TRUE)
			}
			
	
			# DON't FORGET TO ADD TRUE AND FALSE
		}
	
	}
	
}


.checkplotgridpref <- function(){
	#Note: Old code in newmethod_function and newtool_function  (which this function replaces)
	#if(!is.null(dev.list())){par(mfrow=c(1,1))}
	
	if(!is.null(dev.list())){ #Only do something if there is an active graphics device
		current.griddim1 <- par()$mfrow[1]
		current.griddim2 <- par()$mfrow[2]
		
		biclustering.objects <- .GetEnvBiclustGUI("biclustering.objects")
		if(!("plotgrid" %in% names(biclustering.objects))){
			biclustering.objects$plotgrid <- c(1,1)
#			assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
			.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
						
		}
		
		griddim1 <- biclustering.objects$plotgrid[1]
		griddim2 <- biclustering.objects$plotgrid[2]
		
		if(current.griddim1!=griddim1 & current.griddim2!=griddim2){
			par(mfrow=c(griddim1,griddim2))
		}
		
	}
}


.EnvBiclustGUI <- new.env()


.GetEnvBiclustGUI <- function(x){
	if(!exists(x,envir=.EnvBiclustGUI,inherits=FALSE)){
		return(NULL)
	}
	else{
		return(get(x=x,envir=.EnvBiclustGUI,inherits=FALSE))
	}
	
}

.AssignEnvBiclustGUI <- function(x,value){
	assign(x=x,value=value,envir=.EnvBiclustGUI)
}
