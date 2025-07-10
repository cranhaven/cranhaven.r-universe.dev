# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################

# methodname


cluster_template <- function(methodname="",methodfunction,methodhelp="",data.arg,other.arg,methodseed=methodseed,grid.config=grid.config,grid.rows=grid.rows,new.frames,superbiclust.comp,bcdiag.comp,data.transf="matrix",data.discr=FALSE,data.bin=FALSE,extrabiclustplot=FALSE,methodshow=TRUE,methodsave=TRUE,extrabiclustforisa=FALSE){
	
	# Saving the environment in the biclustering.objects + delete old environment of this window to clear memory!!!!!
	method_result <- gsub(" ","",methodname,fixed=TRUE)
	method_result <- gsub("-","",method_result,fixed=TRUE)
	.update.biclustering.object(method_result,where="envir",ENVIR=environment())
	
	#########################################################################################################################################################
	## General preparation ##
	#########################
	
	# Making the frames #
	initializeDialog(title = gettextRcmdr(paste("Biclustering - ",methodname,sep="")), use.tabs=TRUE,tabs=c("clusterTab","plotdiagTab"))
	
	# Setting up the general frames in the tabs
	clusterFrame <- tkframe(clusterTab)
	plotdiagFrame <- tkframe(plotdiagTab)
	

	
	##########################################################################################################################################################
	##########################################################################################################################################################
	
	
	##########################################################################################################################################################
	## THE CLUSTER FRAME & PLOTDIAG FRAME ##
	########################################
	

	
	for(Tab in c(1,2)){
	
	if(length(new.frames[[Tab]])!=0){
	
	##########################################################
	## Determining if there are rowframes, if so make them: ##
	##########################################################
	
	if(length(grid.rows[[Tab]])!=0){
		for(i in 1:length(grid.rows[[Tab]])){
			
			if(Tab==1){
				row.command <- paste("clusterFrame_row",i," <- .make.correct.frame(grid.rows[[1]][[",i,"]]$title,grid.rows[[1]][[",i,"]]$border,clusterFrame)",sep="")
				.eval.command(row.command)
			
				# Special case for a rowframe: NO border, but Title:
				if(grid.rows[[1]][[i]]$title!="" & grid.rows[[1]][[i]]$border==FALSE){
					temp.command <- paste("tkgrid(labelRcmdr(clusterFrame_row",i ,",fg=getRcmdr('title.color'),font='RcmdrTitleFont',text=gettextRcmdr(grid.rows[[1]][[",i,"]]$title)),sticky='nw')"    ,sep="")
					.eval.command(temp.command)
				}
			}
			
			if(Tab==2){
				row.command <- paste("plotdiagFrame_row",i," <- .make.correct.frame(grid.rows[[2]][[",i,"]]$title,grid.rows[[2]][[",i,"]]$border,plotdiagFrame)",sep="")
				.eval.command(row.command)
				
				# Special case for a rowframe: NO border, but Title:
				if(grid.rows[[2]][[i]]$title!="" & grid.rows[[2]][[i]]$border==FALSE){
					temp.command <- paste("tkgrid(labelRcmdr(plotdiagFrame_row",i ,",fg=getRcmdr('title.color'),font='RcmdrTitleFont',text=gettextRcmdr(grid.rows[[2]][[",i,"]]$title)),sticky='nw')"    ,sep="")
					.eval.command(temp.command)
				}
				
				
			}
			
			
		}
		
	}
	
	
	
	##########################
	## Making of the frames ##
	##########################
	
	for(ii in 1:length(new.frames[[Tab]])){    # Had to use `ii` as radioButtons had an `i`for loop which was interfering with thisone

		current.frame <- new.frames[[Tab]][[ii]]
		frame.name <- current.frame$frame.name
		
		###### Determine if current.frame should be put in a rowframe or clusterframe ( + title,border options) #####
		
		if(length(grid.rows[[Tab]])!=0){			
			temp.names <- lapply(grid.rows[[Tab]],FUN=function(x){return(x$name.frames)})
			boolean <- sapply(temp.names,FUN=function(x){return( frame.name %in% x )})
			
			if(sum(boolean)==1){
				if(Tab==1){
					window <- paste("clusterFrame_row",which(boolean==TRUE),sep="")	
				}
				if(Tab==2){
					window <- paste("plotdiagFrame_row",which(boolean==TRUE),sep="")	
				}
			}	
			else{
				# If sum>2 an error was made, namely 2 times the same frame name. (Normally this is troubleshooted in .grid.rows or .grid.matrix
				if(Tab==1){
					window <- "clusterFrame"
				}
				if(Tab==2){
					window <- "plotdiagFrame"
				}
			}			
		}else {
			if(Tab==1){
				window <- "clusterFrame"
			}
			if(Tab==2){
				window <- "plotdiagFrame"
			}
		}
		
		
		##### Make the frame in which the current frame will be placed (for all kinds of types)
			frame.command <- paste("current.frame$frame <- .make.correct.frame(current.frame$title,current.frame$border,",window,")")
			.eval.command(frame.command)
			
			#cat(frame.command)
		
		## MAKE A FRAME DEFINED ON THE TYPE
		#new.frames[[ii]] <- .make.type.frame(current.frame,input="clusterTab")  # DOES NOT WORK PROPERLY -> some TCL objects start working incorrectly if this part is put inside another function
		
	###### ENTRY FIELDS #########################################################################################################
	
	if(current.frame$type=="entryfields"){
		
#			# Make the entry frame (containing ALL entries)  (DEPENDING ON TITLE AND BORDER VALUES)
#			
#			frame.command <- paste("current.frame$frame <- .make.correct.frame(current.frame$title,current.frame$border,",window,")")
#			.eval.command(frame.command)
		
		# Depending on other kind of boxes, this maybe can be put more upwards				
		
		#cat("E")						
		
		number.entries <- length(current.frame$arguments)
		arguments <- current.frame$arguments
		argument.names <- current.frame$argument.names
		initial.values <- current.frame$initial.values
		
		# Making the element which will contain the separate entry frames
		
		current.frame$entry.frames <- list()
		current.frame$entry.vars <- list()
		current.frame$entry.fields <- list()
		
		# Make title when NOT using a border (special case of 'title = "a title"' & 'border=FALSE'
		
		if(current.frame$title!="" & current.frame$border==FALSE){
			tkgrid(labelRcmdr( current.frame$frame,fg=getRcmdr("title.color"),font="RcmdrTitleFont"   ,text=gettextRcmdr(current.frame$title)),sticky="nw")
			
		}
		#cat(2)
		
		# Make frames inside entry frame for each argument
		for(j in 1:number.entries){
			
			current.frame$entry.frames[[j]] <- tkframe(current.frame$frame)
			current.frame$entry.vars[[j]] <- tclVar(paste(initial.values[j]))
			current.frame$entry.fields[[j]] <- ttkentry(current.frame$entry.frames[[j]],width=current.frame$entry.width[j],textvariable=current.frame$entry.vars[[j]])
			
			
			
			tkgrid(labelRcmdr(current.frame$entry.frames[[j]],text=gettextRcmdr(paste(argument.names[j],": ",sep=""))),current.frame$entry.fields[[j]],sticky="nw")
			tkgrid(current.frame$entry.frames[[j]],sticky="ne")
			
			
		}
		#cat(3)
		
		#print(new.frames)
		new.frames[[Tab]][[ii]] <- current.frame
		#print(new.frames)
	}
	
	###### RADIO BUTTONS ##########################################################################################
	if(current.frame$type=="radiobuttons"){
		#cat("R")
		
		# Setting up the radio buttons
		current.frame$argument.values <- sapply(current.frame$argument.values,FUN=function(x){return(paste("BUTTONSTART",x,sep=""))})
		current.frame$initial.values <- paste("BUTTONSTART",current.frame$initial.values,sep="")
		
		radioButtons(current.frame$frame,name=frame.name,buttons=current.frame$argument.values,values=current.frame$argument.values,labels=gettextRcmdr(current.frame$argument.names),initialValue=current.frame$initial.values,title="")
		
		# Make title when NOT using a border (special case of 'title = "a title"' & 'border=FALSE')
		if(current.frame$title!="" & current.frame$border==FALSE){
			tkgrid(labelRcmdr( current.frame$frame,fg=getRcmdr("title.color"),font="RcmdrTitleFont"   ,text=gettextRcmdr(current.frame$title)),sticky="nw")
			
		}
		
		# Saving the tclVar variable of the radioframe
		eval(parse(text=paste("current.frame$radioVar <-",frame.name,"Variable",sep="" )))
		
		# Putting down the radio button frame (which is generated automatically by radioButtons)
		radio.command <- paste("tkgrid(",frame.name,"Frame,sticky='nw')",sep="")
		.eval.command(radio.command)
		
		#cat(radio.command)
		
		new.frames[[Tab]][[ii]] <- current.frame
				
		
		#print(new.frames)
	}
	
	###### CHECK BOXES ##########################################################################################
	
	if(current.frame$type=="checkboxes"){
		#cat("C")
		
		# Setting up Check Boxes
		checkBoxes(current.frame$frame,frame=frame.name,boxes=paste(current.frame$arguments),initialValues=current.frame$initial.values,labels=sapply(current.frame$argument.names,FUN=gettextRcmdr))
		
		# Make title when NOT using a border (special case of 'title = "a title"' & 'border=FALSE')
		if(current.frame$title!="" & current.frame$border==FALSE){
			tkgrid(labelRcmdr( current.frame$frame,fg=getRcmdr("title.color"),font="RcmdrTitleFont"   ,text=gettextRcmdr(current.frame$title)),sticky="nw")
			
		}
		
		# Saving the tclVar variables of the checkbox frame
		current.frame$checkVar <- list()
		arguments <- current.frame$arguments
		
		for(j in 1:length(arguments)){
			temp.arg <- arguments[j]
			eval(parse(text=paste("current.frame$checkVar[[",j,"]] <- ",temp.arg,"Variable"  ,sep="")))
		}
		
		# Putting down the check button frame (which is generated automatically by checkBoxes)	
		
		check.command <- paste("tkgrid(",frame.name,",sticky='nw')",sep="")
		.eval.command(check.command)
		
		new.frames[[Tab]][[ii]] <- current.frame
		
		
	}
	###### VALUE SLIDERS ##########################################################################################
	
	if(current.frame$type=="valuesliders"){
		#cat("V")
		
		number.sliders <- length(current.frame$arguments)
		arguments <- current.frame$arguments
		argument.names <- current.frame$argument.names
		initial.values <- current.frame$initial.values
		
		# Making the element which will contain the separate slider frames
		
		current.frame$slider.frames <- list()
		current.frame$slider.vars <- list()
		current.frame$slider <- list()
		
		# Make title when NOT using a border (special case of 'title = "a title"' & 'border=FALSE')
		
		if(current.frame$title!="" & current.frame$border==FALSE){
			tkgrid(labelRcmdr( current.frame$frame,fg=getRcmdr("title.color"),font="RcmdrTitleFont"   ,text=gettextRcmdr(current.frame$title)),sticky="nw")
			
		}
		
		# Make frames inside valueslider frame for each slider
		
		for( j in 1:number.sliders){
			
			current.frame$slider.frames[[j]] <- tkframe(current.frame$frame)
			current.frame$slider.vars[[j]] <- tclVar(as.character(initial.values[j]))
			current.frame$slider[[j]] <- tkscale(current.frame$slider.frames[[j]],variable=current.frame$slider.vars[[j]],showvalue=TRUE,from=current.frame$from[j],to=current.frame$to[j],length=current.frame$length[j],resolution=current.frame$by[j],orient="horizontal")
			
			tkgrid(labelRcmdr(current.frame$slider.frames[[j]],text=gettextRcmdr(current.frame$argument.names[j])), current.frame$slider[[j]] ,sticky="sw")
			tkgrid(current.frame$slider.frames[[j]],sticky="nw")
			
			
		}
		
		new.frames[[Tab]][[ii]] <- current.frame
	}
	
	###### SPIN BOXES ##########################################################################################
	
	if(current.frame$type=="spinboxes"){
		
		#cat("S")
		
		number.spins <- length(current.frame$arguments)
		arguments <- current.frame$arguments
		argument.names <- current.frame$argument.names
		initial.values <- current.frame$initial.values
		
		# Making the element which will contain the separate spinbox frames
		
		current.frame$spin.frames <- list()
		current.frame$spin.vars <- list()
		current.frame$spin <- list()
		
		# Make title when NOT using a border (special case of 'title = "a title"' & 'border=FALSE')
		
		if(current.frame$title!="" & current.frame$border==FALSE){
			tkgrid(labelRcmdr( current.frame$frame,fg=getRcmdr("title.color"),font="RcmdrTitleFont"   ,text=gettextRcmdr(current.frame$title)),sticky="nw")
			
		}
		
		# Make frames inside spinboxes frame for each spinbox
		
		for(j in 1:number.spins){
			
			current.frame$spin.frames[[j]] <- tkframe(current.frame$frame)
			current.frame$spin.vars[[j]] <- tclVar(as.character(initial.values[j]))
			current.frame$spin[[j]] <- tkspinbox(current.frame$spin.frames[[j]],from=current.frame$from[j],to=current.frame$to[j],width=current.frame$entry.width,textvariable=current.frame$spin.vars[[j]] ,state="readonly",increment=current.frame$by[j])
			
			tkgrid(labelRcmdr(current.frame$spin.frames[[j]],text=gettextRcmdr(current.frame$argument.names[j])),current.frame$spin[[j]],sticky='nw')
			tkgrid(current.frame$spin.frames[[j]],sticky='nw')	
			
		}
		
		new.frames[[Tab]][[ii]] <- current.frame
		
	}
	
	
	###### MANUAL BUTTONS ########################################################################################
	# NOTE: ONLY CODED FOR PLOTDIAG TAB 
	
	if(current.frame$type=="buttons"){
		method_result <- gsub(" ","",methodname,fixed=TRUE)
		method_result <- gsub("-","",method_result,fixed=TRUE)
		button_result <- gsub(" ","",current.frame$button.name,fixed=TRUE)
		button_result <- gsub("&","",button_result,fixed=TRUE)
		
		
		function.command <- paste(current.frame$button.function,"(",sep="")
		first.arg=TRUE
		
		if(current.frame$button.data!=""){
			
			input.data <- ActiveDataSet()		
			
			if(current.frame$button.data.transf=="matrix"){input.data <- paste("as.matrix(",input.data,")",sep="")}
			if(current.frame$button.data.transf=="ExprSet"){input.data <- paste0("as.ExprSet(",input.data,")")}
			
			function.command <- paste(function.command,current.frame$button.data,"=",input.data  ,sep="")
			first.arg=FALSE
		}
		
		
		if(current.frame$button.biclust!=""){
			if(first.arg==TRUE){
				function.command <- paste(function.command,current.frame$button.biclust,"=",method_result,sep="")	
			}
			else{
				function.command <- paste(function.command,",",current.frame$button.biclust,"=",method_result,sep="")
			}
		}
			
#		if(current.frame$button.data=="" & current.frame$button.biclust=="" & current.frame$button.otherarg==""){
#			function.command <- paste(function.command,"...",sep="")
#		}
		
		if(current.frame$button.otherarg!=""){
			
			function.command <- paste(function.command,current.frame$button.otherarg,sep="")
			
			# old code:
			#function.command <- .build.button.function('",function.command,"',",arg.names,",'",button_result,"',new.frames,",save,")
		}
		
		#cat(function.command)
		arg.names <- .transform.vector2text(current.frame$arg.frames)
		
		save <- current.frame$save
		show <- current.frame$show

		# in this button command, it should be checked if method_result is in biclustering.objects$all
		temp.command <- paste("function(){
									biclustering.objects <- .GetEnvBiclustGUI(\"biclustering.objects\")

									if(is.null(biclustering.objects)){
										
										.rcmdr.warning('Apply Show Results first')
									}

									else{
										if(!('",method_result,"' %in% biclustering.objects$all)){
											.rcmdr.warning('Apply Show Results first')
										}

										if('",method_result,"' %in% biclustering.objects$all){
											function.command <- .build.button.function(\"",function.command,"\",",arg.names,",\"",button_result,"\",new.frames,",save,")


											if(",show,"==TRUE){
												doItAndPrint(function.command)
											}

											if(",show,"!=TRUE){
												justDoIt(function.command)
											}
											
											if(",save,"==TRUE){
												doItAndPrint('",button_result,"')
											}

											.checkplotgridpref()
										}
									}
								}",sep="")
		#cat(temp.command)
		eval(parse(text=paste("button.command <- ",temp.command,sep="")))
		
		current.frame$button.command <- button.command
		
		current.frame$buttonRcmdr <- buttonRcmdr(current.frame$frame,command=current.frame$button.command,text=gettextRcmdr(current.frame$button.name),foreground="darkgreen",default="active",width=current.frame$button.width,borderwidth=3)
		
		#OKbutton <- buttonRcmdr(resultsFrame,text=gettextRcmdr("Show Result"),foreground="darkgreen",width="12",command=onOK,default="active",borderwidth=3)
		
		tkgrid(current.frame$buttonRcmdr,sticky="s")
		
		new.frames[[Tab]][[ii]] <- current.frame
		#cat("MANUALBUTTONMADE")
		#cat(str(new.frames))
		
	}
	}
	
	########################################################################
	## Configuring of the frames (normal & combined rows) in CLUSTERFRAME ##
	########################################################################
	
	#cat(4)
	
	if(length(grid.rows[[Tab]])!=0){	special.rows <- lapply(grid.rows[[Tab]],FUN=function(x){return(x$rows)})
										special.rows.vector <- sort(unlist(special.rows))} else {special.rows.vector <- c()}
	row.index <- 1
							
	while(row.index <= dim(grid.config[[Tab]])[1]){
		
		## Placing 'rows' in grid####
					
		if((length(grid.rows[[Tab]])!=0) & (row.index %in% special.rows.vector) ){
			
			#cat(9)
				
			rowframe.index <- which(lapply(special.rows,FUN=function(x){return(row.index %in% x)}) ==TRUE)
				
			
			
			for(i in special.rows[[rowframe.index]]){
				grid.command <- paste("tkgrid(")
				for(column.index in 1:dim(grid.config[[Tab]])[2]){
					if(is.na(grid.config[[Tab]][i,column.index])){} else {grid.command <- paste(grid.command,"new.frames[[",Tab,"]][[",.find.frame(new.frames[[Tab]],grid.config[[Tab]][i,column.index]) ,"]]$frame,",sep="")}
					
				}
				grid.command <- paste(grid.command,"sticky='nw',padx=6,pady=6)")
				#cat(grid.command)
				.eval.command(grid.command)
			}			
			
		
			if(Tab==1){
				grid.command <- paste("tkgrid(clusterFrame_row",rowframe.index,",sticky='nw')",sep="")
			}
			if(Tab==2){
				grid.command <- paste("tkgrid(plotdiagFrame_row",rowframe.index,",sticky='nw')",sep="")
			}
			
			.eval.command(grid.command)
			#eval(parse(text=grid.command))
				
			row.index <- i+1
			
		}
			
				
		###########################
		
		else{	
			#cat(10)
			## Normal grid matrix####
			grid.command <- "tkgrid("
			for(column.index in 1:dim(grid.config[[Tab]])[2]){
				if(is.na(grid.config[[Tab]][row.index,column.index])){} else {grid.command <- paste(grid.command,"new.frames[[",Tab,"]][[",.find.frame(new.frames[[Tab]],grid.config[[Tab]][row.index,column.index]) ,"]]$frame,",sep="")}
			
			}
				
			grid.command <- paste(grid.command,"sticky='nw',padx=6,pady=6)",sep="")
			#cat(grid.command)
			.eval.command(grid.command)
			#eval(parse(text=grid.command))
			row.index <- row.index + 1
			#############################	
		}
	}

	
	#cat(5)

	#cat(6)
	
	}
	}
	
	
	
	## Making the discretize box in the clusterTab ##
	if(data.discr==TRUE){
		
		discrFrame <- ttklabelframe(clusterTab,text=gettextRcmdr("Discretize Data (Biclust)"))
		
		checkBoxes(discrFrame,frame="discrcheck",boxes="discr.check",initialValues=c(0),labels=gettextRcmdr("Discretize?"))
		
		discrentryFrame <- tkframe(discrFrame)
		discr.nof <- tclVar("10")
		discr.nof.field <- ttkentry(discrentryFrame,width="2",textvariable=discr.nof)

		discrquantFrame <- tkframe(discrFrame)
		checkBoxes(discrquantFrame,frame="discrquant",boxes="discr.quant",initialValues=c(0),labels=gettextRcmdr("Use quantiles? (else equally spaced)"))
		
		tkgrid(labelRcmdr(discrentryFrame,text=gettextRcmdr("Number of Levels")),discr.nof.field,sticky="nw")
		
		tkgrid(discrcheck,sticky="nw")
		tkgrid(discrquant,sticky="nw",padx=8)
		tkgrid(discrentryFrame,discrquantFrame,sticky="nw")
		#tkgrid(discrFrame,sticky="nw",padx=6,pady=6)  # this is added in the end
	}

	
	## Making the binarize box in the clusterTab
	
	
	if(data.bin==TRUE){
		
		binFrame <- ttklabelframe(clusterTab,text=gettextRcmdr("Binarize Data (Biclust)"))
		checkBoxes(binFrame,frame="bincheck",boxes="bin.check",initialValues=c(0),labels=gettextRcmdr("Binarize?"))
		
		binentryFrame <- tkframe(binFrame)
		bin.threshold <- tclVar("NA")
		bin.threshold.field <- ttkentry(binentryFrame,width="4",textvariable=bin.threshold)
		
		tkgrid(labelRcmdr(binentryFrame,text=gettextRcmdr("Threshold (NA=median)")),bin.threshold.field,sticky="nw")
		
		tkgrid(bincheck,sticky="nw")
		tkgrid(binentryFrame,sticky="nw")
		#tkgrid(binFrame,sticky="nw",padx=6,pady=6)
		
	}
	
	
	############################################
	## Making ONOK, ONCANCEL, ONHELP funtions ##
	############################################
	
	onOK <- function(){
		
		# Note: build in IF clause to convert data set to matrix
		
		# Building the command:
		method_result <- gsub(" ","",methodname,fixed=TRUE)
		method_result <- gsub("-","",method_result,fixed=TRUE)

	
	
		input.data <- ActiveDataSet()
		input.data.transf <- ""

		# Convert to data to matrix if asked:
		if(data.transf=="matrix"){
			input.data <- paste("as.matrix(",input.data,")",sep="")
			input.data.transf <- "as.matrix"
		}
		# Convert data to ExprSet if asked:
		if(data.transf=="ExprSet"){
			input.data <- paste("as.ExprSet(",input.data,")",sep="")
			input.data.transf <- "as.ExprSet"
		}
		
		
		# Make sure discretize and binarize are not checked at the same time
		GO <- TRUE
		if(data.discr==TRUE & data.bin==TRUE){
			if(tclvalue(discr.checkVariable)=="1" & tclvalue(bin.checkVariable)=="1"){GO <- FALSE }
		}
		

		if( GO==TRUE){
		
			if(methodsave==FALSE){
				methodshow <- FALSE
				command <- ""
			}	
			else{
				command <- paste0(method_result," <- ")
			}
			
			if(data.discr==TRUE & data.bin==TRUE){
			
				if(tclvalue(discr.checkVariable)=="1"){
				
					if(tclvalue(discr.quantVariable)=="1"){temp.quant <- TRUE}else{temp.quant <- FALSE}
					
					discr.command <- paste("x <- discretize(as.matrix(",ActiveDataSet(),"),nof=",tclvalue(discr.nof),",quant=",temp.quant,")",sep="")
					doItAndPrint(discr.command)
				
					command <- paste(command,methodfunction,"(",data.arg,"=",input.data.transf,"(x)",other.arg,sep="")
				
				}
			
				else if(tclvalue(bin.checkVariable)=="1"){
				
					bin.command <- paste("x <- binarize(as.matrix(",ActiveDataSet(),",)threshold=",tclvalue(bin.threshold),")",sep="")
					doItAndPrint(bin.command)
					
					command <- paste(command,methodfunction,"(",data.arg,"=",input.data.transf,"(x)",other.arg,sep="")
				
				}
			
				else{
					command <- paste(command,methodfunction,"(",data.arg,"=",input.data,other.arg,sep="")
				}
			}
		
			if(data.bin==TRUE & data.discr==FALSE){# I AM HERE
				if(tclvalue(bin.checkVariable)=="1"){
				
					bin.command <- paste("x <- binarize(as.matrix(",ActiveDataSet(),"),threshold=",tclvalue(bin.threshold),")",sep="")
					doItAndPrint(bin.command)
				
					command <- paste(command,methodfunction,"(",data.arg,"=",input.data.transf,"(x)",other.arg,sep="")
				
				}
				else{
					command <- paste(command,methodfunction,"(",data.arg,"=",input.data,other.arg,sep="")
				
				}
			}
		
			if(data.discr==TRUE & data.bin==FALSE){
				if(tclvalue(discr.checkVariable)=="1"){
				
					if(tclvalue(discr.quantVariable)=="1"){temp.quant <- TRUE}else{temp.quant <- FALSE}
				
					discr.command <- paste("x <- discretize(as.matrix(",ActiveDataSet(),"),nof=",tclvalue(discr.nof),",quant=",temp.quant,")",sep="")
					doItAndPrint(discr.command)
				
					command <- paste(command,methodfunction,"(",data.arg,"=",input.data.transf,"(x)",other.arg,sep="")
				
				}
				else{
					command <- paste(command,methodfunction,"(",data.arg,"=",input.data,other.arg,sep="")
				}
			
			}
		
			if(data.discr==FALSE & data.bin==FALSE){
				command <- paste(command,methodfunction,"(",data.arg,"=",input.data,other.arg,sep="")
			}
		
			#command <- paste(method_result,"<-",methodfunction,"(",data.arg,"=",input.data,other.arg,sep="")
		
			for(i in 1:length(new.frames[[1]])){
				current.frame <- new.frames[[1]][[i]]
			
				command <- .build.command.argument(current.frame,command)
			
			
			}
		
			command <- paste(command,")",sep="")
		
			if(methodseed==TRUE){doItAndPrint(paste("set.seed(",tclvalue(seedVar),")",sep=""))}
			
			if(methodsave==TRUE){
				doItAndPrint(command)
			}
			else{
				justDoIt(command)
			}
			
			#assign(method_result,eval(parse(text=method_result)),envir=.GlobalEnv)  # Not necessary, doItAndPrint already makes it 'global' do to printing it in the console
			if(methodshow==TRUE){doItAndPrint(method_result)}
			# NOTE: ADD GLOBAL VARIABLES TO 2 GLOBAL LIST: ALL BICLUST OBJECTS + ALL COMPATIBLE BICLUST OBJECTS (TRUE/FALSE)
		
			if(method_result %in% ls(,envir=.GlobalEnv)){.update.biclustering.object(method_result,where="all")}
		
			if(superbiclust.comp==TRUE){.update.biclustering.object(method_result,where="superbiclust")}
			if(bcdiag.comp==TRUE){.update.biclustering.object(method_result,where="bcdiag")}
			
			# Save analysis commands
			analysis.command <- c()
			if(data.bin==TRUE){if(tclvalue(bin.checkVariable)=="1"){analysis.command <- c(analysis.command,bin.command)}}
			if(data.discr==TRUE){if(tclvalue(discr.checkVariable)=="1"){analysis.command <- c(analysis.command,discr.command)}}
			analysis.command <- c(analysis.command,command)
			if(method_result %in% ls(,envir=.GlobalEnv)){.update.biclustering.object(method_result,where="analysis",ANALYSIS=analysis.command)}
			
			
		}
		else{justDoIt("warning('Cannot discretize and binarize at the same time')")}
		
	}
	onCancel <- function() {
		if (GrabFocus()) 
			tkgrab.release(top)
		tkdestroy(top)
		tkfocus(CommanderWindow())
	}
	
	onHelp <- function() {
		tkgrab.release(window)
		#doItAndPrint(paste("?",methodfunction,sep=""))
		print(help(methodhelp))
		#eval(parse(text=paste("?",methodfunction,sep="")))
	}
	
	
	
	#########################################################
	## Making the buttons in the clusterFrame (+ seed box) ##
	#########################################################
	
	clusterFrame_buttons <- tkframe(clusterTab)
	
	
	resultsFrame <- tkframe(clusterFrame_buttons)
	exithelpFrame <- tkframe(clusterFrame_buttons)
	
	OKbutton <- buttonRcmdr(resultsFrame,text=gettextRcmdr("Show Result"),foreground="darkgreen",width="12",command=onOK,default="active",borderwidth=3)
	exitButton <- buttonRcmdr(exithelpFrame, text = gettextRcmdr("Exit"), foreground = "red", width = "12", command = onCancel, borderwidth = 3)
	helpButton <- buttonRcmdr(exithelpFrame, text = gettextRcmdr("Help"), foreground = "red", width = "12", command = onHelp , borderwidth = 3)
	
	if(methodseed==TRUE){
		seedFrame <- tkframe(clusterFrame_buttons)
#		seedVar <- tclVar("666")
		
		random.seed <- as.character(round(runif(1,1,999)))
		seedVar <- tclVar(random.seed)
		
		seedField <- ttkentry(seedFrame,width="4",textvariable=seedVar)
	}

	tkgrid(OKbutton)
	if(methodseed==TRUE){tkgrid(seedField,labelRcmdr(seedFrame,text=gettextRcmdr("(Seed)")) )}
	tkgrid(exitButton,helpButton)
	
	tkgrid(resultsFrame,exithelpFrame)
	if(methodseed==TRUE){tkgrid(seedFrame,sticky="w")}
	
	##########################################################################################################################################################
	##########################################################################################################################################################
	
	##############################################
	## Making the buttons in the plotDiag frame ##
	##############################################
	
	plotdiagFrame_buttons <- tkframe(plotdiagTab)
	
	diagFrame <- tkframe(plotdiagFrame_buttons)
	exitFrame <- tkframe(plotdiagFrame_buttons)
	
	exitButton2 <- buttonRcmdr(exitFrame, text = gettextRcmdr("Exit"), foreground = "red", width = "12", command = onCancel, borderwidth = 3)
	tkgrid(exitButton2)
	
	on_extrabiclustplot <- function(){
		biclustextraplots_WINDOW(methodname)
	}
	
	on_bcdiag <- function(){
		bcdiag_WINDOW(methodname)
	}
	
	on_extraisaplot <- function(){
		isaextraplots_WINDOW(methodname)
	}
	
	on_superbiclust <- function(){
		superbiclust_WINDOW(methodname,methodseed,methodsave)  # add, methodseed
	}
	
	if(extrabiclustplot==TRUE){
		extrabiclustplotButton <- buttonRcmdr(diagFrame,text=gettextRcmdr("Extra Biclust Plots"),foreground="red",width="20",command=on_extrabiclustplot,borderwidth=3)
		tkgrid(extrabiclustplotButton,sticky="w")
	}
	
	if(extrabiclustforisa==TRUE){
		extraisaplotButton <- buttonRcmdr(diagFrame,text=gettextRcmdr("Extra Biclust Plots"),foreground="red",width="20",command=on_extraisaplot,borderwidth=3)
		tkgrid(extraisaplotButton,sticky="w")
	}
	
	
	if(bcdiag.comp==TRUE){
		bcdiagButton <- buttonRcmdr(diagFrame,text=gettextRcmdr("BcDiag"),foreground="red",width="12",command=on_bcdiag,borderwidth=3)
	
	}
	
	if(superbiclust.comp==TRUE){
		superbiclustButton <- buttonRcmdr(diagFrame,text=gettextRcmdr("Superbiclust"),foreground="red",width="12",command=on_superbiclust,borderwidth=3)
		
	}
	
	if(bcdiag.comp==TRUE & superbiclust.comp==TRUE){
		tkgrid(bcdiagButton,superbiclustButton,sticky="w")
	}
	if(bcdiag.comp==FALSE & superbiclust.comp==TRUE){
		tkgrid(superbiclustButton,sticky="w")
	}
	if(bcdiag.comp==TRUE & superbiclust.comp==FALSE){
		tkgrid(bcdiagButton,sticky="w")
	}
	
	tkgrid(diagFrame,exitFrame,sticky="w")
	##########################################################################################################################################################
	## Making the final grids ##
	############################ 
	
	tkgrid(clusterFrame,padx=5,pady=5,sticky="nw")
	
	if(data.discr==TRUE){tkgrid(discrFrame,sticky="nw",padx=6,pady=6)}
	if(data.bin==TRUE){tkgrid(binFrame,sticky="nw",padx=6,pady=6)}
	
	tkgrid(clusterFrame_buttons,padx=5,pady=7,sticky="sew")
	tkgrid(plotdiagFrame,padx=5,pady=5,sticky="nw")
	tkgrid(plotdiagFrame_buttons,padx=5,pady=7,sticky="sew")
	
	tkgrid.columnconfigure(clusterFrame_buttons, 0, weight=1)
	tkgrid.columnconfigure(clusterFrame_buttons, 1, weight=1)
	tkgrid.configure(resultsFrame,sticky="w")
	tkgrid.configure(exithelpFrame,sticky="e")
	
	tkgrid.columnconfigure(plotdiagFrame_buttons, 0, weight=1)
	tkgrid.columnconfigure(plotdiagFrame_buttons, 1, weight=1)
	tkgrid.configure(diagFrame,sticky="w")
	tkgrid.configure(exitFrame,sticky="e")
	
	dialogSuffix(use.tabs=TRUE, grid.buttons=FALSE,onOK=onOK,tabs=c("clusterTab","plotdiagTab"),tab.names=c("Biclustering","Plots & Diagnostics"),preventGrabFocus=TRUE)
	
}



#cluster_template()