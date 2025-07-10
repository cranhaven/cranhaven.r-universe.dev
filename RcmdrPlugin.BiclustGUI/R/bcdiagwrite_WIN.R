# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################

# NOTE: Note that newtool can ALSO be used to make new dialogs bound a button in the plotdiag Frame !!!

bcdiagwrite_WINDOW <- function(methodname,fabia.thresZ=0.5,fabia.thresL=NULL){  
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	
	
	method_result <- gsub(" ","",methodname,fixed=TRUE)
	method_result <- gsub("-","",method_result,fixed=TRUE)	
	
	# Special Case names
	fabia.names <- c("Fabia Laplace Prior","Fabia Post-Projection","Fabia Sparseness Projection","Fabia SPARSE")
	
	## Special Case: Fabia  (Because of the need for thresholds)
	if(methodname %in% fabia.names){
		eval(parse(text=paste("method_class <- class(",method_result,")",sep=""))) # Can only do it here and not earlier, cause not sure of the object exists at this point
		if(method_class == "Factorization" ){
			#bcdiag.fabia <- TRUE
			extra.arg <- paste0(method_result,",mname='fabia',fabia.thresZ=",fabia.thresZ,",fabia.thresL=",fabia.thresL)
			
		}
		else{ # This handles the case if superbiclust has been applied to Fabia object
			#bcdiag.fabia <- FALSE
			result_temp <- .tobiclust_transf(method_result)
			extra.arg <- paste0(result_temp,",mname='biclust'")
		}
	}
	## General Case
	else{
		#bcdiag.fabia <- FALSE
		result_temp <- .tobiclust_transf(method_result)
		extra.arg <- paste0(result_temp,",mname='biclust'")
	}
	
	
	###############################################################################################################################################################################
	## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
	#####################################################
	
	
	toolname <- "BCDIAG Output"
	
	toolhelp <- "writeBic"
	
	
	# Do not change this line:
	input <- "plotdiagTab"
	
	#######################
	## MAKING THE WINDOW ##
	#######################
	
	### ADDING FRAMES ####
	
	# Idem as plotdiag tab.
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "entryframe"  
	argument.names <- c("Title","Delimiter (def.=' ')") 
	argument.types <- c("char","char")
	arguments <- c("bicname","delimiter")
	initial.values <- c("Output Result"," ")
	title <- ""
	border <- FALSE
	entry.width <- c("15","15")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	####		CHECK BOXES FRAME 			  ####
	#                               			 #
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "appendframe"
	argument.names <- c("Append?") 
	arguments <- c("append") 
	initial.values <- c(1) 
	title <- ""
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
	
	
#	####	    	MANUAL BUTTONS FRAME 			  ####
#	#                               					 #
#	
#	type <- "buttons"
#	
#	# Change variables accordingly:
#	frame.name <- "button_setwd"  
#	button.name <- "Set Work. Dir."  
#	button.function <- "Setwd" 
#	button.data <- "" 
#	button.biclust <-  "" 
#	button.otherarg <- "x=TRUE"
#	save <- FALSE
#	show <- FALSE
#	arg.frames <- c() 
#	
#	# Do not change this line:
#	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button_write"  
	button.name <- "Write"  
	button.function <- "writeBic.GUI" 
	button.data <- "dset" 
	button.biclust <-  "" 
	button.otherarg <- paste0(",bicResult=",extra.arg)
	save <- FALSE
	arg.frames <- c("entryframe","appendframe") 
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
	### CONFIGURING GRID ###
	grid.config <- .grid.matrix(input=input,c("entryframe","appendframe","button_write",NA),byrow=TRUE,nrow=2,ncol=2,grid.config=grid.config)
	
	
	### COMBINING ROWS ###
	grid.rows <- .combine.rows(input=input,rows=c(1,2),title="Writing a Summary Output to text-file:",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	##################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL NEW TOOL FUNCTION ##
	##################################################################
	
	newtool_template(toolname=toolname,methodname=methodname,toolhelp=toolhelp,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames)
	
	
}


bcdiaghighlight_WINDOW <- function(methodname,dset,bnum,bres,mname="biclust",fabia.thresZ=0.5,fabia.thresL=NULL){
	initializeDialog(title = gettextRcmdr(paste0(methodname,"- BCDIAG - Select Genes/Samples for Line Plot")),use.tabs=FALSE) 
	
	
	method_result <- gsub(" ","",methodname,fixed=TRUE)
	method_result <- gsub("-","",method_result,fixed=TRUE)
	method_result <- gsub("&","",method_result,fixed=TRUE)
	

#	index <- BcDiag:::indexedBic(dset=dset,bres=bres,mname=mname,bnum=bnum,fabia.thresZ=fabia.thresZ,fabia.thresL=fabia.thresL)
	index <- indexedBic(dset=dset,bres=bres,mname=mname,bnum=bnum,fabia.thresZ=fabia.thresZ,fabia.thresL=fabia.thresL)

	index.rows <- index[[1]]
	index.cols <- index[[2]]
	
	rows.names <- rownames(dset)[index.rows]
	cols.names <- colnames(dset)[index.cols]
	
	
	onOK <- function(){
		
		# create string of vector
		rows.sel <- as.integer(tkcurselection(rowsBox))+1
		cols.sel <- as.integer(tkcurselection(colsBox))+1
		
		rows.vector <- paste0("c(",paste0(as.character(index.rows[rows.sel]),collapse=","),")")
		cols.vector <- paste0("c(",paste0(as.character(index.cols[cols.sel]),collapse=","),")")

		
		biclustering.objects <- .GetEnvBiclustGUI("biclustering.objects")
		
		eval(parse(text=paste0("temp.env <- biclustering.objects$ENVIR$BCDIAG",method_result)))
		
		eval(parse(text=paste0("tclvalue(new.frames$plotdiagTab[[1]]$entry.vars[[1]]) <- '",rows.vector,"'")),envir=temp.env)
		eval(parse(text=paste0("tclvalue(new.frames$plotdiagTab[[1]]$entry.vars[[2]]) <- '",cols.vector,"'")),envir=temp.env)
		# eval this in temp.env
#		tclvalue(new.frames$plotdiagTab[[8]]$frame$entry.vars[[1]]) <- ""
#		tclvalue(new.frames$plotdiagTab[[8]]$frame$entry.vars[[2]]) <- ""
		if (GrabFocus()) 
			tkgrab.release(top)
		tkdestroy(top)
		tkfocus(CommanderWindow())
	}
	
	
	onCancel <- function() {
		
		#return input back to c()
		biclustering.objects <- .GetEnvBiclustGUI("biclustering.objects")
		eval(parse(text=paste0("temp.env <- biclustering.objects$ENVIR$BCDIAG",method_result)))
		
		eval(parse(text=paste0("tclvalue(new.frames$plotdiagTab[[1]]$entry.vars[[1]]) <- 'c()'")),envir=temp.env)
		eval(parse(text=paste0("tclvalue(new.frames$plotdiagTab[[1]]$entry.vars[[2]]) <- 'c()'")),envir=temp.env)
		
		
		if (GrabFocus()) 
			tkgrab.release(top)
		tkdestroy(top)
		tkfocus(CommanderWindow())
	}
	
	mainFrame <- tkframe(top)
	
	rowsFrame <- tkframe(mainFrame)
	colsFrame <- tkframe(mainFrame)
	
	rowsBox <- tklistbox( rowsFrame , height=8, exportselection="FALSE",
			selectmode="multiple", background="white")
	for (rows in rows.names) tkinsert(rowsBox, "end", rows)
	rowsScroll <- ttkscrollbar(rowsFrame,command=function(...) tkyview(rowsBox, ...))
	tkconfigure(rowsBox, yscrollcommand=function(...) tkset(rowsScroll, ...))
	if(length(rowsFrame)!=0){tkselection.set(rowsBox,0)}
	
	colsBox <- tklistbox( colsFrame , height=8, exportselection="FALSE",
			selectmode="multiple", background="white")
	for (cols in cols.names) tkinsert(colsBox, "end", cols)
	colsScroll <- ttkscrollbar(colsFrame,command=function(...) tkyview(colsBox, ...))
	tkconfigure(colsBox, yscrollcommand=function(...) tkset(colsScroll, ...))
	if(length(colsFrame)!=0){tkselection.set(colsBox,0)}
	
	
	
	tkgrid(labelRcmdr(rowsFrame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("BC Row Names:")),sticky="nw")
	tkgrid(rowsBox,rowsScroll,padx="4") #,sticky="ns"
	tkgrid.configure(rowsScroll,sticky="ns")
	
	tkgrid(labelRcmdr(colsFrame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("BC Col Names:")),sticky="nw")
	tkgrid(colsBox,colsScroll,padx="4") #,sticky="ns"
	tkgrid.configure(colsScroll,sticky="ns")
		
	tkgrid(rowsFrame,colsFrame,sticky="nw",padx="6",pady="6")
	
	buttonFrame <- tkframe(top)
	okButton <- buttonRcmdr(buttonFrame,command=onOK,text=gettextRcmdr("Set"),foreground="darkgreen",default="active",width="12",borderwidth=3)
	cancelButton <- buttonRcmdr(buttonFrame,command=onCancel,text=gettextRcmdr("Cancel"),foreground="darkgreen",default="active",width="12",borderwidth=3)
	
	
	tkgrid(okButton,cancelButton,sticky="ew")
	tkgrid.columnconfigure(buttonFrame, 0, weight=1)
	tkgrid.columnconfigure(buttonFrame, 1, weight=1)
	tkgrid.configure(okButton,sticky="w")
	tkgrid.configure(cancelButton,sticky="e")
	
	tkgrid(mainFrame)
	tkgrid(buttonFrame,sticky="sew",pady="10")
			
	
	dialogSuffix(use.tabs=FALSE, grid.buttons=FALSE,onOK=onOK,preventGrabFocus=TRUE)
	
	
}
