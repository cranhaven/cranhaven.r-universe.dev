# Project: BiclustGUI
# 
# Author: Gebruiker
###############################################################################

#tkmessageBox(title="Error Message", message="Homogeneity parameter has to be in interval [0,1]!",icon="warning",type="ok")

clearresults_WINDOW <- function(){
	
	## USE TKMESSAGEBOX
	ReturnVal <- tkmessageBox(title="Clear all Bicluster Results",message = "Are you sure?",icon = "warning", type = "yesno", default = "no")
	if(tclvalue(ReturnVal)=="yes"){
		rm(list=.makeSuperbiclustResultList(),envir=.GlobalEnv)
		#rm(list="biclustering.objects",envir=.GlobalEnv)
		rm(list="biclustering.objects",envir=.EnvBiclustGUI)
		
		# Also remove objects that end on "INFO"
		info.index <- sapply(ls(envir=.GlobalEnv),FUN=function(x){grepl("[[:alnum:]]+INFO$",x)})
		rm(list=ls(envir=.GlobalEnv)[info.index],envir=.GlobalEnv)
	}
	
	
	## DO THE WARNING BOX MANUALLY
	
#	initializeDialog(title = gettextRcmdr("Clear all Bicluster Results...")) 
#	
#	onOK <- function(){
#		rm(list=.makeResultList(),envir=.GlobalEnv)
#		rm(list="biclustering.objects",envir=.GlobalEnv)
#		
#		if (GrabFocus()) 
#			tkgrab.release(top)
#		tkdestroy(top)
#		tkfocus(CommanderWindow())
#		
#	}
#	
#	onCancel <- function() {
#		if (GrabFocus()) 
#			tkgrab.release(top)
#		tkdestroy(top)
#		tkfocus(CommanderWindow())
#	}
#	
#	
#	
#	qFrame <- tkframe(top)
#	buttonFrame <- tkframe(top)
#	
#	tkgrid(labelRcmdr(qFrame,text=gettextRcmdr("Are you sure?")),sticky="s",padx="25",pady="12")
#	tkgrid(qFrame,sticky="s",padx="30")
#	
#	yesButton <- buttonRcmdr(buttonFrame,command=onOK,text=gettextRcmdr("Yes"),foreground="darkgreen",default="active",width="12",borderwidth=3)
#	noButton <- buttonRcmdr(buttonFrame,command=onCancel,text=gettextRcmdr("No"),foreground="darkgreen",default="active",width="12",borderwidth=3)
#	
#	tkgrid(yesButton,noButton,sticky="swe")
#	tkgrid(buttonFrame,sticky="swe")
#	
#	tkgrid.columnconfigure(buttonFrame, 0, weight=1)
#	tkgrid.columnconfigure(buttonFrame, 1, weight=1)
#	tkgrid.configure(yesButton,sticky="w")
#	tkgrid.configure(noButton,sticky="e")
#
#	
#	dialogSuffix(onOK=onOK)
}



plotgridpref_WINDOW <- function(){
	initializeDialog(title = gettextRcmdr("Make Plot Grid Dimensions...")) 
	
	# Making biclustering object if necessary + adding the grid preferences if necessary
	biclustering.objects <- .GetEnvBiclustGUI("biclustering.objects")
#	if(!("biclustering.objects" %in% ls(envir=.GlobalEnv))){
	if(is.null(biclustering.objects)){
		biclustering.objects <- list()
		
		biclustering.objects$all <- character()
		biclustering.objects$bcdiag <- c()
		biclustering.objects$superbiclust <- c()
		biclustering.objects$dataconnect <- data.frame(result=character(),data=character(),stringsAsFactors=FALSE)
		
#		assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
		.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
		
	}
	if(!("plotgrid" %in% names(biclustering.objects))){
		biclustering.objects$plotgrid <- c(1,1)
		
#		assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
		.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
		
	}

	griddim1 <- biclustering.objects$plotgrid[1]
	griddim2 <- biclustering.objects$plotgrid[2]
	
	gridFrame <- tkframe(top)
	buttonFrame <- tkframe(top)
	rightbuttonFrame <- tkframe(buttonFrame)
	
	onOK <- function(){
		griddim1 <- tclvalue(grid1_vars)
		griddim2 <- tclvalue(grid2_vars)
		
		# in methodfunction: only use the par if the mfrow settings have been changed.. BUT is there a way to see if layout has been used? Need to something to get the current layout graphics
		# solution: save par() and do identical() if TRUE, do nothing, if FALSE, change back
		
		# ps: don't forget to check biclustering.objects and see if the default has been made (1,1)
		
		biclustering.objects$plotgrid <- c(as.numeric(griddim1),as.numeric(griddim2))
#		assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
		.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
		
		
		doItAndPrint(paste0("par(mfrow=c(",griddim1,",",griddim2,"))"))
		
		onCancel()
	}
	
	onDefault <- function(){
		biclustering.objects$plotgrid <- c(1,1)
#		assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
		.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
		
		
		doItAndPrint("par(mfrow=c(1,1))")
		onCancel()
	}
	
	onCancel <- function() {
		if (GrabFocus()) 
			tkgrab.release(top)
		tkdestroy(top)
		tkfocus(CommanderWindow())
	}
	
	tkgrid(labelRcmdr(top,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Dimensions:")),sticky="nw")
		
	grid1_entry <- tkframe(gridFrame)
	grid1_vars <- tclVar(paste0(griddim1))
	grid1_field <- ttkentry(grid1_entry,width=4,textvariable=grid1_vars)
	tkgrid(grid1_field,sticky="nw")
	
	grid2_entry <- tkframe(gridFrame)
	grid2_vars <- tclVar(paste0(griddim2))
	grid2_field <- ttkentry(grid2_entry,width=4,textvariable=grid2_vars)
	tkgrid(grid2_field,sticky="nw")
		
	tkgrid(grid1_entry,labelRcmdr(gridFrame,text=gettextRcmdr(" by ")),grid2_entry,sticky="nw")
	
	tkgrid(gridFrame,sticky="nw")
	emptyFrame <- tkframe(top)
	tkgrid(emptyFrame,padx=100)
	
	rightbuttonFrame <- tkframe(buttonFrame)
	
	okButton <- buttonRcmdr(buttonFrame,command=onOK,text=gettextRcmdr("Ok"),foreground="darkgreen",width="4",borderwidth=3)
	
	defaultButton <- buttonRcmdr(rightbuttonFrame,command=onDefault,text=gettextRcmdr("Default"),foreground="darkgreen",width="8",borderwidth=3)	
	exitButton <- buttonRcmdr(rightbuttonFrame,command=onCancel,text=gettextRcmdr("Exit"),foreground="darkgreen",width="8",borderwidth=3)
	tkgrid(defaultButton,exitButton)
	
	tkgrid(okButton,rightbuttonFrame,sticky="wes")
	tkgrid(buttonFrame,sticky="sew",pady=10)
	tkgrid.columnconfigure(buttonFrame, 0, weight=1)
	tkgrid.columnconfigure(buttonFrame, 1, weight=1)
	tkgrid.configure(okButton,sticky="w")
	tkgrid.configure(rightbuttonFrame,sticky="e")
	
	dialogSuffix(onOK=onOK,preventGrabFocus=TRUE)
	
}
