# Project: BiclustGUI
# 
# Author: Gebruiker
###############################################################################

# MAKE BUTTON TO GIVE VERY SHORT SUMMARY OF RESULT
# FOLLOW UP WITH A BUTTON WICH LEADS TO A 2-TAB DIALOG -> BICLUST/BCDIAG PLOTS

extractbicluster_WINDOW <- function(){
	
	initializeDialog(title = gettextRcmdr("Extract Biclusters...")) 
	
	### PREPARATION & BUTTON-FUNCTIONS ###
	AllResults <- .makeResultList()
	
	onSetwd <- function(){	Setwd()	}
	
	onCancel <- function() {
		if (GrabFocus()) 
			tkgrab.release(top)
		tkdestroy(top)
		tkfocus(CommanderWindow())
	}
	
	# GENERAL FRAMES
	resultFrame <- tkframe(top)
	fabiaoptionsFrame <- tkframe(top)
	extract1Frame <- tkframe(top)
	extract2Frame <- tkframe(top)
	buttonFrame <- tkframe(top)
	
	# Button FUNCTIONS
	
	onOK <- function(){
		sel <- as.integer(tkcurselection(resultBox))+1
		if(length(AllResults)==0){
			justDoIt(paste0("warning('No available results',call.=FALSE)"))
		}
		else if(length(sel)==0){
			justDoIt(paste0("warning('No result selected',call.=FALSE)"))
		}
		else{
			#sel <- as.integer(tkcurselection(resultBox))+1
			sel.result.name <- AllResults[sel]
		
			eval(parse(text=paste0("sel.result <-",sel.result.name)))
		
			# Special Case: fabia (because of thresholds)
			if(class(sel.result)=="Factorization"){
			
				result <- .tobiclust_transf(sel.result.name,thresZ=paste0(tclvalue(thresZ_vars)),thresL=paste0(tclvalue(thresL_vars)))
			}
			# General Case
			else{
			
				result <- .tobiclust_transf(sel.result.name)
			}
		
			doItAndPrint(paste0("summary(",result,")"))
		}
	}
	
	
	onExtract <- function(){
		if(length(AllResults)==0){
			justDoIt(paste0("warning('No available results',call.=FALSE)"))
		}
		else{
			sel <- as.integer(tkcurselection(resultBox))+1
			sel.result.name <- AllResults[sel]
			
			eval(parse(text=paste0("sel.result <-",sel.result.name)))
			
			# Special Case: fabia (because of thresholds)
			if(class(sel.result)=="Factorization"){
				
				result <- .tobiclust_transf(sel.result.name,thresZ=paste0(tclvalue(thresZ_vars)),thresL=paste0(tclvalue(thresL_vars)))
			}
			# General Case
			else{
				
				result <- .tobiclust_transf(sel.result.name)
			}
			which <- tclvalue(which_vars)
			from <- tclvalue(from_vars)
			to <- tclvalue(to_vars)
			selection <- tclvalue(sel_vars)
			dim <- tclvalue(dim_vars)
			if(tclvalue(save_vars)=="1"){save <- TRUE}else{save <- FALSE}
			save.name <- tclvalue(savename_vars)
			
			eval(parse(text=paste0("temp.correct <- .correctdataforresult(",sel.result.name,")")))
			if(temp.correct){
				temp.command <- paste0(save.name," <- ExtractBiclustersGUI(result=",result,",which='",which,"',from=",from,",to=",to,",selection=",selection,",dim='",dim,"',save=",save,",save.name='",save.name,"')")
				doItAndPrint(temp.command)
				doItAndPrint(save.name)
			}
		}
		
		
	}
	
	
	### BICLUSTER RESULTS FRAME ##
	
	
	resultBox <- tklistbox( resultFrame , height=5, exportselection="FALSE",
			selectmode="single", background="white")
	for (result in AllResults) tkinsert(resultBox, "end", result)
	resultScroll <- ttkscrollbar(resultFrame,command=function(...) tkyview(resultBox, ...))
	tkconfigure(resultBox, yscrollcommand=function(...) tkset(resultScroll, ...))
	if(length(AllResults)!=0){tkselection.set(resultBox,0)}
	
	tkgrid(labelRcmdr(resultFrame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Biclustering Results:")),sticky="nw")
	
	
	
	summaryButton <- buttonRcmdr(resultFrame,command=onOK,text=gettextRcmdr("Summary"),foreground="darkgreen",default="active",width="12",borderwidth=3)
	
	
	
	tkgrid(resultBox,resultScroll,summaryButton) #,sticky="ns"
	tkgrid.configure(resultScroll,sticky="ns")
	tkgrid.configure(summaryButton,sticky="es",padx=5)
		
	
	### FABIA OPTIONS FRAME ###
	
	
	tkgrid(labelRcmdr(fabiaoptionsFrame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Fabia Result Options")),sticky="nw")
	
	
	thresZ_entry <- tkframe(fabiaoptionsFrame)
	thresZ_vars <- tclVar("0.5")
	thresZ_field <- ttkentry(thresZ_entry,width=6,textvariable=thresZ_vars)
	tkgrid(labelRcmdr(thresZ_entry,text=gettextRcmdr("Threshold Bicluster Sample: ")),thresZ_field,sticky="nw")
	tkgrid(thresZ_entry,sticky="ne")
	
	thresL_entry <- tkframe(fabiaoptionsFrame)
	thresL_vars <- tclVar("NULL")
	thresL_field <- ttkentry(thresL_entry,width=6,textvariable=thresL_vars)
	tkgrid(labelRcmdr(thresL_entry,text=gettextRcmdr("Threshold Bicluster Loading: ")),thresL_field,sticky="nw")
	tkgrid(thresL_entry,sticky="ne")

	
	
	### EXTRACT OPTIONS ###
	
	whichFrame <- tkframe(extract1Frame)
	dimFrame <- tkframe(extract1Frame)
	
	# Which Frame
	tkgrid(labelRcmdr(whichFrame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Which Biclusters?")),sticky="nw")
	radioButtons(whichFrame,name="whichRadio",buttons=c("range","selection","all"),values=c("range","selection","all"),labels=gettextRcmdr(c("Range: ","Selection: ","All")),initialValue="all",title="")
	which_vars <- whichRadioVariable

	
	whichEntry <- tkframe(whichFrame)
	fromtoFrame <- tkframe(whichEntry)
	
	from_entry <- tkframe(fromtoFrame)
	from_vars <- tclVar("1")
	from_field <- ttkentry(from_entry,width=3,textvariable=from_vars)
	tkgrid(labelRcmdr(from_entry,text=gettextRcmdr("From")),from_field,sticky="nw")
	
	to_entry <- tkframe(fromtoFrame)
	to_vars <- tclVar("1")
	to_field <- ttkentry(to_entry,width=3,textvariable=to_vars)
	tkgrid(labelRcmdr(to_entry,text=gettextRcmdr("To")),to_field,sticky="nw")
	
	tkgrid(from_entry,to_entry)
	tkgrid(fromtoFrame)
	
	
	sel_entry <- tkframe(whichEntry)
	sel_vars <- tclVar("c(1)")
	sel_field <- ttkentry(sel_entry,width=15,textvariable=sel_vars)
	tkgrid(labelRcmdr(sel_entry,text=gettextRcmdr("")),sel_field,sticky="nw")
	
	tkgrid(sel_entry,stick="nw")
	
	tkgrid(whichRadioFrame,whichEntry ,stick="nw")
	
	# Dimension Frame
	tkgrid(labelRcmdr(dimFrame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Dimension to Extract?")),sticky="nw")
	radioButtons(dimFrame,name="dimRadio",buttons=c("col","row","both"),values=c("col","row","both"),labels=gettextRcmdr(c("Columns","Rows","Columns & Rows")),initialValue="both",title="")
	dim_vars <- dimRadioVariable
	tkgrid(dimRadioFrame)
	
	
	tkgrid(whichFrame,dimFrame,sticky="nw")
	tkgrid.configure(dimFrame,padx="26")
	
	### EXTRACT FRAME 2 ###
	
	checkBoxes(extract2Frame,frame="saveCheck",boxes=paste("save"),initialValues=0,labels=gettextRcmdr("Save as RData?"))
	save_vars <- saveVariable
	
	savename_entry <- tkframe(extract2Frame)
	savename_vars <- tclVar("Extract")
	savename_field <- ttkentry(savename_entry,width=10,textvariable=savename_vars)
	tkgrid(labelRcmdr(savename_entry,text=gettextRcmdr("Extract Name: ")),savename_field,sticky="nw")

	extractButton <- buttonRcmdr(extract2Frame,command=onExtract,text=gettextRcmdr("Extract"),foreground="darkgreen",default="active",width="12",borderwidth=3)
	
	tkgrid(saveCheck,sticky="nw")
	tkgrid(savename_entry, extractButton  ,sticky="nw")
	tkgrid.configure(extractButton,padx="24")
	
	
	### BUTTONS FRAME ###

	setwdButton <- buttonRcmdr(buttonFrame,command=onSetwd,text=gettextRcmdr("Set Work Dir."),foreground="darkgreen",default="active",width="12",borderwidth=3)
	exitButton <- buttonRcmdr(buttonFrame,command=onCancel,text=gettextRcmdr("Exit"),foreground="darkgreen",width="8",borderwidth=3)
	tkgrid(setwdButton,exitButton,sticky="sew")
	
	### FINAL FRAME GRIDS ###
	tkgrid(resultFrame,sticky="nw")
	tkgrid(fabiaoptionsFrame,sticky="nw",pady="6")
	tkgrid(extract1Frame,sticky="nw")
	tkgrid(extract2Frame,sticky="nw",pady="18")
	tkgrid(buttonFrame,sticky="sew")
	
	tkgrid.columnconfigure(buttonFrame, 0, weight=1)
	tkgrid.columnconfigure(buttonFrame, 1, weight=1)
	tkgrid.configure(setwdButton,sticky="w")
	tkgrid.configure(exitButton,sticky="e")
	
	
	dialogSuffix(onOK=onOK,preventGrabFocus=TRUE)
	
	
	
	
}






