# Project: BiclustGUI
# 
# Author: Ewoud
###############################################################################


exportTXT_WINDOW <- function(){
	
	initializeDialog(title = gettextRcmdr("Write Output to text file...")) 
	
	### PREPARATION & BUTTON-FUNCTIONS ###
	AllResults <- .makeResultList()
	
	onSetwd <- function(){	Setwd()	}
	
	onCancel <- function() {
		if (GrabFocus()) 
			tkgrab.release(top)
		tkdestroy(top)
		tkfocus(CommanderWindow())
	}
	
	onHelp <- function() {
		tkgrab.release(window)
		print(help("writeBic"))
	}
	
		
	
	### GENERAL FRAMES ###
	
	resultFrame <- tkframe(top)
	exportoptions <- tkframe(top)
	buttons <- tkframe(top)
	
	### BICLUSTER RESULTS FRAME ##

		
	resultBox <- tklistbox( resultFrame , height=5, exportselection="FALSE",
			selectmode="single", background="white")
	for (result in AllResults) tkinsert(resultBox, "end", result)
	resultScroll <- ttkscrollbar(resultFrame,command=function(...) tkyview(resultBox, ...))
	tkconfigure(resultBox, yscrollcommand=function(...) tkset(resultScroll, ...))
	if(length(AllResults)!=0){tkselection.set(resultBox,0)}
	
	tkgrid(labelRcmdr(resultFrame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Biclustering Results:")),sticky="nw")
	tkgrid(resultBox,resultScroll) #,sticky="ns"
	tkgrid.configure(resultScroll,sticky="ns")
	
	
	### EXPORT ENTRY FRAMES ### 
	exportentry <- tkframe(exportoptions)
	
#	filename_entry <- tkframe(exportentry)
#	filename_vars <- tclVar("output")
#	filename_field <- ttkentry(filename_entry,width=15,textvariable=filename_vars)
#	tkgrid(labelRcmdr(filename_entry,text=gettextRcmdr("Filename: ")),filename_field,sticky="nw")
#	tkgrid(filename_entry,sticky="ne")
	
	title_entry <- tkframe(exportentry)
	title_vars <- tclVar("Output Result")
	title_field <- ttkentry(title_entry,width=15,textvariable=title_vars)
	tkgrid(labelRcmdr(title_entry,text=gettextRcmdr("Title: ")),title_field,sticky="nw")
	tkgrid(title_entry,sticky="ne")
	
	
	delim_entry <- tkframe(exportentry)
	delim_vars <- tclVar("")
	delim_field <- ttkentry(delim_entry,width=15,textvariable=delim_vars)
	tkgrid(labelRcmdr(delim_entry,text=gettextRcmdr("Delimiter (def=' '): ")),delim_field,sticky="nw")
	tkgrid(delim_entry,sticky="ne")
	
		
	### APPEND FRAME ###
	appendoption <- tkframe(exportoptions)
	
	checkBoxes(appendoption,frame="appendFrame",boxes="append",initialValues=1,labels="Append?")
	append_vars <- appendVariable
	tkgrid(appendFrame,padx="15")
	
	### FABIA OPTIONS FRAME ###
	
	fabiaoptionsFrame <- tkframe(top)
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
	
	
	### THE EXPORT BUTTON FUNCTION ###
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
		
			mname <- "biclust"
			# Special Case: fabia (because of thresholds)
			if(class(sel.result)=="Factorization"){
			
				bicResult <- .tobiclust_transf(sel.result.name,thresZ=paste0(tclvalue(thresZ_vars)),thresL=paste0(tclvalue(thresL_vars)))
			}
			# General Case
			else{
			
				bicResult <- .tobiclust_transf(sel.result.name)
			}
		
			delimiter <- tclvalue(delim_vars)
			if(delimiter==""){delimiter.paste <- delimiter}
			if(delimiter!=""){delimiter.paste <- paste0(",delimiter='",delimiter,"'")}
		
			#bicResult <- sel.result.name
			#fileName <- tclvalue(filename_vars)
			bicname <- tclvalue(title_vars)
			append <- as.character(tclvalue(append_vars))
			
					
			# Correct data check
			eval(parse(text=paste0("temp.correct <- .correctdataforresult(",sel.result.name,")")))
			if(temp.correct){
				
				# Old one with filename-input
				#export.command <- paste0("writeBic.GUI(dset=as.matrix(",ActiveDataSet(),"),fileName='",fileName,"',bicResult=",bicResult,",bicname='",bicname,"',mname='",mname,"',append=",append,delimiter.paste,")")
				export.command <- paste0("writeBic.GUI(dset=as.matrix(",ActiveDataSet(),"),bicResult=",bicResult,",bicname='",bicname,"',mname='",mname,"',append=",append,delimiter.paste,")")
				
				#print(export.command)
				doItAndPrint(export.command)
			}
		}
	}
	
	
	### WORKING DIR & EXPORT BUTTON ###

	buttonsleft <- tkframe(buttons)
	#setwdButton <- buttonRcmdr(buttonsleft,command=onSetwd,text=gettextRcmdr("Set Work Dir."),foreground="darkgreen",default="active",width="12",borderwidth=3)
	exportButton <- buttonRcmdr(buttonsleft,command=onOK,text=gettextRcmdr("Export"),foreground="darkgreen",default="active",width="10",borderwidth=3)
	#tkgrid(setwdButton,exportButton)
	tkgrid(exportButton)
	
	
	
	### EXIT & HELP BUTTON ###
	buttonsright <- tkframe(buttons)
	exitButton <- buttonRcmdr(buttonsright,command=onCancel,text=gettextRcmdr("Exit"),foreground="darkgreen",width="8",borderwidth=3)
	helpButton <- buttonRcmdr(buttonsright,command=onHelp,text=gettextRcmdr("Help"),foreground="darkgreen",width="8",borderwidth=3)
	tkgrid(exitButton,helpButton)
	

	### FINAL FRAME CONFIGURATION ###
	tkgrid(resultFrame,sticky="nw")
	tkgrid(labelRcmdr(exportoptions,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Export Options")),sticky="nw")
	
	
	tkgrid(exportentry,appendoption,sticky="nw")
	
	
	tkgrid(exportoptions,pady="10")
	
	tkgrid(fabiaoptionsFrame,pady="10",sticky="nw")
	
	tkgrid(buttonsleft,buttonsright)
	tkgrid(buttons,sticky="sew",pady=8)
	tkgrid.columnconfigure(buttons, 0, weight=1)
	tkgrid.columnconfigure(buttons, 1, weight=1)
	tkgrid.configure(buttonsleft,sticky="w")
	tkgrid.configure(buttonsright,sticky="e")
	
	dialogSuffix(onOK=onOK,preventGrabFocus=TRUE)
	
}

#exportTXT_WINDOW()
