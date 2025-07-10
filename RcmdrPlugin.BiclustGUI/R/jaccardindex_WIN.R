# Project: BiclustGUI
# 
# Author: lucp8394
###############################################################################


jaccard_WINDOW <- function(){
	
	initializeDialog(title = gettextRcmdr("Compute Jaccard Index of 2 biclustering results...")) 
	
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
		print(help("jaccardind"))
	}
	
	
	
	### GENERAL FRAMES ###
	resultfabiaFrame <- tkframe(top)
	resultFrame <- tkframe(resultfabiaFrame)
	buttons <- tkframe(top)
	
	### BICLUSTER RESULTS FRAME ##
	
	
	resultBox <- tklistbox( resultFrame , height=5, exportselection="FALSE",
			selectmode="multiple", background="white")
	for (result in AllResults) tkinsert(resultBox, "end", result)
	resultScroll <- ttkscrollbar(resultFrame,command=function(...) tkyview(resultBox, ...))
	tkconfigure(resultBox, yscrollcommand=function(...) tkset(resultScroll, ...))
	if(length(AllResults)!=0){tkselection.set(resultBox,0)}
	
	tkgrid(labelRcmdr(top,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Biclustering Results (select 2):")),sticky="nw")
	tkgrid(resultBox,resultScroll,sticky="nw") #,sticky="ns"
	tkgrid.configure(resultScroll,sticky="ns")
	
	
	
	### FABIA OPTIONS FRAME ###
	
	fabiaoptionsFrame <- tkframe(resultfabiaFrame)
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
	
	
	### THE JACCARD BUTTON FUNCTION ###
	onOK <- function(){
		sel <- as.integer(tkcurselection(resultBox))+1
		if(length(AllResults)==0){
			justDoIt(paste0("warning('No available results',call.=FALSE)"))
		}
		else if(length(sel)==0){
			justDoIt(paste0("warning('No result selected',call.=FALSE)"))
		}
		else if(length(sel)==1){
			justDoIt(paste0("warning('Please select 2 results',call.=FALSE)"))
		}
		else if(length(sel)>2){
			justDoIt(paste0("warning('Please select no more than 2 results',call.=FALSE)"))
		}
		else{
			
			sel.result1 <- AllResults[sel[1]]
			sel.result2 <- AllResults[sel[2]]
						
			bicres1 <- .tobiclust_transf(sel.result1,thresZ=paste0(tclvalue(thresZ_vars)),thresL=paste0(tclvalue(thresL_vars)))
			bicres2 <- .tobiclust_transf(sel.result2,thresZ=paste0(tclvalue(thresZ_vars)),thresL=paste0(tclvalue(thresL_vars)))
								
			
			# Correct data check
			eval(parse(text=paste0("temp.correct1 <- .correctdataforresult(",sel.result1,")")))
			eval(parse(text=paste0("temp.correct2 <- .correctdataforresult(",sel.result2,")")))
			
			
			if(temp.correct1 & temp.correct2){
				
				jaccard.command <- paste0("jaccardind(bicres1=",bicres1,",bicres2=",bicres2,")")
				doItAndPrint(jaccard.command)
			}
		}
	}
	
	
	### WORKING DIR & EXPORT BUTTON ###
	
	buttonsleft <- tkframe(buttons)
	#setwdButton <- buttonRcmdr(buttonsleft,command=onSetwd,text=gettextRcmdr("Set Work Dir."),foreground="darkgreen",default="active",width="12",borderwidth=3)
	jaccardButton <- buttonRcmdr(buttonsleft,command=onOK,text=gettextRcmdr("Jaccard Index"),foreground="darkgreen",default="active",width="14",borderwidth=3)
	#tkgrid(setwdButton,exportButton)
	tkgrid(jaccardButton)
	
	
	
	### EXIT & HELP BUTTON ###
	buttonsright <- tkframe(buttons)
	exitButton <- buttonRcmdr(buttonsright,command=onCancel,text=gettextRcmdr("Exit"),foreground="darkgreen",width="8",borderwidth=3)
	helpButton <- buttonRcmdr(buttonsright,command=onHelp,text=gettextRcmdr("Help"),foreground="darkgreen",width="8",borderwidth=3)
	tkgrid(exitButton,helpButton)
	
	
	### FINAL FRAME CONFIGURATION ###

	tkgrid(resultFrame,fabiaoptionsFrame,sticky="nw")
	tkgrid(resultfabiaFrame,sticky="nw")
	tkgrid.configure(fabiaoptionsFrame,sticky="se",padx="25")
	
	tkgrid(buttonsleft,buttonsright)
	tkgrid(buttons,sticky="sew",pady=8)
	tkgrid.columnconfigure(buttons, 0, weight=1)
	tkgrid.columnconfigure(buttons, 1, weight=1)
	tkgrid.configure(buttonsleft,sticky="w")
	tkgrid.configure(buttonsright,sticky="e")
	
	dialogSuffix(onOK=onOK,preventGrabFocus=TRUE)
	
}

