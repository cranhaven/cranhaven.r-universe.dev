# Project: BiclustGUI
# 
# Author: Ewoud
###############################################################################


exportFURBY_WINDOW <- function(){
	initializeDialog(title = gettextRcmdr("Write Output for FURBY"),use.tabs=TRUE,tabs=c("tab1","tab2")) 
	
	###################
	### PREPARATION ###
	###################
	
	# Make Result Lists
	AllResults <- .makeResultList()
	sel.fabia <- which(sapply(AllResults,FUN=function(x){
						eval(parse(text=paste0("x <- ",x)))
						return(class(x))
					})=="Factorization")
	
	tab1.results <- AllResults[sel.fabia]
	if(length(sel.fabia)==0){
		tab2.results <- AllResults
	}
	else{
		tab2.results <- AllResults[-sel.fabia]
	}
	# Some button functions
	onOK <- function(){}
	onSetwd <- function(){	Setwd()	}
	
	onCancel <- function() {
		if (GrabFocus()) 
			tkgrab.release(top)
		tkdestroy(top)
		tkfocus(CommanderWindow())
	}
	
	onHelp <- function() {
		tkgrab.release(window)
		browseURL("http://caleydo.github.io/projects/furby/")
	}
	
	
	
	#############
	### TAB 1 ###
	#############
	#tab1Frame
	tab1Frame <- tkframe(tab1)
	# RESULT FRAME
	result1Frame <- tkframe(tab1Frame)
	result1Box <- tklistbox( result1Frame , height=5, exportselection="FALSE",
			selectmode="single", background="white")
	for (result in tab1.results) tkinsert(result1Box, "end", result)
	result1Scroll <- ttkscrollbar(result1Frame,command=function(...) tkyview(result1Box, ...))
	tkconfigure(result1Box, yscrollcommand=function(...) tkset(result1Scroll, ...))
	if(length(tab1.results)!=0){tkselection.set(result1Box,0)}
	
	tkgrid(labelRcmdr(result1Frame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("FABIA Results:")),sticky="nw")
	tkgrid(result1Box,result1Scroll) #,sticky="ns"
	tkgrid.configure(result1Scroll,sticky="ns")
	
	### TAB 1 - ENTRY FRAMES 1 ### 
	tab1entry1 <- tkframe(tab1Frame)
	
	baseName1_entry <- tkframe(tab1entry1)
	baseName1_vars <- tclVar("basename")
	baseName1_field <- ttkentry(baseName1_entry,width=15,textvariable=baseName1_vars)
	tkgrid(labelRcmdr(baseName1_entry,text=gettextRcmdr("Basename: ")),baseName1_field,sticky="nw")
	tkgrid(baseName1_entry,sticky="ne")
	
	
	clusass1_entry <- tkframe(tab1entry1)
	clusass1_vars <- tclVar("NULL")
	clusass1_field <- ttkentry(clusass1_entry,width=15,textvariable=clusass1_vars)
	tkgrid(labelRcmdr(clusass1_entry,text=gettextRcmdr("Cluster Assignments: ")),clusass1_field,sticky="nw")
	tkgrid(clusass1_entry,sticky="ne")
	
	
	clusnam1_entry <- tkframe(tab1entry1)
	clusnam1_vars <- tclVar("NULL")
	clusnam1_field <- ttkentry(clusnam1_entry,width=15,textvariable=clusnam1_vars)
	tkgrid(labelRcmdr(clusnam1_entry,text=gettextRcmdr("Cluster Names: ")),clusnam1_field,sticky="nw")
	tkgrid(clusnam1_entry,sticky="ne")
	
	tkgrid(result1Frame,tab1entry1,sticky="sw")
	
	
	### TAB 1 - ENTRY FRAMES 2 ###
	
	tab1entry2 <- tkframe(tab1Frame)
	tkgrid(labelRcmdr(tab1entry2,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Thresholds:")),sticky="nw")
	
	
	thresZ_entry <- tkframe(tab1entry2)
	thresZ_vars <- tclVar("0.5")
	thresZ_field <- ttkentry(thresZ_entry,width=6,textvariable=thresZ_vars)
	tkgrid(labelRcmdr(thresZ_entry,text=gettextRcmdr("Threshold Z (Samples): ")),thresZ_field,sticky="nw")
	tkgrid(thresZ_entry,sticky="ne")
	
	
	thresL_entry <- tkframe(tab1entry2)
	thresL_vars <- tclVar("NULL")
	thresL_field <- ttkentry(thresL_entry,width=6,textvariable=thresL_vars)
	tkgrid(labelRcmdr(thresL_entry,text=gettextRcmdr("Threshold L (Loadings): ")),thresL_field,sticky="nw")
	tkgrid(thresL_entry,sticky="ne")
	
	
	### Tab 1 - Button Functions ###

	button.export.fabia.thresholds <- function(){
		sel <- as.integer(tkcurselection(result1Box))+1
		if(length(tab1.results)==0){
			justDoIt(paste0("warning('No available results',call.=FALSE)"))
		}
		else if(length(sel)==0){
			justDoIt(paste0("warning('No result selected',call.=FALSE)"))
		}
		else{
			#sel <- as.integer(tkcurselection(result1Box))+1
			fabiaRes <- tab1.results[sel]
			thresZ <- tclvalue(thresZ_vars)
		
			export.command <- paste0("export.fabia.threshL(fabiaRes=",fabiaRes,", threshZ=",thresZ,")")
			doItAndPrint(export.command)
		}
	}
	button.export.fabia.results <- function(){
		sel <- as.integer(tkcurselection(result1Box))+1
		if(length(tab1.results)==0){
			justDoIt(paste0("warning('No available results',call.=FALSE)"))
		}
		else if(length(sel)==0){
			justDoIt(paste0("warning('No result selected',call.=FALSE)"))
		}
		else{
			#sel <- as.integer(tkcurselection(result1Box))+1
			fabiaRes <- tab1.results[sel]
			thresZ <- tclvalue(thresZ_vars)
			thresL <- tclvalue(thresL_vars)
			baseName <- tclvalue(baseName1_vars)
		
			clusterAssignments <- tclvalue(clusass1_vars)
			if(clusterAssignments!="NULL"){clusterAssignments <- paste0("'",clusterAssignments,"'")}
			clusterNames <- tclvalue(clusnam1_vars)
			if(clusterNames!="NULL"){clusterNames <- paste0("'",clusterNames,"'")}
		
			export.command <- paste0("export.fabia(fabiaRes=",fabiaRes,",baseName='",baseName,"',clusterAssignments=",clusterAssignments,",clusterNames=",clusterNames,",threshZ=",thresZ,",threshL=",thresL,")")
			doItAndPrint(export.command)
		}
	}
	
	### Tab 1 - Buttons ###
	
	tab1buttons <- tkframe(tab1Frame)
	
	fabthresButton <- buttonRcmdr(tab1buttons,command=button.export.fabia.thresholds,text=gettextRcmdr("Export Thresholds"),foreground="darkgreen",default="active",width="16",borderwidth=3)
	fabresButton <- buttonRcmdr(tab1buttons,command=button.export.fabia.results,text=gettextRcmdr("Export Results"),foreground="darkgreen",default="active",width="16",borderwidth=3)
	
	tkgrid(fabthresButton,pady="1")
	tkgrid(fabresButton,pady="1")
	
	tkgrid(tab1entry2,tab1buttons,pady="5")
	tkgrid.configure(tab1buttons,sticky="s",pady="15")
	
	#############
	### TAB 2 ###
	#############
	
	tab2Frame <- tkframe(tab2)
	
	# RESULT FRAME
	result2Frame <- tkframe(tab2Frame)
	result2Box <- tklistbox( result2Frame , height=5, exportselection="FALSE",
			selectmode="single", background="white")
	for (result in tab2.results) tkinsert(result2Box, "end", result)
	result2Scroll <- ttkscrollbar(result2Frame,command=function(...) tkyview(result2Box, ...))
	tkconfigure(result2Box, yscrollcommand=function(...) tkset(result2Scroll, ...))
	if(length(tab2.results)!=0){tkselection.set(result2Box,0)}
	
	tkgrid(labelRcmdr(result2Frame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Biclustering Results:")),sticky="nw")
	tkgrid(result2Box,result2Scroll) #,sticky="ns"
	tkgrid.configure(result2Scroll,sticky="ns")
	
	### TAB 2 - ENTRY FRAMES 1 ### 
	tab2entry1 <- tkframe(tab2Frame)
	
	baseName2_entry <- tkframe(tab2entry1)
	baseName2_vars <- tclVar("basename")
	baseName2_field <- ttkentry(baseName2_entry,width=15,textvariable=baseName2_vars)
	tkgrid(labelRcmdr(baseName2_entry,text=gettextRcmdr("Basename: ")),baseName2_field,sticky="nw")
	tkgrid(baseName2_entry,sticky="ne")
	
	
	clusass2_entry <- tkframe(tab2entry1)
	clusass2_vars <- tclVar("NULL")
	clusass2_field <- ttkentry(clusass2_entry,width=15,textvariable=clusass2_vars)
	tkgrid(labelRcmdr(clusass2_entry,text=gettextRcmdr("Cluster Assignments: ")),clusass2_field,sticky="nw")
	tkgrid(clusass2_entry,sticky="ne")
	
	
	clusnam2_entry <- tkframe(tab2entry1)
	clusnam2_vars <- tclVar("NULL")
	clusnam2_field <- ttkentry(clusnam2_entry,width=15,textvariable=clusnam2_vars)
	tkgrid(labelRcmdr(clusnam2_entry,text=gettextRcmdr("Cluster Names: ")),clusnam2_field,sticky="nw")
	tkgrid(clusnam2_entry,sticky="ne")
	
	tkgrid(result2Frame,tab2entry1,sticky="sw")
	tkgrid.configure(tab2entry1,padx="10",sticky="es")
	
	
	### Tab 2 - Button Functions ###
	button.export.biclust <- function(){
		sel <- as.integer(tkcurselection(result2Box))+1
		if(length(tab2.results)==0){
			justDoIt(paste0("warning('No available results',call.=FALSE)"))
		}
		else if(length(sel)==0){
			justDoIt(paste0("warning('No result selected',call.=FALSE)"))
		}
		else{
			#sel <- as.integer(tkcurselection(result2Box))+1
			baseName <- tclvalue(baseName2_vars)
		
			clusterAssignments <- tclvalue(clusass2_vars)
			if(clusterAssignments!="NULL"){clusterAssignments <- paste0("'",clusterAssignments,"'")}
			clusterNames <- tclvalue(clusnam2_vars)
			if(clusterNames!="NULL"){clusterNames <- paste0("'",clusterNames,"'")}
		
			biClust.name <- tab2.results[sel]
		

			# Choosing the correct transformation to biclust
			biClust <- .tobiclust_transf(biClust.name)
		
			# Correct data check
			eval(parse(text=paste0("temp.correct <- .correctdataforresult(",biClust.name,")")))	
			if(temp.correct){
				export.command <- paste0("export.Biclust(biClust=",biClust,",x=as.matrix(",ActiveDataSet(),"),baseName='",baseName,"',clusterAssignments=",clusterAssignments,",clusterNames=",clusterNames,")")
				doItAndPrint(export.command)
			}
		}
	}
	
	### Tab 2 - Buttons ###
	
	tab2buttons <- tkframe(tab2Frame)
	emptyFrame <- tkframe(tab2Frame)
	
	exportbiclustButton <- buttonRcmdr(tab2buttons,command=button.export.biclust,text=gettextRcmdr("Export Results"),foreground="darkgreen",default="active",width="16",borderwidth=3)
	
	tkgrid(exportbiclustButton,sticky="se",pady="20")
	tkgrid(emptyFrame,tab2buttons,sticky="se")
	
	
	####################
	### GRID BUTTONS ###
	####################
	tkgrid(tab1Frame,sticky="nw")
	tkgrid(tab2Frame,sticky="nw")
	
	buttonsFrame <- tkframe(top)
	buttonsleft <- tkframe(buttonsFrame)
	buttonsright <- tkframe(buttonsFrame)
	
	setwdButton <- buttonRcmdr(buttonsleft,command=onSetwd,text=gettextRcmdr("Set Work. Dir."),foreground="darkgreen",width="12",borderwidth=3)
	exitButton <- buttonRcmdr(buttonsright,command=onCancel,text=gettextRcmdr("Exit"),foreground="darkgreen",width="8",borderwidth=3)
	helpButton <- buttonRcmdr(buttonsright,command=onHelp,text=gettextRcmdr("Help"),foreground="darkgreen",width="8",borderwidth=3)
	tkgrid(setwdButton)
	tkgrid(exitButton,helpButton)
	
	tkgrid(buttonsleft,buttonsright,pady="8")
	tkgrid.columnconfigure(buttonsFrame, 0, weight=1)
	tkgrid.columnconfigure(buttonsFrame, 1, weight=1)
	tkgrid.configure(buttonsleft,sticky="w")
	tkgrid.configure(buttonsright,sticky="e")
	

	
	dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE,onOK=onOK,tabs=c("tab1","tab2"),tab.names=c("FABIA","Other"),preventGrabFocus=TRUE)
}

#exportFURBY_WINDOW()