# Project: BiclustGUI
# 
# Author: lucp8394
###############################################################################


# Rows & Cols -> IF EMPTY, PUT IT TO c() !!!!! OR SIMPLY DON'T FILL IN, USE DEFAULT OF FUNCTION ITSELF

findbiclusters_WINDOW <- function(){
	
	initializeDialog(title = gettextRcmdr("Find Biclusters...")) 
	
	### PREPARATION & BUTTON-FUNCTIONS ###
	AllResults <- .makeResultList()
	
		
	onCancel <- function() {
		if (GrabFocus()) 
			tkgrab.release(top)
		tkdestroy(top)
		tkfocus(CommanderWindow())
	}
	
	# GENERAL FRAMES
	resultFrame <- tkframe(top)
	fabiaoptionsFrame <- tkframe(top)
	rowscolsFrame <- tkframe(top)
	findFrame <- tkframe(top)
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
			
			
			# First check for correct data on all selected results
			data.check <- sapply(sel.result.name,FUN=function(x){
						eval(parse(text=paste0("temp.correct <- .correctdataforresult(",x,")")))
						return(temp.correct)
					})
			
			if(all(data.check)){
				# Making the result argument
				result.arg <- "list("
				
				for(i.result in 1:length(sel.result.name)){
					eval(parse(text=paste0("sel.result <-",sel.result.name[i.result])))
				
					# Special Case: fabia (because of thresholds)
					if(class(sel.result)=="Factorization"){
						temp.result <- .tobiclust_transf(sel.result.name[i.result],thresZ=paste0(tclvalue(thresZ_vars)),thresL=paste0(tclvalue(thresL_vars)))
					}
					
					# General Case
					else{
						temp.result <- .tobiclust_transf(sel.result.name[i.result])
					}
					if(i.result != 1){result.arg <- paste0(result.arg,",")}
					result.arg <- paste0(result.arg,sel.result.name[i.result],"=",temp.result)
					
				}
				result.arg <- paste0(result.arg,")")
				
				# Preparing the other arguments
				
				rows <- tclvalue(datarows_vars)
				cols <- tclvalue(datacols_vars)
				if(rows==""){rows <- "c()"}
				if(cols==""){cols <- "c()"}
				data <- ActiveDataSet()
				savename <- tclvalue(name_vars)
				
				temp.command <- paste0(savename," <- FindBiclusters(results=",result.arg,",rows=",rows,",cols=",cols,",data=as.matrix(",data,"))")
				doItAndPrint(temp.command)
				if(tclvalue(save_vars)=="1"){doItAndPrint(savename)}
			}
	
		}
	}
	

	
	
	### BICLUSTER RESULTS FRAME ##
	
	
	resultBox <- tklistbox( resultFrame , height=5, exportselection="FALSE",
			selectmode="multiple", background="white")
	for (result in AllResults) tkinsert(resultBox, "end", result)
	resultScroll <- ttkscrollbar(resultFrame,command=function(...) tkyview(resultBox, ...))
	tkconfigure(resultBox, yscrollcommand=function(...) tkset(resultScroll, ...))
	if(length(AllResults)!=0){
		for(i.tksel in 0:(length(AllResults)-1)){
			tkselection.set(resultBox,i.tksel)
		}
	}
	
	
	tkgrid(labelRcmdr(top,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Biclustering Results (Select 1 or more):")),sticky="nw")
	
		
	tkgrid(resultBox,resultScroll,sticky="nws") 
	tkgrid.configure(resultScroll,sticky="nws")

	
	
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
	
	
	
	### ROWS & COLUMNS ###
	tkgrid(labelRcmdr(rowscolsFrame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Data Rows & Columns")),sticky="nw")
	
	datarows_entry <- tkframe(rowscolsFrame)
	datarows_vars <- tclVar("c()")
	datarows_field <- ttkentry(datarows_entry,width=30,textvariable=datarows_vars)
	tkgrid(labelRcmdr(datarows_entry,text=gettextRcmdr("Rows Vector: ")),datarows_field,sticky="nw")
	tkgrid(datarows_entry,sticky="ne")
	
	datacols_entry <- tkframe(rowscolsFrame)
	datacols_vars <- tclVar("c()")
	datacols_field <- ttkentry(datacols_entry,width=30,textvariable=datacols_vars)
	tkgrid(labelRcmdr(datacols_entry,text=gettextRcmdr("Columns Vector: ")),datacols_field,sticky="nw")
	tkgrid(datacols_entry,sticky="ne")
	
	tkgrid(labelRcmdr(rowscolsFrame,text=gettextRcmdr("(Numeric/Character Vectors or Name of vector object)")),thresZ_field,sticky="nw")
	
	### FIND FRAME ###
	
	name_entry <- tkframe(findFrame)
	name_vars <- tclVar("FindBC")
	name_field <- ttkentry(name_entry,width=10,textvariable=name_vars)
	tkgrid(labelRcmdr(name_entry,text=gettextRcmdr("Save Name: ")),name_field,sticky="nw")
	
	
	findButton <- buttonRcmdr(findFrame,command=onOK,text=gettextRcmdr("Find Biclusters"),foreground="darkgreen",width="16",borderwidth=3,default="active")
	
	checkBoxes(findFrame,frame="saveShow",boxes=paste("save"),initialValues=0,labels=gettextRcmdr("Show Object?"))
	save_vars <- saveVariable
	tkgrid(saveShow,sticky="nw")
	
	tkgrid(name_entry,findButton,sticky="nw")
	tkgrid.configure(findButton,padx="35")
	
	
	
	
	
	### BUTTONS FRAME ###
	
	exitButton <- buttonRcmdr(buttonFrame,command=onCancel,text=gettextRcmdr("Exit"),foreground="darkgreen",width="8",borderwidth=3)
	tkgrid(exitButton,sticky="se")
	
	### FINAL FRAME GRIDS ###
	tkgrid(resultFrame,sticky="nw")
	tkgrid(fabiaoptionsFrame,sticky="nw",pady="6")
	tkgrid(rowscolsFrame,sticky="nw")
	tkgrid(findFrame,sticky="nw",pady="20")
	tkgrid(buttonFrame,sticky="se")
	

	
	
	dialogSuffix(onOK=onOK,preventGrabFocus=TRUE)
	
	
	
	
	
}