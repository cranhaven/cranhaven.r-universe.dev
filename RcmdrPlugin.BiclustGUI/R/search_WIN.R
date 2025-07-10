# Project: search_test
# 
# Author: Ewoud
###############################################################################


# This function needs to be expanded as soon as more search criteria are getting added
SearchMethodData <- function(data,type,discovery){
	
	if(discovery=="All"){
		out <- data[data$type==type,1]
	}
	else{
		out <- data[(data$type==type & data$discovery==discovery),1]
	}
		
	return(out)
}




search_WINDOW <- function(){

	initializeDialog(title = gettextRcmdr("Search Methods... ")) # Change to Search Biclustering Methods...
	
	.makesearchdata()
	method_data <- .GetEnvBiclustGUI("biclustGUI_biclusteringsearchdata") # Save the global variable in method_data
	
	onOK <- function(){
		tkdelete(methodBox, "0", "end")
		type <- tclvalue(radiotypeVariable)
		discovery <- tclvalue(radiodiscoVariable)
		method.names <- SearchMethodData(method_data,type,discovery)
		for (name in method.names) tkinsert(methodBox, "end", name)
#		assign("global.variable.list",method.names,envir=.GlobalEnv)
		.AssignEnvBiclustGUI("global.variable.list",method.names)
		
		#global.variable.list<<- method.names
		
	}
	
	onWindow <- function(){
		global.variable.list <- .GetEnvBiclustGUI("global.variable.list")
		
		if(is.null(global.variable.list)){justDoIt("warning('Search for methods first',call.=FALSE)")}
		else{
			sel1 <- as.integer(tkcurselection(methodBox))+1
			sel2 <- which(global.variable.list[sel1]==method_data$name)
			# Code in comment to be used when implementing it in package itself
			tkdestroy(top)
			eval(parse(text=paste(method_data$window[sel2])))
			#print(method_data$window[sel2])
		}
	}
	
	searchFrame <- tkframe(top)
	searchinputFrame <- tkframe(searchFrame)
	
	
	## Type Frame
	typeFrame <- ttklabelframe(searchinputFrame,text=gettextRcmdr("Bicluster Type"))
	radioButtons(typeFrame,name="radiotype",buttons=c("Constant","CoherentValues","CoherentEvolution"),values=c("Constant","Coherent Values","Coherent Evolution"),labels=gettextRcmdr(c("Constant Values","Coherent Values","Coherent Evolution")),initialValue="Constant",title="")
	tkgrid(radiotypeFrame)
	
	## Discovery Frame
	discoFrame <- ttklabelframe(searchinputFrame,text=gettextRcmdr("Discovering Type"))
	radioButtons(discoFrame,name="radiodisco",buttons=c("Additive","Multiplicative","All"),values=c("Additive","Multiplicative","All"),labels=gettextRcmdr(c("Additive","Multiplicative","All")),initialValue="All",title="")
	tkgrid(radiodiscoFrame)
	
	## Search Button
	searchButton <- buttonRcmdr(searchinputFrame,command=onOK,text=gettextRcmdr("Search"),foreground="darkgreen",default="active",width="12",borderwidth=3)
	
	
	## Result Frame
	resultFrame <- ttklabelframe(searchFrame,text=gettextRcmdr("Results:"))
	
	methodBox <- tklistbox(resultFrame, height=5, exportselection="FALSE",
			selectmode="single", background="white")
	methodScroll <- ttkscrollbar(resultFrame,command=function(...) tkyview(methodBox, ...))
	tkconfigure(methodBox, yscrollcommand=function(...) tkset(methodScroll, ...))
		
	openwindowButton <- buttonRcmdr(resultFrame,command=onWindow,text=gettextRcmdr("Go to"),foreground="darkgreen",default="active",width="12",borderwidth=3)
	
	
	tkgrid(methodBox,methodScroll,openwindowButton) #,sticky="ns"
	tkgrid.configure(methodScroll,sticky="ns")
	
	tkgrid(typeFrame,discoFrame,searchButton,sticky="sw")
	tkgrid.configure(discoFrame,padx="7")
	tkgrid.configure(searchButton,padx="10")
	tkgrid(searchinputFrame,sticky="nw")
	tkgrid(resultFrame,sticky="w")
	tkgrid(searchFrame,sticky="nw")
	

	onCancel <- function(){}
	
	
	dialogSuffix(onOK=onOK,preventGrabFocus=TRUE)
	
	
}


#search_WINDOW()