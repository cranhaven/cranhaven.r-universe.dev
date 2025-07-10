# Project: BiclustGUI
# 
# Author: Gebruiker
###############################################################################


# 2 tabs
# use special save and open widgets

# check correct data?
# load, give warning it will overwrite previous plaid result + update biclustering object
saveload_WINDOW <- function(){
	
	initializeDialog(title = gettextRcmdr("Save & Load Biclustering Results"),use.tabs=TRUE,tabs=c("tab1","tab2")) 
	
#	AllResults <- .makeResultList()
	AllResults <- .makeSuperbiclustResultList()
	
	#AllResults <- c("test1","test2")
	
	onOK <- function(){}
	
	onLoadResult <- function(){
		# Do checks of correct data. Load isolated (USE LAZYLOAD). Then check. Then load in global. Then overwrite.
		# Load in result in isolated environment + remove afterwards
		
		#http://stackoverflow.com/questions/8700619/get-specific-object-from-rdata-file
#		e = local({load("C:/Users/lucp8394/Desktop/outputtest/something.RData"); environment()})
#		tools:::makeLazyLoadDB(e, "New")
#		lazyLoad("New")
		# Also in INFO not available -> save not compatible. NO BETTER IDEA. Add extra input "If save was not created in GUI, so only contains result" Listbox with Possible Methods + Entryfield to enter dataname (which should be loaded in Rcmdr)
		
		
		# Open the data with Lazy Load in the environment of this function (not yet globally)
		fileNameLoc <- tclvalue(tkgetOpenFile(filetypes="{{RData Files} {.RData .rda}} {{All files} *}")) 
#		tempLoc <- paste0(dirname(fileNameLoc),"/tempload")
		
#		e = local({load(fileNameLoc); environment()})
#		tools:::makeLazyLoadDB(e, tempLoc)
#		lazyLoad(tempLoc)
		load(fileNameLoc)
		
		filename <- basename(fileNameLoc)
		filename <- gsub(".RData","",filename)
		filename <- gsub(".rda","",filename)
		
		biclustering.objects <- .GetEnvBiclustGUI("biclustering.objects")
		if(is.null(biclustering.objects)){
			biclustering.objects <- list()
			
			biclustering.objects$all <- character()
			biclustering.objects$bcdiag <- c()
			biclustering.objects$superbiclust <- c()
			biclustering.objects$dataconnect <- data.frame(result=character(),data=character(),stringsAsFactors=FALSE)
			
#			assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
			.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
			
		}
		
		
#		get("biclustering.objects",envir=.GlobalEnv)
		biclustering.objects.original <- biclustering.objects # Be able to put the old one back if correctdata returns FALSE
		
		# The case if the INFO file is available (when loading saved object from GUI)
		if(paste0(filename,"INFO") %in% ls()){
			# Get the INFO
			eval(parse(text=paste0("temp.object <- ",filename,"INFO")))
			
			# Make exception for list of results
			if(temp.object$dataconnect[1,1]!="ListOfResults"){
				temp.type <- temp.object$all
			}else{
				temp.type <- "ListOfResults"
			}
			
			temp.data <- temp.object$dataconnect[,2]
			GO <- TRUE
		}	
		else{
			# The case were you need additional information from the dialog itself since INFO is not available.
			
			sel2 <- as.integer(tkcurselection(resultBox2))+1
			temp.data <- tclvalue(data_vars)
			GO <- FALSE
			if((length(sel2)!=0)&(temp.data!="")){	
				temp.type <- method_data$saveobject[sel2]
				GO <- TRUE
			}
			
		}	
			
		# Change temp.type if list of results to the actualy filename (since this must be saved in the dataconnect)
		if(temp.type=="ListOfResults"){
			temp.type <- filename
			ListOfResults <- TRUE
		}else{ListOfResults <- FALSE}
			
		if(GO==TRUE){
			# Put INFO into current biclustering.object
			
			# First check if there is already one of this method in the biclustering object, if so overwrite
#			if(temp.type %in% biclustering.objects$all){
			if(temp.type %in% as.character(biclustering.objects$dataconnect[,1])){
					
				# GIVE WARNING FOR OVERWRITING!!
				
				ReturnVal <- tkmessageBox(title="Loading Biclustering Results",message = paste0("This will overwrite the current ",temp.type," Result. Are you sure?"),icon = "warning", type = "yesno", default = "no")
				
				if(tclvalue(ReturnVal)=="yes"){
					
					if(!ListOfResults){
						which.typeall <- which(temp.type==biclustering.objects$all)
						which.typebc <- which(temp.type==biclustering.objects$bcdiag)
						which.typesuper <- which(temp.type==biclustering.objects$superbiclust)
						
						biclustering.objects$all[which.typeall] <- temp.type
						biclustering.objects$bcdiag[which.typebc] <- temp.type
						biclustering.objects$superbiclust[which.typesuper] <- temp.type
					}
										
					which.data <- which(biclustering.objects$dataconnect[,1]==temp.type)
									
					
					biclustering.objects$dataconnect <- biclustering.objects$dataconnect[-which.data,]
					biclustering.objects$dataconnect <- rbind(biclustering.objects$dataconnect,data.frame(result=temp.type,data=temp.data))
					
					# put in biclustering objects
			
#					assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
					.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
			
					
					# Check if correct is or can be active dataset
					eval(parse(text=paste0("temp.correct <- .correctdataforresult(",temp.type,")")))
			
					if(temp.correct==TRUE){
						# NOW DO THE LOAD in global environmnent + overwrite
						doItAndPrint(paste0("load('",fileNameLoc,"')"))
						if(!ListOfResults){doItAndPrint(paste0(temp.type," <- ",filename))}
#						unlink(paste0(tempLoc,".rdb"))
#						unlink(paste0(tempLoc,".rdx"))
					}
					else{
						ReturnVal2 <- tkmessageBox(title="Loading Biclustering Results",message ="Correct Active Dataset unavailable. See warning in Message Box",icon = "error", type = "ok")
#						assign("biclustering.objects",biclustering.objects.original,envir=.GlobalEnv) # Put the original biclustering.objects back
						.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects.original)
						biclustering.objects <- .GetEnvBiclustGUI("biclustering.objects")
						
#						get("biclustering.objects",envir=.GlobalEnv)
#						unlink(paste0(tempLoc,".rdb"))
#						unlink(paste0(tempLoc,".rdx"))
						
					}

				}
				
			}
			else{ # Case You don't need to overwrite
				
				# simply add it
				if(!ListOfResults){
					biclustering.objects$all <- c(biclustering.objects$all,temp.type)
					biclustering.objects$bcdiag <- c(biclustering.objects$bcdiag,temp.type)
					biclustering.objects$superbiclust <- c(biclustering.objects$superbiclust,temp.type)
				}
				
				biclustering.objects$dataconnect <- rbind(biclustering.objects$dataconnect,data.frame(result=temp.type,data=temp.data))
	
#				assign("biclustering.objects",biclustering.objects,envir=.GlobalEnv)
				.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects)
	
				
				# Check if correct is or can be active dataset
				eval(parse(text=paste0("temp.correct <- .correctdataforresult(",temp.type,")")))
				
				if(temp.correct==TRUE){
					# NOW DO THE LOAD in global environmnent + overwrite
					doItAndPrint(paste0("load('",fileNameLoc,"')"))
					if(!ListOfResults){doItAndPrint(paste0(temp.type," <- ",filename))}
#					unlink(paste0(tempLoc,".rdb"))
#					unlink(paste0(tempLoc,".rdx"))
				}
				else{
					ReturnVal2 <- tkmessageBox(title="Loading Biclustering Results",message ="Correct Active Dataset unavailable. See warning in Message Box",icon = "error", type = "ok")
#					assign("biclustering.objects",biclustering.objects.original,envir=.GlobalEnv) # Put the original biclustering.objects back
#					get("biclustering.objects",envir=.GlobalEnv)
					.AssignEnvBiclustGUI("biclustering.objects",biclustering.objects.original)
					biclustering.objects <- .GetEnvBiclustGUI("biclustering.objects")
#					unlink(paste0(tempLoc,".rdb"))
#					unlink(paste0(tempLoc,".rdx"))
					
				}
				
			}
	
		}
		else{
			ReturnVal2 <- tkmessageBox(title="Loading Biclustering Results",message ="The Optional Load Information was not filled in correctly.",icon = "error", type = "ok")
#			unlink(paste0(tempLoc,".rdb"))
#			unlink(paste0(tempLoc,".rdx"))
		}
		
		# Remove the result from window... or is window environment deleted automatically? NOTE: Yes, at new the environment is deleted
		#rm(filename)
		#rm(paste0(filename,"INFO"))
		
	}
	# NOTE: ENABLE SHOW EXTENSIONS -> PUT IN VIGNETTE!!
	onSave <- function(){
		
		
		sel <- as.integer(tkcurselection(resultBox))+1
		if(length(AllResults)==0){
			justDoIt(paste0("warning('No available results',call.=FALSE)"))
		}
		else if(length(sel)==0){
			justDoIt(paste0("warning('No result selected',call.=FALSE)"))
		}
		else{
			SelResult <- AllResults[sel]
			init.name <- paste0(SelResult,".RData")

			
			filenameLoc <- tclvalue(tkgetSaveFile(initialfile=init.name,filetypes="{{RData Files} {.RData .rda}} {{All files} *}"))
			#doItAndPrint(print(filenameLoc))
			
			filename <- basename(filenameLoc)
			filename <- gsub(".RData","",filename)
			filename <- gsub(".rda","",filename)
			
			biclustering.objects <- .GetEnvBiclustGUI("biclustering.objects")
			
			index.dataconnect <- which(biclustering.objects$dataconnect$result==SelResult)
			temp.info <- list()
			temp.info$dataconnect <- biclustering.objects$dataconnect[index.dataconnect,]
			
#			temp.info$all <- SelResult
			# Make exception for list of results (no All slot + change resultname to filename)
			if(!.isListofBiclustGUIresults(SelResult)){
				temp.info$all <- SelResult
			}else{
				dataconnect.temp <- data.frame(result="ListOfResults",data=temp.info$dataconnect[,2])
				temp.info$dataconnect <- dataconnect.temp				
			}
			
			save.command <- paste0("save(list=c('",filename,"','",filename,"INFO'),file='",filenameLoc,"')")
			
			#assign(paste0(filename,"INFO"),temp.info,envir=.GlobalEnv) # change to doItandPrint where temp.info is put in filenameINFO
			
			temp.info.command <- paste0(filename,"INFO <- list(dataconnect=data.frame(result=\"",as.character(temp.info$dataconnect$result),"\",data=\"",as.character(temp.info$dataconnect$data),"\"),all=\"",temp.info$all,"\")")
			
			
			doItAndPrint(temp.info.command)
			doItAndPrint(paste0(filename," <- ",SelResult))
			doItAndPrint(save.command)
			
			onCancel()
			

		}

	}
	
	onCancel <- function() {
		if (GrabFocus()) 
			tkgrab.release(top)
		tkdestroy(top)
		tkfocus(CommanderWindow())
	}
	
	#############
	### TAB 1 ###
	#############

	tab1Frame <- tkframe(tab1)
	
	resultFrame <- tkframe(tab1Frame)
	
	resultBox <- tklistbox( resultFrame , height=5, exportselection="FALSE",
			selectmode="single", background="white")
	for (result in AllResults) tkinsert(resultBox, "end", result)
	resultScroll <- ttkscrollbar(resultFrame,command=function(...) tkyview(resultBox, ...))
	tkconfigure(resultBox, yscrollcommand=function(...) tkset(resultScroll, ...))
	if(length(AllResults)!=0){tkselection.set(resultBox,0)}
	
	tkgrid(labelRcmdr(resultFrame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Biclustering Results:")),sticky="nw")
	tkgrid(resultBox,resultScroll) #,sticky="ns"
	tkgrid.configure(resultScroll,sticky="ns")
	
	tkgrid(resultFrame,sticky="nw",padx="6",pady="6")
		
	saveButton <- buttonRcmdr(tab1Frame,command=onSave,text=gettextRcmdr("Save"),foreground="darkgreen",default="active",width="12",borderwidth=3)
	
	tkgrid(saveButton,sticky="se",padx="6",pady="10")
	
	tkgrid(tab1Frame)
	
	#############
	### TAB 2 ###
	#############
	
	tab2Frame <- tkframe(tab2)
	loadoptions <- tkframe(tab2Frame)
	
	loadButton <- buttonRcmdr(tab2Frame,command=onLoadResult,text=gettextRcmdr("Load Result"),foreground="darkgreen",default="active",width="12",borderwidth=3)
	tkgrid(loadButton,sticky="nw",padx="6",pady="15")
	
	.makesearchdata()
	method_data <- .GetEnvBiclustGUI("biclustGUI_biclusteringsearchdata")# Save the global variable in method_data
	
	tkgrid(labelRcmdr(tab2Frame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Optional Load Information (for non-GUI saves)")),sticky="nw",padx="6")
		
	resultBox2 <- tklistbox( loadoptions , height=5, exportselection="FALSE",
			selectmode="single", background="white")
	for (result in c(method_data[,1],"ListOfResults")) tkinsert(resultBox2, "end", result)
	resultScroll2 <- ttkscrollbar(loadoptions,command=function(...) tkyview(resultBox2, ...))
	tkconfigure(resultBox2, yscrollcommand=function(...) tkset(resultScroll2, ...))
	tkgrid(resultBox2,resultScroll2,sticky="nw",padx="5") #,sticky="ns"
	tkgrid.configure(resultScroll2,sticky="ns")
	tkgrid(loadoptions,sticky="nw",padx="6",pady="10")
	
	
	data_entry <- tkframe(tab2Frame)
	data_vars <- tclVar("")
	data_field <- ttkentry(data_entry,width=15,textvariable=data_vars)
	tkgrid(labelRcmdr(data_entry,text=gettextRcmdr("Dataset Name: ")),data_field,sticky="nw")
	tkgrid(data_entry,sticky="nw",padx="5",pady="10")
	
	
	tkgrid(tab2Frame,sticky="nw")

	
	####################
	### GRID BUTTONS ###
	####################
	
	buttonsFrame <- tkframe(top)
	exitButton <- buttonRcmdr(buttonsFrame,command=onCancel,text=gettextRcmdr("Exit"),foreground="darkgreen",width="8",borderwidth=3)
	tkgrid(exitButton,sticky="es")
	
	
	
	dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE,onOK=onOK,tabs=c("tab1","tab2"),tab.names=c("Save","Load"),preventGrabFocus=TRUE)
		
}
