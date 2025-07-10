# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


biclustextraplots_WINDOW <- function(methodname){  
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	
	
	###############################################################################################################################################################################
	## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
	#####################################################
	
	## Determining of the option for extra results should be given
	make.extra.results <- TRUE
	list.methods.noextra <- c("Rqubic")
	if(methodname %in% list.methods.noextra){make.extra.results <- FALSE}
	##
	
	
	toolname <- "Extra Plots from `biclust'"
	
	toolhelp <- "bubbleplot"
	
	
	
	# Do not change this line:
	input <- "plotdiagTab"
	
	#######################
	## MAKING THE WINDOW ##
	#######################
	
	### ADDING FRAMES ####
	
	# Idem as plotdiag tab.
	
	####				####
	## MAKE BARCHART PLOT ##
	####				####
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
#                               						 #
	
	type <- "buttons"
	
	# Change variables accordingly:
		
	frame.name <- "barchartbuttonframe"  
	button.name <- "Draw Plot"  
	button.function <- "biclustbarchart" 
	button.data <- "x" 
	button.biclust <-  "Bicres" 
	save <- FALSE
	arg.frames <- c() 
	
	# Do not change this line: 
	new.frames <- .add.frame(input=input,save=save,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	####				####
	##   MAKE BUBBLE PLOT ##
	####				####
	
	
	####		RADIO BUTTONS FRAME - EXAMPLE 				####
	#                               						   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "bubbleplotprojframe"
	argument.names <- c("Mean","Iso Mds","Cmd Scale")  
	arguments <- c("projection")		
	argument.values <- c("mean","isomds","cmdscale") 
	argument.types <- "char"
	initial.values <- "mean" 
	title <- "Projection:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	####		CHECK BOXES FRAME 			  ####
	#                               			 #
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "bubbleplotlabelframe"
	argument.names <- c("Show Labels?") 
	arguments <- c("showLabels") 
	initial.values <- c(0) 
	title <- ""
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "bubbleplotentryframe"  
	argument.names <- c("bicResult2","bicResult3") 
	argument.types <- c("num","num")
	arguments <- c("bicResult2","bicResult3")
	initial.values <- c("NULL","NULL")
	title <- "Extra Bicluster Results?"
	border <- FALSE
	entry.width <- c("8","8")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	
	frame.name <- "bubbleplotbuttonframe"  
	button.name <- "Draw Plot"  
	button.function <- "bubbleplot" 
	button.data <- "x" 
	button.biclust <-  "bicResult1" 
	arg.frames <- c("bubbleplotentryframe","bubbleplotprojframe","bubbleplotlabelframe") 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	####					  ####
	##  Barplot of bicluster	##
	####					  ####
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "plotclustentryframe"  
	argument.names <- c("Total Number of Biclusters") 
	argument.types <- c("num")
	arguments <- c("noC")
	initial.values <- c("6")
	title <- ""
	border <- FALSE
	entry.width <- c("4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	####		CHECK BOXES FRAME 			  ####
	#                               			 #
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "plotclustcheckframe"
	argument.names <- c("Legend?") 
	arguments <- c("legende") 
	initial.values <- c(0) 
	title <- ""
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
#                               						 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	
	frame.name <- "plotclustbuttonframe"  
	button.name <- "Draw Plot"  
	button.function <- "plotclust" 
	button.data <- "x" 
	button.biclust <-  "res" 
	save <- FALSE
	arg.frames <- c("plotclustcheckframe","plotclustentryframe") 
	
	# Do not change this line: 
	new.frames <- .add.frame(input=input,save=save,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
	###############################################################
	
	### CONFIGURING GRID ###
	if(make.extra.results==TRUE){
		grid.config <- .grid.matrix(input=input,c("bubbleplotprojframe","bubbleplotentryframe","bubbleplotbuttonframe","bubbleplotlabelframe",NA,NA,"plotclustentryframe","plotclustcheckframe","plotclustbuttonframe"   ,"barchartbuttonframe",NA,NA)    ,byrow=TRUE,nrow=4,ncol=3,grid.config=grid.config)
	}
	else{
		grid.config <- .grid.matrix(input=input,c("bubbleplotprojframe",NA,"bubbleplotbuttonframe","bubbleplotlabelframe",NA,NA,"plotclustentryframe","plotclustcheckframe","plotclustbuttonframe"   ,"barchartbuttonframe",NA,NA)    ,byrow=TRUE,nrow=4,ncol=3,grid.config=grid.config)
	}
	### COMBINING ROWS ###
	grid.rows <- .combine.rows(input=input,rows=c(1,2),title="Biclust Bubble Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(3),title="Barplot of Bicluster",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(4),title="Bar Chart Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	##################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL NEW TOOL FUNCTION ##
	##################################################################
	
	newtool_template(toolname=toolname,methodname=methodname,toolhelp=toolhelp,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames)
	
	
}
