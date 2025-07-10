# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


isaextraplots_WINDOW <- function(methodname){  
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	
	method_result <- gsub(" ","",methodname,fixed=TRUE)
	method_result <- gsub("-","",method_result,fixed=TRUE)
	###############################################################################################################################################################################
	## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
	#####################################################
	
	
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
	button.biclust <-  "" 
	save <- FALSE
	arg.frames <- c() 
	button.otherarg <- paste(",Bicres=.isa2biclust(",method_result,")",sep="")
	
	# Do not change this line: 
	new.frames <- .add.frame(button.otherarg=button.otherarg,input=input,save=save,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
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
	
	
	####	    	MANUAL BUTTONS FRAME			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	
	frame.name <- "bubbleplotbuttonframe"  
	button.name <- "Draw Plot"  
	button.function <- "bubbleplot" 
	button.data <- "x" 
	button.biclust <-  "" 
	arg.frames <- c("bubbleplotprojframe","bubbleplotlabelframe") 
	button.otherarg <- paste(",bicResult1=.isa2biclust(",method_result,")",sep="")
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(button.otherarg=button.otherarg,input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
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
	button.biclust <-  "" 
	save <- FALSE
	arg.frames <- c("plotclustcheckframe","plotclustentryframe") 
	button.otherarg <- paste(",res=.isa2biclust(",method_result,")",sep="")
	
	# Do not change this line: 
	new.frames <- .add.frame(button.otherarg=button.otherarg,input=input,save=save,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
	
	###							###
	##       Heatmap Plot        ##
	###							###
	
	####		CHECK BOXES FRAME 			  ####
	#                               			 #
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "heatplotcheckframe"
	argument.names <- c("Local") 
	arguments <- c("local") 
	initial.values <- c(1) 
	title <- ""
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
	
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "heatplotentryframe"  
	argument.names <- c("Bicluster Number") 
	argument.types <- c("num")
	arguments <- c("number")
	initial.values <- c(1)
	title <- ""
	border <- FALSE
	entry.width <- c("2")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	####	    	MANUAL BUTTONS FRAME	  ####
	#                               			 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "heatbuttonframe"  
	button.name <- "Draw Plot"  
	button.function <- "drawHeatmap" 
	button.data <- "x" 
	button.biclust <-  "" 
	save <- FALSE
	arg.frames <- c("heatplotcheckframe","heatplotentryframe") 
	button.otherarg <- paste(",bicResult=.isa2biclust(",method_result,")",sep="")
	
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(button.otherarg=button.otherarg,input=input,frame.name=frame.name,type=type,save=save,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	###############################################################
	
	
	###							###
	##     Biclust Member Plot   ##
	###							###
	
	####		CHECK BOXES FRAME 			  ####
	#                               			 #
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "mplotcheckframe"
	argument.names <- c("Mid") 
	arguments <- c("mid") 
	initial.values <- c(1) 
	title <- ""
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "mplotentryframe"  
	argument.names <- c("Bicluster Label") 
	argument.types <- c("char")
	arguments <- c("cl_label")
	initial.values <- c("")
	title <- ""
	border <- FALSE
	entry.width <- c("8")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME - EXAMPLE 				  ####
	#                               								 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "memberbuttonframe"  
	button.name <- "Draw Plot"  
	button.function <- "biclustmember" 
	button.data <- "x" 
	button.biclust <-  "" 
	save <- FALSE
	arg.frames <- c("mplotcheckframe","mplotentryframe") 
	button.otherarg <- paste(",bicResult=.isa2biclust(",method_result,")",sep="")
	
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(button.otherarg=button.otherarg,input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	###############################################################
	
	
	###############################################################
	
	### CONFIGURING GRID ###
	
	grid.config <- .grid.matrix(input=input,c("heatplotcheckframe","heatplotentryframe","heatbuttonframe","mplotcheckframe","mplotentryframe","memberbuttonframe","bubbleplotprojframe","bubbleplotlabelframe","bubbleplotbuttonframe","plotclustentryframe","plotclustcheckframe","plotclustbuttonframe"   ,"barchartbuttonframe",NA,NA)    ,byrow=TRUE,nrow=5,ncol=3,grid.config=grid.config)
	
	### COMBINING ROWS ###
	
	grid.rows <- .combine.rows(input=input,rows=c(1),title="Heatmap Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(2),title="Biclustmember Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	grid.rows <- .combine.rows(input=input,rows=c(3),title="Biclust Bubble Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(4),title="Barplot of Bicluster",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(5),title="Bar Chart Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	##################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL NEW TOOL FUNCTION ##
	##################################################################
	
	newtool_template(toolname=toolname,methodname=methodname,toolhelp=toolhelp,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames)
	
	
}
