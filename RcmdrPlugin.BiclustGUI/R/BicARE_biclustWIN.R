# Project: BiclustGUI
# 
# Author: Ewoud
###############################################################################

bicarebiclust_WINDOW <- function(methodname){  
	
	method_result <- gsub(" ","",methodname,fixed=TRUE)
	method_result <- gsub("-","",method_result,fixed=TRUE)
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	
	
	###############################################################################################################################################################################
	## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
	#####################################################
	
	
	toolname <- "Biclust Plots"
	
	toolhelp <- "biclust"
	
	
	
	#######################
	## MAKING THE WINDOW ##
	#######################
	
	####################################
	#### PLOTTING & DIAGNOSTICS TAB ####
	####################################
	
	input <- "plotdiagTab"
	
	###							###
	## Parallel Coordinates Plot ##
	###							###
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "pplottypeframe"
	argument.names <- c("Default","Combined (rows & columns)")  
	arguments <- c("type2")		
	argument.values <- c("default","combined") 
	argument.types <- "char"
	initial.values <- "default" 
	title <- "Plot Type:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	
	
	####		CHECK BOXES FRAME 			  ####
	#                               			 #
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "pplotcheckframe"
	argument.names <- c("Plot Only Column","Plot Rows & Columns","Compare") 
	arguments <- c("plotcol","plotBoth","compare") 
	initial.values <- c(1,0,1) 
	title <- "Default Type Options:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
	
	
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "pplotentryframe"  
	argument.names <- c("Bicluster Number") 
	argument.types <- c("num")
	arguments <- c("number")
	initial.values <- c(1)
	title <- ""
	border <- FALSE
	entry.width <- c("2")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "parallelbuttonframe"  
	button.name <- "Draw Plot"  
	button.function <- "parallelCoordinates3" 
	button.data <- "x" 
	button.biclust <-  "" 
	button.otherarg <- paste(",bicResult=.bicare2biclust(",method_result,")",sep="")
	save <- FALSE
	arg.frames <- c("pplotcheckframe","pplotentryframe","pplottypeframe") 
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	###############################################################
	
	
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
	button.otherarg <- paste(",bicResult=.bicare2biclust(",method_result,")",sep="")
	
	save <- FALSE
	arg.frames <- c("heatplotcheckframe","heatplotentryframe") 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,save=save,button.name=button.name,button.function=button.function,button.data=button.data,button.otherarg=button.otherarg,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
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
	button.otherarg <- paste(",bicResult=.bicare2biclust(",method_result,")",sep="")
	
	save <- FALSE
	arg.frames <- c("mplotcheckframe","mplotentryframe") 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.otherarg=button.otherarg,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	###############################################################
	
	
	###							###
	## Summary & Diagnostics Box ##
	###							###
	
	
	
	####	    	MANUAL BUTTONS FRAME		  ####
	#                               	 			 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "fstatbuttonframe"  
	button.name <- "Obs. F Stat."  
	button.function <- "computeObservedFstat" 
	button.data <- "x" 
	button.biclust <-  "" 
	button.otherarg <- paste(",bicResult=.bicare2biclust(",method_result,")",sep="")
	
	arg.frames <- c("fstatentryframe") 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.otherarg=button.otherarg,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	###############################################################
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "fstatentryframe"  
	argument.names <- c("Bicluster Number") 
	argument.types <- c("num")
	arguments <- c("number")
	initial.values <- c("1")
	title <- ""
	border <- FALSE
	entry.width <- c("4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "bootstrapentryframe"  
	argument.names <- c("Bicluster Number","Number Bootstrap Replicates") 
	argument.types <- c("num","num")
	arguments <- c("number","nResamplings")
	initial.values <- c(1,100)
	title <- "Bootstrap Options:"
	border <- FALSE
	entry.width <- c("4","4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	####		CHECK BOXES FRAME 			  ####
	#                               			 #
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "bootstrapreplacementframe"
	argument.names <- c("With Replacement?") 
	arguments <- c("replace") 
	initial.values <- c(1) 
	title <- ""
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
	
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "bootstrapbuttonframe"  
	button.name <- "Bootstrap"  
	button.function <- "diagnoseColRow" 
	button.data <- "x" 
	button.biclust <-  "" 
	button.otherarg <- paste(",bicResult=.bicare2biclust(",method_result,")",sep="")
	
	arg.frames <- c("bootstrapentryframe","bootstrapreplacementframe") 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.otherarg=button.otherarg,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
	####	    	MANUAL BUTTONS FRAME		  ####
	#                               				 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "bootstrapvisualbuttonframe"  
	button.name <- "Visualize"  
	button.function <- "diagnosticPlot" 
	button.data <- "" 
	button.biclust <-  "" 
	button.otherarg <- "bootstrapOutput=Bootstrap"
	save <- FALSE
	arg.frames <- c() 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames,button.otherarg=button.otherarg)
	
	####				####
	## MAKE BARCHART PLOT ##
	####				####
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
#                               						 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	
	frame.name <- "barchartbuttonframe"  
	button.name <- "Barchart"  
	button.function <- "biclustbarchart" 
	button.data <- "x" 
	button.biclust <-  "" 
	button.otherarg <- paste(",Bicres=.bicare2biclust(",method_result,")",sep="")
	
	save <- FALSE
	arg.frames <- c() 
	
	# Do not change this line: 
	new.frames <- .add.frame(input=input,save=save,frame.name=frame.name,type=type,button.otherarg=button.otherarg,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
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
	title <- "Bubble Plot"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
	
	
	####	    	MANUAL BUTTONS FRAME			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	
	frame.name <- "bubbleplotbuttonframe"  
	button.name <- "Bubble Plot"  
	button.function <- "bubbleplot" 
	button.data <- "x" 
	button.biclust <-  "" 
	button.otherarg <- paste(",bicResult1=.bicare2biclust(",method_result,")",sep="")
	
	arg.frames <- c("bubbleplotprojframe","bubbleplotlabelframe") 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.otherarg=button.otherarg,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
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
	title <- "Barplot"
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
	button.name <- "BarPlot"  
	button.function <- "plotclust" 
	button.data <- "x" 
	button.biclust <-  "" 
	button.otherarg <- paste(",res=.bicare2biclust(",method_result,")",sep="")
	
	save <- FALSE
	arg.frames <- c("plotclustcheckframe","plotclustentryframe") 
	
	# Do not change this line: 
	new.frames <- .add.frame(input=input,save=save,frame.name=frame.name,button.otherarg=button.otherarg,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	####		CHECK BOXES FRAME 			  ####
	#                               			 #
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "testentry"
	argument.names <- c("") 
	arguments <- c("") 
	initial.values <- c(0) 
	title <- "Title"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME 	  ####
	#             								 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "summarybuttonframe"  
	button.name <- "Summary"  
	button.function <- "summary" 
	button.data <- "" 
	button.biclust <-  "" 
	arg.frames <- c()
	save <- FALSE
	button.otherarg <- paste(",object=.bicare2biclust(",method_result,")",sep="")
	
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,button.otherarg=button.otherarg,save=save,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	###############################################################################################################################################################################
	## CONFIGURATION OF GRID OF FRAMES - PLOTDIAGTAB ##
	###################################################
	
	
	#########################
	#### THE GRID MATRIX ####
	#########################
	
	
	grid.config <- .grid.matrix(input=input,c("summarybuttonframe","fstatentryframe","fstatbuttonframe","barchartbuttonframe","bootstrapentryframe",NA,"bootstrapreplacementframe","bootstrapbuttonframe","bootstrapvisualbuttonframe" ,"pplottypeframe","pplotentryframe","parallelbuttonframe","pplotcheckframe",NA,NA,"heatplotcheckframe","heatplotentryframe","heatbuttonframe","mplotcheckframe","mplotentryframe","memberbuttonframe","plotclustentryframe","plotclustcheckframe","plotclustbuttonframe","bubbleplotlabelframe","bubbleplotprojframe","bubbleplotbuttonframe"),byrow=TRUE,nrow=9,ncol=3,grid.config=grid.config)
	
	
	
	########################
	#### COMBINING ROWS ####
	########################
	
	grid.rows <- .combine.rows(input=input,rows=c(1,2,3),title="Summary, Barchart & Diagnostics",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(4,5),title="Parallel Coordinate Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(6),title="Heatmap Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(7),title="Biclustmember Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(8,9),title="Biclust Bubble Plot & Barplot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	#grid.rows <- .combine.rows(input=input,rows=c(10),title="Barplot of Bicluster",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	
	
	
	##################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL NEW TOOL FUNCTION ##
	##################################################################
	
	newtool_template(toolname=toolname,methodname=methodname,toolhelp=toolhelp,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames)
	
	
}

