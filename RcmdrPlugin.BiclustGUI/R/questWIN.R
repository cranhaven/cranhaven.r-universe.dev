# Project: Master Thesis
# 
# Author: Gebruiker
###############################################################################


# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


biclustquest_WIN <- function(){     # Change newmethod to your own method name
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	# List of frame objects. add.frame will return new.frames with an additional frame
	# Possible improvement: Instead of overwriting new.frames each time. 
	#      Maybe save the variable to something global or in a separate environment.
	
	
	
	###############################################################################################################################################################################
	## GENERAL INFORMATION ABOUT THE NEW METHOD/WINDOW ##
	#####################################################
	
	# Note that the idea is that each new dialog coincides with 1 clustering function on which 
	# multiple plot or diagnostic functions can be used (or even general ones).
	
	# compatibility (with superclust & bcdiag)
	# clusterfunction, plotfunctions, diagnosticfunctions
	# name
	
	
	# Define the name of the method as it will appear in the top of the window:
	methodname <- "Questmotif"
	
	# Define the function (as it is named in your package)
	# Note: If you have got a support function which is used for iterations, use it in this 'mainfunction'
	methodfunction <- "biclust.quest.GUI"
	
	
	# Define the name of the data argument for your function
	data.arg <- "x"
	
	
	# Define any other arguments in the function, which should not be changed by the user.
	# These arguments may also include a certain method for your function, since it is the idea to give each method a separate window.
	other.arg <- ""  # Here this the method needs to be specified by the user!
	
	# Help Object
	methodhelp <- "BCQuest"

	# Extra
	methodshow <- FALSE
	methodsave <- FALSE
	
	# Possibility to give a seed ?
	methodseed <- TRUE
	
	# Add a discretize box?
	data.discr <- FALSE
	
	# Add a binarize box?
	data.bin <- FALSE
	
	## COMPATIBILITY? ##
	
	# BcDiag
	bcdiag.comp <- TRUE
	
	# SuperBiclust
	superbiclust.comp <- TRUE
	
	# Biclust only
	extrabiclustplot <- TRUE
	
	###############################################################################################################################################################################
	###############################################################################################################################################################################
	
	###############################################################################################################################################################################
	## ADDING OF ARGUMENT FRAMES ##
	###############################
	
	
	
	########################
	#### CLUSTERING TAB ####
	########################
	
	input <- "clusterTab"
	
	####		RADIO BUTTONS FRAME - EXAMPLE 				####
	#                               						   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "methodframe"
	argument.names <- c("Nominal Data (Default)","Ordinal Data","Metric Data")  
	arguments <- c("method")		
	argument.values <- c("BCQuest","BCQuestord","BCQuestmet") 
	argument.types <- "char"
	initial.values <- "BCQuest" 
	title <- "Method:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,argument.types=argument.types,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames)	
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "metframe"  
	argument.names <- c("Quantile","Variance") 
	argument.types <- c("num","num")
	arguments <- c("quant","vari")
	initial.values <- c(0.25,1)
	title <- "Metric Specific Options:"
	border <- FALSE
	entry.width <- c("4","4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "ordframe"  
	argument.names <- c("Half Margin of Interval") 
	argument.types <- c("num")
	arguments <- c("d")
	initial.values <- c(1)
	title <- "Ordinal Specific Options:"
	border <- FALSE
	entry.width <- c("4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "entryframe1"  
	argument.names <- c("Number of samples choosen","Number of repetitions","Sample Size in repetitions") 
	argument.types <- c("num","num","num")
	arguments <- c("ns","nd","sd")
	initial.values <- c(10,10,5)
	title <- ""
	border <- FALSE
	entry.width <- c("4","4","4")  
	
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "entryframe2"  
	argument.names <- c("Scaling Factor for column result","Number of biclusters") 
	argument.types <- c("num","num")
	arguments <- c("alpha","number")
	initial.values <- c(0.05,100)
	title <- ""
	border <- FALSE
	entry.width <- c("4","4")  
	
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	###############################################################################################################################################################################
	## CONFIGURATION OF GRID OF FRAMES - CLUSTERTAB ##
	##################################################
	
	#########################
	#### THE GRID MATRIX ####
	#########################
	
	grid.config <- .grid.matrix(input=input,c("methodframe",NA,"metframe","ordframe","entryframe1","entryframe2"),byrow=TRUE,nrow=3,ncol=2,grid.config=grid.config)
	
	
	####################################
	#### COMBINING ROWS -CLUSTERTAB ####
	####################################
	grid.rows <- .combine.rows(input=input,rows=c(1,2),title="Method Specifications",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(3),title="Questmotif Specifications",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	
	###############################################################################################################################################################################
	###############################################################################################################################################################################
	
	
	
	
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
	button.biclust <-  "bicResult" 
	button.otherarg <- ""
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
	button.biclust <-  "bicResult" 
	save <- FALSE
	arg.frames <- c("heatplotcheckframe","heatplotentryframe") 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,save=save,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
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
	button.biclust <-  "bicResult" 
	save <- FALSE
	arg.frames <- c("mplotcheckframe","mplotentryframe") 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	###############################################################
	
	
	###							###
	## Summary & Diagnostics Box ##
	###							###
	
	
	####	    	MANUAL BUTTONS FRAME 	  ####
	#             								 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "summarybuttonframe"  
	button.name <- "Summary"  
	button.function <- "summary" 
	button.data <- "" 
	button.biclust <-  "object" 
	arg.frames <- c()
	save <- FALSE
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	###############################################################
	
	
	####	    	MANUAL BUTTONS FRAME		  ####
	#                               	 			 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "fstatbuttonframe"  
	button.name <- "Obs. F Stat."  
	button.function <- "computeObservedFstat" 
	button.data <- "x" 
	button.biclust <-  "bicResult" 
	arg.frames <- c("fstatentryframe") 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
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
	button.biclust <-  "bicResult" 
	arg.frames <- c("bootstrapentryframe","bootstrapreplacementframe") 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
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
	
	
	###############################################################################################################################################################################
	## CONFIGURATION OF GRID OF FRAMES - PLOTDIAGTAB ##
	###################################################
	
	
	#########################
	#### THE GRID MATRIX ####
	#########################
	
	
	grid.config <- .grid.matrix(input=input,c("summarybuttonframe","fstatentryframe","fstatbuttonframe","bootstrapentryframe",NA,NA,"bootstrapreplacementframe","bootstrapbuttonframe","bootstrapvisualbuttonframe" ,"pplottypeframe","pplotentryframe","parallelbuttonframe","pplotcheckframe",NA,NA,"heatplotcheckframe","heatplotentryframe","heatbuttonframe","mplotcheckframe","mplotentryframe","memberbuttonframe"),byrow=TRUE,nrow=7,ncol=3,grid.config=grid.config)
	
	
	
	########################
	#### COMBINING ROWS ####
	########################
	
	grid.rows <- .combine.rows(input=input,rows=c(1,2,3),title="Summary & Diagnostics",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(4,5),title="Parallel Coordinate Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(6),title="Heatmap Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(7),title="Biclustmember Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	#########################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL CLUSTERTEMPLATE FUNCTION ##
	#########################################################################
	
	cluster_template(methodname=methodname,methodfunction=methodfunction,methodhelp=methodhelp,methodshow=methodshow,methodsave=methodsave,data.arg=data.arg,other.arg=other.arg,methodseed=methodseed,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames,superbiclust.comp=superbiclust.comp,bcdiag.comp=bcdiag.comp,data.discr=data.discr,data.bin=data.bin,extrabiclustplot=extrabiclustplot)
	
}

