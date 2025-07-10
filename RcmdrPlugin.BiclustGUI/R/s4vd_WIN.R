# Project: BiclustGUI
# 
# Author: lucp8394
###############################################################################


s4vd_WIN <- function(){     # Change newmethod to your own method name
	
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
	methodname <- "S4VD"
	
	# Define the function (as it is named in your package)
	# Note: If you have got a support function which is used for iterations, use it in this 'mainfunction'
	methodfunction <- "biclust"
	
	
	# Define the name of the data argument for your function
	data.arg <- "x"
	
	
	# Define any other arguments in the function, which should not be changed by the user.
	# These arguments may also include a certain method for your function, since it is the idea to give each method a separate window.
	other.arg <- ",method=BCs4vd()"  # Comma in the beginning but not at the end ! 
	
	# Help Object
	methodhelp <- "BCs4vd"
	
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
	
	# Biclust only (Not for public use)
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
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "generalentry1"  
	argument.names <- c("Maximum number of biclusters","Weight for adaptive LASSO (>=0 ; 0=LASSO)","Maximum number of iterations in single BC","Convergence Threshold") 
	argument.types <- c("num","num","num","num") 
	arguments <- c("nbiclust","gamm","iter","merr") 
	initial.values <- c(10,0,100,1e-04)
	title <- ""
	border <- FALSE
	entry.width <- c("5","5","5","5")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "generalentry2"  
	argument.names <- c("Minimal number of rows","Minimal number of columns","Minimal starting iterations before convergence") 
	argument.types <- c("num","num","num") 
	arguments <- c("row.min","col.min","start.iter") 
	initial.values <- c(4,4,0)
	title <- ""
	border <- FALSE
	entry.width <- c("3","3","3")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	#### CHECK BOXES FRAME  ####
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "generalcheck1"
	argument.names <- c("Allow neg. corr. of columns over rows","Allow neg. corr. of rows over columns","Allow column overlap between BC's","Allow row overlap between BC's")  
	arguments <- c("cols.nc","rows.nc","col.overlap","row.overlap") 
	initial.values <- c(0,1,1,1)  
	title <- ""
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,new.frames=new.frames)
	
	#### CHECK BOXES FRAME  ####
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "generalcheck2"
	argument.names <- c("Save stability path for plotting")  
	arguments <- c("savepath") 
	initial.values <- c(1)  
	title <- ""
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,new.frames=new.frames)
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "SSentry1"  
	argument.names <- c("Number of subsamples","Size of subsamples","Range cutoff threshold") 
	argument.types <- c("num","num","num") 
	arguments <- c("steps","size","ss.thr") 
	initial.values <- c(100,0.632,"c(0.6,0.65)")
	title <- ""
	border <- FALSE
	entry.width <- c("5","5","10")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "SSentry2"  
	argument.names <- c("PCWER right singular vector (columns)","PCWER left singular vector (rows)") 
	argument.types <- c("num","num") 
	arguments <- c("pcerv","pceru") 
	initial.values <- c(0.05,0.05)
	title <- ""
	border <- FALSE
	entry.width <- c("5","5")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	#### CHECK BOXES FRAME  ####
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "SScheck1"
	argument.names <- c("Fast pointwise stability selection")  
	arguments <- c("pointwise") 
	initial.values <- c(1)  
	title <- ""
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,new.frames=new.frames)
	
	###############################################################################################################################################################################
	## CONFIGURATION OF GRID OF FRAMES - CLUSTERTAB ##
	##################################################
	
	#########################
	#### THE GRID MATRIX ####
	#########################
	
	grid.config <- .grid.matrix(input=input,c("generalentry1",NA,"generalentry2",NA,"generalcheck1","generalcheck2","SSentry1","SSentry2","SScheck1",NA),byrow=TRUE,nrow=5,ncol=2,grid.config=grid.config)
	
	
	####################################
	#### COMBINING ROWS -CLUSTERTAB ####
	####################################
	
	grid.rows <- .combine.rows(input=input,rows=c(1,2,3),title="General Parameters",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(4,5),title="Stability Selection Paramaters",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	
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
	button.width <- "12"
	button.data.transf <- "matrix"
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.data.transf=button.data.transf,button.width=button.width,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
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
	
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "stabpathnumber"  
	argument.names <- c("Bicluster Number") 
	argument.types <- c("num")
	arguments <- c("number")
	initial.values <- c(1)
	title <- ""
	border <- FALSE
	entry.width <- c("4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME		  ####
	#                               				 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "stabpathbutton"  
	button.name <- "Visualize"  
	button.function <- "stabpath" 
	button.data <- "" 
	button.biclust <-  "res" 
	button.otherarg <- ""
	save <- FALSE
	show <- TRUE
	arg.frames <- c("stabpathnumber") 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames,button.otherarg=button.otherarg,show=show)
	
	
	
	###############################################################################################################################################################################
	## CONFIGURATION OF GRID OF FRAMES - PLOTDIAGTAB ##
	###################################################
	
	
	#########################
	#### THE GRID MATRIX ####
	#########################
	
	
	grid.config <- .grid.matrix(input=input,c("summarybuttonframe","fstatentryframe","fstatbuttonframe","bootstrapentryframe",NA,NA,"bootstrapreplacementframe","bootstrapbuttonframe","bootstrapvisualbuttonframe" ,"pplottypeframe","pplotentryframe","parallelbuttonframe","pplotcheckframe",NA,NA,"heatplotcheckframe","heatplotentryframe","heatbuttonframe","mplotcheckframe","mplotentryframe","memberbuttonframe","stabpathnumber","stabpathbutton",NA),byrow=TRUE,nrow=8,ncol=3,grid.config=grid.config)
	
	
	
	########################
	#### COMBINING ROWS ####
	########################
	
	grid.rows <- .combine.rows(input=input,rows=c(1,2,3),title="Summary & Diagnostics",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(4,5),title="Parallel Coordinate Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(6),title="Heatmap Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(7),title="Biclustmember Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(8),title="S4VD Plot - Stability Selection Path",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
		
	#########################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL CLUSTERTEMPLATE FUNCTION ##
	#########################################################################
	
	cluster_template(methodname=methodname,methodfunction=methodfunction,methodhelp=methodhelp,data.arg=data.arg,other.arg=other.arg,methodseed=methodseed,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames,superbiclust.comp=superbiclust.comp,bcdiag.comp=bcdiag.comp,data.discr=data.discr,data.bin=data.bin,extrabiclustplot=extrabiclustplot)
	
}
