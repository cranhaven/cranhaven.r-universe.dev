# Project: BiclustGUI
# 
# Author: lucp8394
###############################################################################


rqubic_WINDOW <- function(){     # Change newmethod to your own method name
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	
	#####################################################
	## GENERAL INFORMATION ABOUT THE NEW METHOD/WINDOW ##
	#####################################################
	
	methodname <- "Rqubic"
	methodfunction <- "rqubic.GUI"
	methodsave <- FALSE
	other.arg <- ""
	methodhelp <- "rqubic"
	data.arg <- "x"
	data.transf <- "ExprSet"
	
	# Extra Data Conversion Boxes
	data.discr <- FALSE
	data.bin <- FALSE
	
	# Possibility to give a seed ?
	methodseed <- TRUE
	
	## COMPATIBILITY? ##
	
	# BcDiag
	bcdiag.comp <- TRUE
	
	# SuperBiclust
	superbiclust.comp <- TRUE
	
	# Biclust only (Not for public use)
	extrabiclustplot <- TRUE
	
	########################
	#### CLUSTERING TAB ####
	########################
	
	input <- "clusterTab"
	
	### 1. ADDING THE FRAMES ###
	
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "datainputframe"  
	argument.names <- c("ExpressionSet Name") 
	argument.types <- c("char") 
	arguments <- c("eSetData.name") 
	initial.values <- c("NULL")
	title <- ""
	border <- FALSE
	entry.width <- c("15")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	#### CHECK BOXES FRAME  ####
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "disccheckframe"
	argument.names <- c("Discretize?")  
	arguments <- c("check.disc") 
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
	frame.name <- "discentryframe"  
	argument.names <- c("Estimated Proportion","Levels") 
	argument.types <- c("num","num") 
	arguments <- c("q","rank") 
	initial.values <- c(0.06,1)
	title <- ""
	border <- FALSE
	entry.width <- c("4","4")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "seedentryframe"  
	argument.names <- c("Minimum Score") 
	argument.types <- c("num") 
	arguments <- c("minColWidth") 
	initial.values <- c(2)
	title <- ""
	border <- FALSE
	entry.width <- c("3")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "qubicentryframe"  
	argument.names <- c("Max Biclusters","Tolerance","Redundant Proportion") 
	argument.types <- c("num","num","num") 
	arguments <- c("report.no","tolerance","filter.proportion") 
	initial.values <- c(100,0.95,1)
	title <- ""
	border <- FALSE
	entry.width <- c("4","4","4")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	
	
	### 2. CONFIGURING THE GRID ###
	
	grid.config <- .grid.matrix(input=input,c("datainputframe",NA,"disccheckframe","discentryframe","seedentryframe",NA,"qubicentryframe",NA),nrow=4,ncol=2,byrow=TRUE,grid.config=grid.config)
	
	
	### 3. COMBING THE ROWS ###
	
	grid.rows <- .combine.rows(input=input,rows=c(1),title="Expression Set Input (NULL='Active Dataset') ",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(2),title="Quantile Discretization ",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(3),title="Generate Seeds ",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(4),title="Qualitative Biclustering Options ",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	
	####################################
	#### PLOTTING & DIAGNOSTICS TAB ####
	####################################
	
	input <- "plotdiagTab"
	
	### 1. ADDING THE FRAMES ###
	
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
	button.otherarg <- ""
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
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
	
	
	### 2. CONFIGURING THE GRID ###
	
	grid.config <- .grid.matrix(input=input,c("summarybuttonframe","fstatentryframe","fstatbuttonframe","bootstrapentryframe",NA,NA,"bootstrapreplacementframe","bootstrapbuttonframe","bootstrapvisualbuttonframe" ,"pplottypeframe","pplotentryframe","parallelbuttonframe","pplotcheckframe",NA,NA,"heatplotcheckframe","heatplotentryframe","heatbuttonframe","mplotcheckframe","mplotentryframe","memberbuttonframe"),byrow=TRUE,nrow=7,ncol=3,grid.config=grid.config)
	
	### 3. COMBING THE ROWS ###
	
	grid.rows <- .combine.rows(input=input,rows=c(1,2,3),title="Summary & Diagnostics",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(4,5),title="Parallel Coordinate Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(6),title="Heatmap Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(7),title="Biclustmember Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	#########################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL CLUSTERTEMPLATE FUNCTION ##
	#########################################################################
	
	cluster_template(methodname=methodname,methodfunction=methodfunction,extrabiclustplot=extrabiclustplot,methodhelp=methodhelp,data.arg=data.arg,other.arg=other.arg,methodseed=methodseed,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames,superbiclust.comp=superbiclust.comp,bcdiag.comp=bcdiag.comp,data.transf=data.transf,data.discr=data.discr,data.bin=data.bin,methodshow=methodshow,methodsave=methodsave)
	
}

#rqubic_WINDOW()


#test <- as.matrix(BicatYeast)
#
#
#
# demo.exprs <- new("ExpressionSet", exprs=test)
### processing the condition information
#		 demo.cond.split <- strsplit(sub("\\.CEL", "", colnames(BicatYeast)), "_")
# demo.group <- sapply(demo.cond.split, function(x) paste(x[-length(x)], collapse="_"))
# demo.time <- sapply(demo.cond.split, function(x) x[length(x)])
# pData(demo.exprs) <- data.frame(group=demo.group, time=demo.time)
# sampleNames(demo.exprs) <- paste(demo.group, demo.time)
# demo.disc <- quantileDiscretize(demo.exprs)
#
# demo.disc.test <- as.data.frame(exprs(demo.disc))

