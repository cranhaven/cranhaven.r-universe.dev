# Project: Master Thesis
# 
# Author: Gebruiker
###############################################################################


# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


isadefault_WIN <- function(){     # Change newmethod to your own method name
	
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
	methodname <- "ISA"

	method_result <- gsub(" ","",methodname,fixed=TRUE)
	method_result <- gsub("-","",method_result,fixed=TRUE)
	
	# Define the function (as it is named in your package)
	# Note: If you have got a support function which is used for iterations, use it in this 'mainfunction'
	methodfunction <- "isa.GUI"
	
	
	# Define the name of the data argument for your function
	data.arg <- "data"
	
	
	methodshow <- FALSE
	methodsave <- FALSE
	
	# Define any other arguments in the function, which should not be changed by the user.
	other.arg <- ""  
	
	# Help Object
	methodhelp <- "isa"
	
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
	extrabiclustplot <- FALSE
	
	# ISA only
	extrabiclustforisa <- TRUE
	
	###############################################################################################################################################################################
	###############################################################################################################################################################################
	
	###############################################################################################################################################################################
	## ADDING OF ARGUMENT FRAMES ##
	###############################
	
	
	
	########################
	#### CLUSTERING TAB ####
	########################
	
	input <- "clusterTab"
	
	######		  ENTRY FIELDS FRAME			#####
	#							    		 		#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "rowentry"  
	argument.names <- c("From","To","By") 
	argument.types <- c("num","num","num")
	arguments <- c("thr.row.from","thr.row.to","thr.row.by") 
	initial.values <- c(1,3,0.5)
	title <- "Row Thresholds"
	border <- FALSE
	entry.width <- c("4","4","4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	######		  ENTRY FIELDS FRAME			#####
	#							    		 		#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "colentry"  
	argument.names <- c("From","To","By") 
	argument.types <- c("num","num","num")
	arguments <- c("thr.col.from","thr.col.to","thr.col.by") 
	initial.values <- c(1,3,0.5)
	title <- "Columns Thresholds"
	border <- FALSE
	entry.width <- c("4","4","4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	######		  ENTRY FIELDS FRAME			#####
	#							    		 		#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "seedentry"  
	argument.names <- c("Number of Seeds") 
	argument.types <- c("num")
	arguments <- c("no.seeds") 
	initial.values <- c(100)
	title <- ""
	border <- FALSE
	entry.width <- c("4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	####		RADIO BUTTONS FRAME 	####
	#                               	  #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "rowradio"
	argument.names <- c("Lower than Average","Higher than Average","Both")   
	arguments <- c("dir.row")		
	argument.types <- "char"  
	argument.values <- c("down","up","updown") 
	initial.values <- "updown" 
	title <- "Row Direction:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	
	####		RADIO BUTTONS FRAME 	####
	#                               	  #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "colradio"
	argument.names <- c("Lower than Average","Higher than Average","Both")   
	arguments <- c("dir.col")		
	argument.types <- "char"  
	argument.values <- c("down","up","updown") 
	initial.values <- "updown" 
	title <- "Column Direction:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	
	###############################################################################################################################################################################
	## CONFIGURATION OF GRID OF FRAMES - CLUSTERTAB ##
	##################################################
	
	#########################
	#### THE GRID MATRIX ####
	#########################
	
	grid.config <- .grid.matrix(input=input,c("rowentry","colentry","seedentry","rowradio","colradio",NA),byrow=TRUE,nrow=2,ncol=3,grid.config=grid.config)
	
	
	####################################
	#### COMBINING ROWS -CLUSTERTAB ####
	####################################
	
	grid.rows <- .combine.rows(input=input,rows=c(1,2),title="ISA Specifications",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	
	###############################################################################################################################################################################
	###############################################################################################################################################################################
	
	
	
	
	####################################
	#### PLOTTING & DIAGNOSTICS TAB ####
	####################################
	
	input <- "plotdiagTab"
	
	####			####
	## SUMMARY BUTTON ##
	####			####
	
	####	    	MANUAL BUTTONS FRAME	  ####
	#                               			 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "summaryframe"  
	button.name <- "Summary"  
	button.function <- "isa.summary" 
	button.data <- "" 
	button.biclust <-  "ISA" 
	save <- FALSE
	arg.frames <- c() 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,save=save,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	###############################################################
	
	####			####
	## EXTRACT BUTTON ##
	####			####
	
	####	    	MANUAL BUTTONS FRAME	  ####
	#                               			 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "extractframe"  
	button.name <- "Extract"  
	button.function <- "isa.extract" 
	button.data <- "" 
	button.biclust <-  "modules" 
	save <- TRUE
	arg.frames <- c() 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,save=save,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	###############################################################
	
	####			####
	## SCOREPLOTS ##
	####			####
	
	
	####		RADIO BUTTONS FRAME 		 ####
	#                               			#
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "scoreplotsradio"
	argument.names <- c("Rows","Columns")   
	arguments <- c("type")		
	argument.values <- c("row","col")
	argument.types <- "char"
	initial.values <- "row" 
	title <- "Scores Plot:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "scoreplotsentry"  
	argument.names <- c("Vector of Biclusters") 
	argument.types <- c("num")
	arguments <- c("biclust")
	initial.values <- c("c(1)")
	title <- ""
	border <- FALSE
	entry.width <- c("10")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	####	    	MANUAL BUTTONS FRAME	  ####
	#                               			 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "scoreplotsbutton"  
	button.name <- "Score Plot"  
	button.function <- "isa.scoreplots" 
	button.data <- "" 
	button.biclust <-  "ISA" 
	save <- FALSE
	arg.frames <- c("scoreplotsradio","scoreplotsentry") 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,save=save,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	###############################################################
	
	####			####
	## PLOTMODULE ##
	####			####
	
	####		CHECK BOXES FRAME 			  ####
	#                               			 #
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "plotmodulecheck"
	argument.names <- c("Binary?") 
	arguments <- c("binary") 
	initial.values <- c(1) 
	title <- "Plot Modules:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
	
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "plotmoduleentry"  
	argument.names <- c("Vector of Biclusters") 
	argument.types <- c("num")
	arguments <- c("to.plot")
	initial.values <- c("c(1)")
	title <- ""
	border <- FALSE
	entry.width <- c("10")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME	  ####
	#                               			 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "plotmodulebutton"  
	button.name <- "Moduleplot"  
	button.function <- "plotModules" 
	button.data <- "data" 
	button.biclust <-  "modules" 
	save <- FALSE
	arg.frames <- c("plotmodulecheck","plotmoduleentry") 
	
	# Do not change this line: (without button.otherarg)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,save=save,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	###############################################################
	
	####				####
	## BICLUST DIAGNOSTICS ##
	####				####
	
	
	####	    	MANUAL BUTTONS FRAME		  ####
	#                               	 			 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "fstatbuttonframe"  
	button.name <- "Obs. F Stat."  
	button.function <- "computeObservedFstat" 
	button.data <- "x" 
	button.biclust <-  "" 
	arg.frames <- c("fstatentryframe") 
	button.otherarg <- paste(",bicResult=.isa2biclust(",method_result,")",sep="")
	
	# Do not change this line: 
	new.frames <- .add.frame(button.otherarg=button.otherarg,input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
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
	arg.frames <- c("bootstrapentryframe","bootstrapreplacementframe") 
	button.otherarg <- paste(",bicResult=.isa2biclust(",method_result,")",sep="")


	# Do not change this line: 
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames,button.otherarg=button.otherarg)
	
	
	
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
	button.otherarg <- paste(",bicResult=.isa2biclust(",method_result,")",sep="")
	save <- FALSE
	arg.frames <- c("pplotcheckframe","pplotentryframe","pplottypeframe") 
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	###############################################################
	
	
	
	###############################################################################################################################################################################
	## CONFIGURATION OF GRID OF FRAMES - PLOTDIAGTAB ##
	###################################################
	
	
	#########################
	#### THE GRID MATRIX ####
	#########################
	grid.config <- .grid.matrix(input=input,c("summaryframe","extractframe",NA,"scoreplotsradio","scoreplotsentry","scoreplotsbutton","plotmodulecheck","plotmoduleentry","plotmodulebutton","fstatentryframe","fstatbuttonframe",NA,"bootstrapentryframe",NA,NA,"bootstrapreplacementframe","bootstrapbuttonframe","bootstrapvisualbuttonframe","pplottypeframe","pplotentryframe","parallelbuttonframe","pplotcheckframe",NA,NA),byrow=TRUE,nrow=8,ncol=3,grid.config=grid.config)
	
	

	
	
	########################
	#### COMBINING ROWS ####
	########################
	
	grid.rows <- .combine.rows(input=input,rows=c(1,2,3),title="Summary & ISA Plots",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(4,5,6),title="Biclust Diagnostics",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(7,8),title="Biclust - Parallel Coordinate Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	
	#########################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL CLUSTERTEMPLATE FUNCTION ##
	#########################################################################
	
	cluster_template(methodname=methodname,methodfunction=methodfunction,methodsave=methodsave,methodhelp=methodhelp,data.arg=data.arg,other.arg=other.arg,methodseed=methodseed,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames,superbiclust.comp=superbiclust.comp,bcdiag.comp=bcdiag.comp,data.discr=data.discr,data.bin=data.bin,extrabiclustplot=extrabiclustplot,methodshow=methodshow,extrabiclustforisa=extrabiclustforisa)
	
}

