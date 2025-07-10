# Project: Master Thesis
# 
# Author: Gebruiker
###############################################################################


# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


fabiasparsenessprojection_WIN <- function(){     # Change newmethod to your own method name
	
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
	methodname <- "Fabia Sparseness Projection"
	
	# Define the function (as it is named in your package)
	# Note: If you have got a support function which is used for iterations, use it in this 'mainfunction'
	methodfunction <- "fabias"
	
	
	# Define the name of the data argument for your function
	data.arg <- "X"
	
	
	methodshow <- FALSE
	
	# Define any other arguments in the function, which should not be changed by the user.
	# These arguments may also include a certain method for your function, since it is the idea to give each method a separate window.
	other.arg <- ""  # Comma in the beginning but not at the end ! 
	
	# Help Object
	methodhelp <- "fabias"
	
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
	frame.name <- "entryframe1"  
	argument.names <- c("Number of Biclusters","Number of Iterations","Sparsness Loadings [0,1]","Sparsness Factors [0.5,2]","Random init. loadings (<0 SVD ; >0 [-r,r])") 
	argument.types <- c("num","num","num","num","num")
	arguments <- c("p","cyc","alpha","spz","random") 
	initial.values <- c(13,500,0.01,0.5,1)
	title <- ""
	border <- FALSE
	entry.width <- c("4","4","4","4","4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	######		  ENTRY FIELDS FRAME 	#####
	#							    		#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "entryframe2"  
	argument.names <- c("Min. value of variational pm.","Max. biclusters for row (default = no limit)","Max. rows per bicluster (default = no limit)","Cycle Start (default = beginning)") 
	argument.types <- c("num","num","num","num")
	arguments <- c("lap","nL","lL","bL") 
	initial.values <- c(1,0,0,0)
	title <- ""
	border <- FALSE
	entry.width <- c("4","4","4","4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	####		RADIO BUTTONS FRAME 	####
	#                               	  #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "nonnegframe"
	argument.names <- c("Default","Non-Negative")   
	arguments <- c("non_negative")		
	argument.types <- "num"  
	argument.values <- c("0","1") 
	initial.values <- "0" 
	title <- "Non-negative factors and loadings:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	####		RADIO BUTTONS FRAME 	####
	#                               	  #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "normframe"
	argument.names <- c("None","Quantile (0.75-0.25)","Var=1")   
	arguments <- c("norm")		
	argument.types <- "num"  
	argument.values <- c("0","1","2") 
	initial.values <- "1" 
	title <- "Data Normalization:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	####		RADIO BUTTONS FRAME 	####
	#                               	  #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "centerframe"
	argument.names <- c("None","Mean","Median","Mode")   
	arguments <- c("center")		
	argument.types <- "num"  
	argument.values <- c("0","1","2","3") 
	initial.values <- "2" 
	title <- "Data Centering"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	
	
	###############################################################################################################################################################################
	## CONFIGURATION OF GRID OF FRAMES - CLUSTERTAB ##
	##################################################
	
	#########################
	#### THE GRID MATRIX ####
	#########################
	
	grid.config <- .grid.matrix(input=input,c("entryframe1","entryframe2","nonnegframe",NA,"centerframe","normframe"),byrow=TRUE,nrow=3,ncol=2,grid.config=grid.config)
	
	
	####################################
	#### COMBINING ROWS -CLUSTERTAB ####
	####################################
	
	grid.rows <- .combine.rows(input=input,rows=c(1,2),title="Fabia Specifications",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(3),title="Data Manipulation",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	
	###############################################################################################################################################################################
	###############################################################################################################################################################################
	
	
	
	
	####################################
	#### PLOTTING & DIAGNOSTICS TAB ####
	####################################
	
	input <- "plotdiagTab"
	
	####					#####
	## SUMMARY & SUMMARY PLOTS ##
	####					#####
	
	
	####	    	MANUAL BUTTONS FRAME	  ####
	#                               			#
	
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "summarybutton"  
	button.name <- "Summary"  
	button.function <- "summary"
	button.data <- ""
	button.biclust <-  "object"
	arg.frames <- c() 
	save <- FALSE 
	show <- TRUE
	button.otherarg <- "" 
	
	# Do not change this line: 
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,button.otherarg=button.otherarg,arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
	
	####		RADIO BUTTONS FRAME 	####
	#                               	  #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "summaryradio"
	argument.names <- c("Information Content of Biclusters","Information Content of Samples","Loadings of the Biclusters","Factors of the Biclusters")   
	arguments <- c("which")		
	argument.types <- "num"  
	argument.values <- c("1","2","3","4") 
	initial.values <- "1" 
	title <- "Summary Plot:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	####	    	MANUAL BUTTONS FRAME	  ####
	#                               			#
	
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "summaryplotbutton"  
	button.name <- "Draw Plot"  
	button.function <- "showSelected"
	button.data <- ""
	button.biclust <-  "object"
	arg.frames <- c("summaryradio") 
	save <- FALSE 
	show <- TRUE
	button.otherarg <- "" 
	
	# Do not change this line: ( STILL NEED TO DELETE BUTTON.OTHERARG FROM THIS LINE)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,button.otherarg=button.otherarg,arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
	
	
	####					#####
	## 			BI PLOT 	   ##
	####					#####
	
	
	######		  ENTRY FIELDS FRAME 	#####
	#							    		#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "biplotentry"  
	argument.names <- c("First","Second") 
	argument.types <- c("num","num")
	arguments <- c("dim1","dim2") 
	initial.values <- c(1,2)
	title <- "Principal Factors"
	border <- FALSE
	entry.width <- c("4","4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	####	    	MANUAL BUTTONS FRAME	  ####
	#                               			#
	
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "biplotbutton"  
	button.name <- "Draw Plot"  
	button.function <- "fabia.biplot"
	button.data <- ""
	button.biclust <-  "x"
	arg.frames <- c("biplotentry") 
	save <- FALSE 
	show <- TRUE
	button.otherarg <- "" 
	
	# Do not change this line: ( STILL NEED TO DELETE BUTTON.OTHERARG FROM THIS LINE)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,button.otherarg=button.otherarg,arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
	
	
	####							#####
	## 	Extract Bicluster & PLOT 	   ##
	####							#####
	
	####		RADIO BUTTONS FRAME 	####
	#                               	  #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "extractradio"
	argument.names <- c("Noise Free data (if available)","Data","Reconstructed Data","Error","Absolute Loadings","Absolute Factors")   
	arguments <- c("which")		
	argument.types <- "num"  
	argument.values <- c("1","2","3","4","5","6") 
	initial.values <- "2" 
	title <- "Extract which plot?"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	######		  ENTRY FIELDS FRAME 	#####
	#							    		#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "extractentry1"  
	argument.names <- c("Threshold bicluster sample","Threshold bicluster loading (def. = estimated)") 
	argument.types <- c("num","num")
	arguments <- c("thresZ","thresL") 
	initial.values <- c("0.5","NULL")
	title <- ""
	border <- FALSE
	entry.width <- c("5","5")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	######		  ENTRY FIELDS FRAME 	#####
	#							    		#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "extractentry2"  
	argument.names <- c("Noise Free Data Matrix") 
	argument.types <- c("num")
	arguments <- c("Y") 
	initial.values <- c("NULL")
	title <- "Optional Noise Data Matrix:"
	border <- FALSE
	entry.width <- c("7")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	####	    	MANUAL BUTTONS FRAME	  ####
	#                               			#
	
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "extractbutton"  
	button.name <- "Extract"  
	button.function <- "extractBic"
	button.data <- ""
	button.biclust <-  "fact"
	arg.frames <- c("extractentry1") 
	save <- TRUE 
	show <- TRUE
	button.otherarg <- "" 
	
	# Do not change this line: ( STILL NEED TO DELETE BUTTON.OTHERARG FROM THIS LINE)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,button.otherarg=button.otherarg,arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME	  ####
	#                               			#
	
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "extractplotbutton"  
	button.name <- "Extract Plot"  
	button.function <- "extractPlot"
	button.data <- ""
	button.biclust <-  "fact"
	arg.frames <- c("extractentry1","extractentry2","extractradio") 
	save <- FALSE
	show <- TRUE
	button.otherarg <- "" 
	
	# Do not change this line: ( STILL NEED TO DELETE BUTTON.OTHERARG FROM THIS LINE)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,button.otherarg=button.otherarg,arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
	
	
	####							#####
	## 				BICLUSTERPLOT	   ##
	####							#####
	
	####		RADIO BUTTONS FRAME 	####
	#                               	  #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "biclusterradio"
	argument.names <- c("Data Matrix (with bicluster)","Only Bicluster")   
	arguments <- c("which")		
	argument.types <- "num"  
	argument.values <- c("1","2") 
	initial.values <- "1" 
	title <- "Bicluster Plot? (Extract First Required!)"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	
	######		  ENTRY FIELDS FRAME 	#####
	#							    		#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "biclusterentry"  
	argument.names <- c("Bicluster Number") 
	argument.types <- c("num")
	arguments <- c("p") 
	initial.values <- c("1")
	title <- ""
	border <- FALSE
	entry.width <- c("4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	####		CHECK BOXES FRAME 			  ####
	#                               			 #
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "biclustercheck"
	argument.names <- c("Opposite Bicluster?") 
	arguments <- c("opp") 
	initial.values <- c(0) 
	title <- ""
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
	
	
	
	####	    	MANUAL BUTTONS FRAME	  ####
	#                               			#
	
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "biclusterplotbutton"  
	button.name <- "Draw Plot"  
	button.function <- "plotBicluster"
	button.data <- ""
	button.biclust <-  ""
	arg.frames <- c("biclusterentry","biclusterradio","biclustercheck") 
	save <- FALSE
	show <- TRUE
	button.otherarg <- "r=Extract" 
	
	# Do not change this line: ( STILL NEED TO DELETE BUTTON.OTHERARG FROM THIS LINE)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,button.otherarg=button.otherarg,arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
	
	
	### EXTRA BICLUST PLOTS BUTTON
	
	####	    	MANUAL BUTTONS FRAME	  ####
	#                               			#
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "exportbiclustbutton"  
	button.name <- "Biclust Plots"  
	button.function <- "fabiabiclust_WINDOW"
	button.data <- ""
	button.biclust <-  ""
	arg.frames <- c("extractentry1") 
	save <- FALSE
	show <- FALSE
	button.otherarg <- paste("methodname='",methodname,"'",sep="") 
	
	# Do not change this line: ( STILL NEED TO DELETE BUTTON.OTHERARG FROM THIS LINE)
	new.frames <- .add.frame(input=input,frame.name=frame.name,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,button.otherarg=button.otherarg,arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
	
	
	###############################################################################################################################################################################
	## CONFIGURATION OF GRID OF FRAMES - PLOTDIAGTAB ##
	###################################################
	
	
	#########################
	#### THE GRID MATRIX ####
	#########################
	
	
	grid.config <- .grid.matrix(input=input,c("summaryradio","summarybutton","summaryplotbutton","extractentry1","extractbutton","exportbiclustbutton","extractradio" ,"extractentry2",NA,"extractplotbutton",NA,NA ,"biclusterradio","biclustercheck",NA,"biclusterentry","biclusterplotbutton",NA   ,"biplotentry","biplotbutton",NA),byrow=TRUE,nrow=7,ncol=3,grid.config=grid.config)
	
	
	
	########################
	#### COMBINING ROWS ####
	########################
	
	grid.rows <- .combine.rows(input=input,rows=c(1),title="Summary & Summary Plots",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(2,3,4,5,6),title="Extract,Extract Plot & Bicluster Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(7),title="BiPlot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	#########################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL CLUSTERTEMPLATE FUNCTION ##
	#########################################################################
	
	cluster_template(methodname=methodname,methodfunction=methodfunction,methodhelp=methodhelp,data.arg=data.arg,other.arg=other.arg,methodseed=methodseed,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames,superbiclust.comp=superbiclust.comp,bcdiag.comp=bcdiag.comp,data.discr=data.discr,data.bin=data.bin,extrabiclustplot=extrabiclustplot,methodshow=methodshow)
	
}

