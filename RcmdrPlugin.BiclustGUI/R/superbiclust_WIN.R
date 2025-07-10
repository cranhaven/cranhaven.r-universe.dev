# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


superbiclust_WINDOW <- function(methodname,methodseed,methodsave){  
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	
	method_result <- gsub(" ","",methodname,fixed=TRUE)
	method_result <- gsub("-","",method_result,fixed=TRUE)
	
	extra.arg <- paste0(",method_result='",method_result,"'")
	
	# THE TRANSFORMATION IS DONE INSIDE THE superbiclust.GUI FUNCTION
	# (It was easier to put there since sometitmes other objects have to be transformed aswell)
	

	
	###############################################################################################################################################################################
	## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
	#####################################################
	
	
	toolname <- "SuperBiclust"
	
	toolhelp <- "superbiclust" 
	

	
	# Do not change this line:
	input <- "plotdiagTab"
	
	#######################
	## MAKING THE WINDOW ##
	#######################
	
	### ADDING FRAMES ####
	
	# Idem as plotdiag tab.
	
	####			 ####
	## REPEAT ANALYSIS ##
	####			 ####
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "repeatentry1"  
	argument.names <- c("Number of Repeats","Starting Seed (empty=random)") 
	argument.types <- c("num","num")
	arguments <- c("number.repeats","start.seed")
	initial.values <- c("10","")
	title <- ""
	border <- FALSE
	entry.width <- c("5","5")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "repeatentry2"  
	argument.names <- c("Object Name") 
	argument.types <- c("char")
	arguments <- c("object.name")
	initial.values <- c(paste0(method_result,"List"))
	title <- ""
	border <- FALSE
	entry.width <- c("15")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "repeatanalysis"  
	button.name <- "Run"  
	button.function <- "repeatanalysis.GUI" 
	button.data <- "" 
	button.biclust <-  ""
	button.otherarg <- paste0("method_result='",method_result,"',methodsave=",methodsave,",toolname='",toolname,"'")
	save <- FALSE
	show <- FALSE
	arg.frames <- c("repeatentry1","repeatentry2") 
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	#### 			  ####
	## EXTRA DATA INPUT ##
	#####			 #####
	
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "biclustcombine"  
	argument.names <- c("") 
	argument.types <- c("num")
	arguments <- c("extra.biclust")
	initial.values <- c("NULL")
	title <- ""
	border <- FALSE
	entry.width <- c("50")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "chooseresults"  
	button.name <- "Choose"  
	button.function <- "chooseresultsGUI" 
	button.data <- "" 
	button.biclust <-  ""
	button.otherarg <- paste0("methodname='",methodname,"',toolname='",toolname,"',methodseed=",methodseed)
	save <- FALSE
	show <- FALSE
	arg.frames <- c() 
		
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
	#### 			  ####
	## FABIA THRESHOLDS ##
	#####			 #####
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "fabiaoptions"  
	argument.names <- c("Bicluster Samples Threshold","Bicluster Loadings Threshold") 
	argument.types <- c("char","char")
	arguments <- c("fabia.thresZ","fabia.thresL")
	initial.values <- c("0.5","NULL")
	title <- ""
	border <- FALSE
	entry.width <- c("5","5")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	
	
	####						  ####
	##	SUPERBICLUST CONFIGURATION	##
	####						  ####
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "indexradio"
	argument.names <- c("Jaccard","Sorensen","Ochiai","Kulczynski","Sensitivity","Specificity")  
	arguments <- c("index")		
	argument.values <- c("jaccard","sorensen","ochiai","kulczynski","sensitivity","specificity") 
	argument.types <- "char"
	initial.values <- "jaccard" 
	title <- "Similarity?"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "typeradio"
	argument.names <- c("Rows","Columns","Both")  
	arguments <- c("type")		
	argument.values <- c("rows","cols","both") 
	argument.types <- "char"
	initial.values <- "both" 
	title <- "Type?"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "superbiclust"  
	button.name <- "SuperBiclust"  
	button.function <- "superbiclust.GUI" 
	button.data <- "" 
	button.biclust <-  "x"
	button.otherarg <- extra.arg
	save <- FALSE
	show <- FALSE
	arg.frames <- c("indexradio","typeradio","biclustcombine","fabiaoptions") 
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	####		####
	## DENDOGRAM  ##
	####		####
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "drawtree"  
	button.name <- "Draw Tree"  
	button.function <- "plot" 
	button.data <- "" 
	button.biclust <-  ""
	button.otherarg <- "x=superbiclust.tree"
	save <- FALSE
	show <- TRUE
	arg.frames <- c() 
	

	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	

	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "cutentry"  
	argument.names <- c("Number of Biclusters","Height") 
	argument.types <- c("num","num")
	arguments <- c("k","h")
	initial.values <- c("NULL","NULL")
	title <- "Where to cut tree? (number overrides height)"
	border <- FALSE
	entry.width <- c("6","6")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "cutree"  
	button.name <- "Cut Tree"  
	button.function <- "cutree" 
	button.data <- "" 
	button.biclust <-  ""
	button.otherarg <- "tree=superbiclust.tree"
	save <- TRUE
	show <- TRUE
	arg.frames <- c("cutentry") 
	
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	####						####
	## Plotting Robust Biclusters ##
	####						####
	
	####		CHECK BOXES FRAME 			  ####
	#                               			 #
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "showrobust"
	argument.names <- c("Show Inside Robust BC?") 
	arguments <- c("show.which") 
	initial.values <- c(0) 
	title <- ""
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "robustbutton"  
	button.name <- "Robust BC"  
	button.function <- "superbiclust.robust.GUI" 
	button.data <- "" 
	button.biclust <-  ""
	button.otherarg <- "CutTree=CutTree"
	save <- FALSE
	show <- TRUE
	arg.frames <- c("showrobust") 
	
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "plottyperadio"
	argument.names <- c("Within Biclusters","All Samples")  
	arguments <- c("type")		
	argument.values <- c("within","all") 
	argument.types <- "char"
	initial.values <- "within" 
	title <- "Type?"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "whichrobust"  
	argument.names <- c("Number") 
	argument.types <- c("num")
	arguments <- c("which.robust")
	initial.values <- c("")
	title <- "Which Robust BC?"
	border <- FALSE
	entry.width <- c("4")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "superplot"  
	button.name <- "Draw Plot"  
	button.function <- "plotSuper.GUI" 
	button.data <- "" 
	button.biclust <-  ""
	button.otherarg <- "CutTree=CutTree"
	save <- FALSE
	show <- FALSE
	arg.frames <- c("plottyperadio","whichrobust") 
	
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	###			 ####
	## SAVE BUTTON ##
	####		 ####
	
	# ONLY FOR BICLUST

	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #

	type <- "buttons"

	# Change variables accordingly:
	frame.name <- "savebutton"  
	button.name <- "Save"  
	button.function <- "biclust.robust.fuse.GUI" 
	button.data <- "" 
	button.biclust <-  ""
#	button.otherarg <- paste("CutTree=CutTree,superbiclust.result=superbiclust.result",",method_result='",method_result,"'",sep="")
	button.otherarg <- paste("method_result='",method_result,"'",sep="")
	save <- FALSE
#	show <- TRUE
	show <- FALSE
	arg.frames <- c() 


	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)

	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "resetbutton"  
	button.name <- paste0("Reset (Original ",methodname,")")  
	button.function <- "robust.reset" 
	button.data <- "" 
	button.biclust <-  ""
	button.otherarg <-  paste("method_result='",method_result,"'",sep="")
	button.width <- paste0(nchar(button.name)-2)
	save <- FALSE
	show <- FALSE
	arg.frames <- c() 
	
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,button.width=button.width,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
	### CONFIGURING GRID ###
	
	if(methodseed){
		grid.config <- .grid.matrix(input=input,c("repeatentry1","repeatentry2","repeatanalysis","biclustcombine","chooseresults",NA,"fabiaoptions",NA,NA,"indexradio","typeradio","superbiclust","drawtree",NA,NA,"cutentry","cutree",NA,"showrobust","robustbutton",NA,"plottyperadio","whichrobust","superplot","savebutton","resetbutton",NA),byrow=TRUE,nrow=9,ncol=3,grid.config=grid.config)
		
	}else{
		grid.config <- .grid.matrix(input=input,c("biclustcombine","chooseresults",NA,"fabiaoptions",NA,NA,"indexradio","typeradio","superbiclust","drawtree",NA,NA,"cutentry","cutree",NA,"showrobust","robustbutton",NA,"plottyperadio","whichrobust","superplot","savebutton","resetbutton",NA),byrow=TRUE,nrow=8,ncol=3,grid.config=grid.config)
	}	
	
	### COMBINING ROWS ###
	
	if(methodseed){
		grid.rows <- .combine.rows(input=input,rows=c(1),title=paste0("Repeat ",methodname," with LAST USED parameters, NOT last selected."),border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(2),title="Extra Data Input - Vector of Results Objects ( c('result1','result2') )",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(3),title="Fabia Thresholds (if required)",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(4),title="Superbiclust Configuration",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(5,6),title="Dendogram",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(7,8),title="Robust Bicluster Gene Profiles",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(9),title="Save the Robust Biclusters (Biclust & BcDiag Only)",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		
	}else{
		grid.rows <- .combine.rows(input=input,rows=c(1),title="Extra Data Input - Vector of Results Objects ( c('result1','result2') )",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(2),title="Fabia Thresholds (if required)",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(3),title="Superbiclust Configuration",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(4,5),title="Dendogram",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(6,7),title="Robust Bicluster Gene Profiles",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		grid.rows <- .combine.rows(input=input,rows=c(8),title="Save the Robust Biclusters (Biclust & BcDiag Only)",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	}
		
	
	
	
	##################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL NEW TOOL FUNCTION ##
	##################################################################
	
	newtool_template(toolname=toolname,methodname=methodname,toolhelp=toolhelp,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames)
	
	
}
