# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


bcdiag_WINDOW <- function(methodname){  
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
### OLD CODE:

#	# Some extra code to determine the input type: "biclust", "fabia", "isa2"
#	biclust.names <- c("Bimax","CC","Plaid","Questmotif","Spectral","XMotifs","IBBIG","Rqubic")
#	fabia.names <- c("Fabia Laplace Prior","Fabia Post-Projection","Fabia Sparseness Projection","Fabia SPARSE")
#	isa.names <- c("ISA")
#	bicare.names <- c("BICARE")
#	
#	if(methodname %in% biclust.names){
#		extra.arg <- ",mname='biclust'"
#	}
#	if(methodname %in% fabia.names){
#		extra.arg <- ",mname='fabia'"
#
#		# Exception when superbiclust has been used on fabia:
#		method_result <- gsub(" ","",methodname,fixed=TRUE)
#		method_result <- gsub("-","",method_result,fixed=TRUE)
#		if(method_result %in% ls(envir=.GlobalEnv)){
#			eval(parse(text=paste("method_class <- class(",method_result,")",sep="")))
#			if(method_class=="Biclust"){
#				extra.arg <- ",mname='biclust'"
#			}
#		}
#	}
#	if(methodname %in% isa.names){
#		extra.arg <- ",mname='isa2'"
#	}
#	
#	if(methodname %in% bicare.names){
#		extra.arg <- ",mname='bicare'"
#	}
	
	#############################
	#############################
	
	method_result <- gsub(" ","",methodname,fixed=TRUE)
	method_result <- gsub("-","",method_result,fixed=TRUE)	
	
	# Special Case names
	fabia.names <- c("Fabia Laplace Prior","Fabia Post-Projection","Fabia Sparseness Projection","Fabia SPARSE")
	
	## Some dummy parameters in the case "Show Results" has not been pressed yet. The buttons itself will throw an error and will not execute so it does not matter what is here
	if(!(method_result %in% ls(envir=.GlobalEnv))){
		if(methodname %in% fabia.names){
			bcdiag.fabia <- TRUE
			extra.arg <- paste0(method_result,",mname='fabia'")
		}
		else{
			bcdiag.fabia <- FALSE
			extra.arg <- paste0(method_result,",mname='biclust'")
		}
	}
	## Special Case: Fabia  (Because of the need for thresholds)
	else if(methodname %in% fabia.names){
		eval(parse(text=paste("method_class <- class(",method_result,")",sep=""))) # Can only do it here and not earlier, cause not sure of the object exists at this point
		if(method_class == "Factorization" ){
			bcdiag.fabia <- TRUE
			extra.arg <- paste0(method_result,",mname='fabia'")
			
		}
		else{ # This handles the case if superbiclust has been applied to Fabia object
			bcdiag.fabia <- FALSE
			result_temp <- .tobiclust_transf(method_result)
			extra.arg <- paste0(result_temp,",mname='biclust'")
		}
	}
	## General Case
	else{
		bcdiag.fabia <- FALSE
		result_temp <- .tobiclust_transf(method_result)
		extra.arg <- paste0(result_temp,",mname='biclust'")
	}
	
	###############################################################################################################################################################################
	## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
	#####################################################
	
	
	toolname <- "BCDIAG"
	
	toolhelp <- "BcDiag-package" 
	
	
	# Do not change this line:
	input <- "plotdiagTab"
	
	#######################
	## MAKING THE WINDOW ##
	#######################
	
	### ADDING FRAMES ####
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "entry_highlight"  
	argument.names <- c("Higlight Genes","Highlight Conditions") 
	argument.types <- c("num","num")
	arguments <- c("gene.lines","condition.lines")
	initial.values <- c("c()","c()")
	title <- "Higlights for Line Plot"
	border <- FALSE
	entry.width <- c("8","8")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	
	####					 ####
	## Fabia Thresholds Frames ##
	####                     ####
	if(bcdiag.fabia==TRUE){
		######		  ENTRY FIELDS FRAME 				#####
		#							    		 			#
		
		type <- "entryfields"
		
		# Change variables accordingly:
		frame.name <- "fabia1"  
		argument.names <- c("Fabia ThresZ") 
		argument.types <- c("num")
		arguments <- c("fabia.thresZ")
		initial.values <- c(0.5)
		title <- ""
		border <- FALSE
		entry.width <- c("5")  
		
		# Do not change this line:
		new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
		
		type <- "entryfields"
		
		# Change variables accordingly:
		frame.name <- "fabia2"  
		argument.names <- c("Fabia ThresL") 
		argument.types <- c("num")
		arguments <- c("fabia.thresL")
		initial.values <- c("NULL")
		title <- ""
		border <- FALSE
		entry.width <- c("5")  
		
		# Do not change this line:
		new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
		
	}
 	
	
	####						 ####
	##	The anomedOnlybic Function ##
	####						 ####
	
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "radio_anomed"
	argument.names <- c("Diagnostic Plots","Tukey Additivity Plot","Anova Plots","Mpolish Plots","Anova & Mpolish")  
	arguments <- c("fit")		
	argument.values <- c("aplot","mplot","anovbplot","mpolishbplot","boxplot") 
	argument.types <- "char"
	initial.values <- "boxplot" 
	title <- "Plot Type:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	
	######		  ENTRY FIELDS FRAME 				#####
	#							    		 			#
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "entry_number"  
	argument.names <- c("Bicluster Number") 
	argument.types <- c("num")
	arguments <- c("bnum")
	initial.values <- c(1)
	title <- ""
	border <- FALSE
	entry.width <- c("3")  
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button_anomed"  
	button.name <- "Draw Plot"  
	button.function <- "anomedOnlybic" 
	button.data <- "dset" 
	button.biclust <-  "" 
	save <- FALSE
	button.otherarg <- paste0(",bres=",extra.arg)
	
	if(bcdiag.fabia==TRUE){
		arg.frames <- c("radio_anomed","entry_number","fabia1","fabia2") 
	}
	else{
		arg.frames <- c("radio_anomed","entry_number") 
	}
	
	
	
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
	####													   ####
	##	    EXPLORATORY PLOTS (biclustered & Clustered Data)     ##
	####						 							   ####
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "radio1_explore"
	argument.names <- c("Mean","Median","Variance","Median Absolute Deviation","Quantile","All")  
	arguments <- c("pfor")		
	argument.values <- c("mean","median","variance","mad","quant","all") 
	argument.types <- "char"
	initial.values <- "mean" 
	title <- "Plot for:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "radio2_explore"
	argument.names <- c("Genes","Conditions")  
	arguments <- c("gby")		
	argument.values <- c("genes","conditions") 
	argument.types <- "char"
	initial.values <- "genes" 
	title <- "Dimension:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button_explore"  
	button.name <- "Bicl. & Clust."  
	button.function <- "exploreBic" 
	button.data <- "dset" 
	button.biclust <-  "" 
	button.otherarg <- paste0(",bres=",extra.arg)
	save <- FALSE
	if(bcdiag.fabia==TRUE){
		arg.frames <- c("radio1_explore","radio2_explore","entry_number","fabia1","fabia2") 
	}
	else{
		arg.frames <- c("radio1_explore","radio2_explore","entry_number") 
	}
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button2_explore"  
	button.name <- "Only Biclust."  
	button.function <- "exploreOnlybic" 
	button.data <- "dset" 
	button.biclust <-  "" 
	button.otherarg <- paste0(",bres=",extra.arg)
	save <- FALSE
	if(bcdiag.fabia==TRUE){
		arg.frames <- c("radio1_explore","radio2_explore","entry_number","fabia1","fabia2") 
	}
	else{
		arg.frames <- c("radio1_explore","radio2_explore","entry_number") 
	}
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	####					####
	##	    PROFILE PLOTS     ##
	####					####
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "radio1_profile"
	argument.names <- c("All","Lines","Boxplot","Histogram","3D")  
	arguments <- c("bplot")		
	argument.values <- c("all","lines","boxplot","histogram","threeD") 
	argument.types <- "char"
	initial.values <- "all" 
	title <- "Plot Type:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	
	####		RADIO BUTTONS FRAME  			####
	#                               			   #
	
	type <- "radiobuttons"
	
	# Change variables accordingly:
	frame.name <- "radio2_profile"
	argument.names <- c("Genes","Conditions")  
	arguments <- c("gby")		
	argument.values <- c("genes","conditions") 
	argument.types <- "char"
	initial.values <- "genes" 
	title <- "Dimension:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,argument.values=argument.values,initial.values=initial.values,title=title,border=border,new.frames=new.frames,argument.types=argument.types)	
	

	####		VALUE SLIDER FRAME - EXAMPLE 				  ####
	#                               							 #
	# Only for numerical values
	type <- "valuesliders"
	
	# Change variables accordingly:
	frame.name <- "slider_profile"
	argument.names <- c("Theta: ","Phi:     ") 
	arguments <- c("teta","ph")
	initial.values <- c(120,30)
	from <- c(-180,-180) 
	to <- c(180,180) 
	by <- c(10,10)  
	length <- c(125,125) 
	title <- "3D Rotation:"
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type,title=title,border=border,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,from=from,to=to,by=by,length=length,new.frames=new.frames)
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button_highlight"  
	button.name <- "Set Highlights"  
	button.function <- "bcdiaghighlight_WINDOW" 
	button.data <- "dset" 
	button.biclust <-  "" 
	button.otherarg <- paste0(",methodname='",methodname,"',bres=",extra.arg)
	button.width <- "14"
	save <- FALSE
	show <- FALSE
	if(bcdiag.fabia==TRUE){
		arg.frames <- c("entry_number","fabia1","fabia2") 
	}
	else{
		arg.frames <- c("entry_number") 
	}
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,show=show,button.width=button.width,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button_profile"  
	button.name <- "Draw Plot"  
	button.function <- "profileBic" 
	button.data <- "dset" 
	button.biclust <-  "" 
	button.otherarg <- paste0(",bres=",extra.arg)
	save <- FALSE
	if(bcdiag.fabia==TRUE){
		arg.frames <- c("radio1_profile","radio2_profile","entry_number","slider_profile","fabia1","fabia2","entry_highlight") 
	}
	else{
		arg.frames <- c("radio1_profile","radio2_profile","entry_number","slider_profile","entry_highlight") 
	}
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	####	    	MANUAL BUTTONS FRAME 			  ####
	#                               					 #
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "button_summaryoutput"  
	button.name <- "Summary"  
	button.function <- "bcdiagwrite_WINDOW" 
	button.data <- "" 
	button.biclust <-  ""
	
	temp <- paste("methodname='",methodname,"'",sep="")
	button.otherarg <- temp
	save <- FALSE
	show <- FALSE
	if(bcdiag.fabia==TRUE){
		arg.frames <- c("fabia1","fabia2") 	
	}
	else{
		arg.frames <- c() 		
	}

	
	# Do not change this line:
	new.frames <- .add.frame(input=input,frame.name=frame.name,show=show,save=save,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
	
	
	
	
	### CONFIGURING GRID ###
	
	if(bcdiag.fabia==TRUE){
		grid.config <- .grid.matrix(input=input,c("entry_number","fabia1","fabia2","button_summaryoutput","radio_anomed","button_anomed",NA,NA,"radio1_explore","radio2_explore",NA,NA,"button_explore","button2_explore",NA,NA,"radio1_profile","radio2_profile","slider_profile",NA,"button_profile","button_highlight","entry_highlight",NA),byrow=TRUE,nrow=6,ncol=4,grid.config=grid.config)
	}
	else{
		grid.config <- .grid.matrix(input=input,c("entry_number","button_summaryoutput",NA,"radio_anomed","button_anomed",NA,"radio1_explore","radio2_explore",NA,"button_explore","button2_explore",NA,"radio1_profile","radio2_profile","slider_profile","button_profile","button_highlight","entry_highlight"),byrow=TRUE,nrow=6,ncol=3,grid.config=grid.config)
	}
	
	
	### COMBINING ROWS ###
	grid.rows <- .combine.rows(input=input,rows=c(1),title="Plotting Cluster Number & Summary Output",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(2),title="ANOVA & Median Polish Residual Plots",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(3,4),title="Exploratory Plots for Biclustered & Clustered data",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(5,6),title="Profile Plots for Biclustered & Clustered data",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	##################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL NEW TOOL FUNCTION ##
	##################################################################
	
	newtool_template(toolname=toolname,methodname=methodname,toolhelp=toolhelp,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames)
	
	
}
