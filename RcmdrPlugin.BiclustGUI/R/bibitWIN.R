# Project: Master Thesis
# 
# Author: Gebruiker
###############################################################################


# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


bibit_WIN <- function(){     # Change newmethod to your own method name
  
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
  methodname <- "BiBit"
  
  # Define the function (as it is named in your package)
  # Note: If you have got a support function which is used for iterations, use it in this 'mainfunction'
  methodfunction <- "bibit2"
  
  
  # Define the name of the data argument for your function
  data.arg <- "matrix"
  
  
  # Define any other arguments in the function, which should not be changed by the user.
  # These arguments may also include a certain method for your function, since it is the idea to give each method a separate window.
  other.arg <- ""  # Comma in the beginning but not at the end !
  
  # Help Object
  methodhelp <- "bibit2"
  
  # Possibility to give a seed ?
  methodseed <- FALSE
  
  # Add a discretize box?
  data.discr <- FALSE
  
  # Add a binarize box?
  data.bin <- TRUE
  
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
  
  
  ######		  ENTRY FIELDS FRAME 				#####
  #							    		 			#
  
  type <- "entryfields"
  
  # Change variables accordingly:
  frame.name <- "entryframe1"  
  argument.names <- c("Minimum Row Size","Minimum Column Size","Noise") 
  argument.types <- c("num","num","num")
  arguments <- c("minr","minc","noise")
  initial.values <- c(2,2,0)
  title <- ""
  border <- FALSE
  entry.width <- c("4","4","4")  
  
  
  # Do not change this line:
  new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
  
  
  ###############################################################################################################################################################################
  ## CONFIGURATION OF GRID OF FRAMES - CLUSTERTAB ##
  ##################################################
  
  #########################
  #### THE GRID MATRIX ####
  #########################
  
  grid.config <- .grid.matrix(input=input,c("entryframe1"),byrow=TRUE,nrow=1,ncol=1,grid.config=grid.config)
  
  
  ####################################
  #### COMBINING ROWS -CLUSTERTAB ####
  ####################################
  
  grid.rows <- .combine.rows(input=input,rows=c(1),title="BiBit Specifications",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)

  
  
  ###############################################################################################################################################################################
  ###############################################################################################################################################################################
  
  
  
  
  ####################################
  #### PLOTTING & DIAGNOSTICS TAB ####
  ####################################
  
  input <- "plotdiagTab"
  
  
  ######		  ENTRY FIELDS FRAME 				#####

  type <- "entryfields"
  
  # Change variables accordingly:
  frame.name <- "colnoiseentryframe"  
  argument.names <- c("Bicluster Number") 
  argument.types <- c("num")
  arguments <- c("BC")
  initial.values <- c(1)
  title <- ""
  border <- FALSE
  entry.width <- c("4")  
  
  # Do not change this line:
  new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
  
  
  ####	    	MANUAL BUTTONS FRAME 			  ####
  #                               					 #
  
  type <- "buttons"
  
  # Change variables accordingly:
  frame.name <- "colnoisebutton"  
  button.name <- "Draw Plot"  
  button.function <- "ColNoiseBC.GUI" 
  button.data <- "" 
  button.biclust <-  "result"
  button.otherarg <- ""
  save <- FALSE
  show <- FALSE
  arg.frames <- c("colnoiseentryframe") 
  
  
  # Do not change this line:
  new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,show=show,type=type,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
  
  ###############################################################
  
  
  
  
  
  
  ####	    	MANUAL BUTTONS FRAME 			  ####
  #                               					 #
  
  type <- "buttons"
  
  # Change variables accordingly:
  frame.name <- "bibitworkflow"  
  button.name <- "BiBit Workflow"  
  button.function <- "bibitworkflow_WIN" 
  button.data <- "" 
  button.biclust <-  ""
  button.width <- "16"
  button.otherarg <- "methodname='BiBit'"
  save <- FALSE
  show <- FALSE
  
  arg.frames <- c() 
  

  # Do not change this line:
  new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,show=show,type=type,button.width=button.width,button.name=button.name,button.otherarg=button.otherarg,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
  
  ###############################################################
  
  
  
  
  
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
  button.name <- "MaxBC Top"  
  button.function <- "MaxBC" 
  button.data <- "" 
  button.biclust <-  "result" 
  button.otherarg <- ",top=1"
  arg.frames <- c()
  save <- FALSE
  
  # Do not change this line: (without button.otherarg)
  new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,type=type,button.otherarg = button.otherarg,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
  
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
  
  
  ###############################################################################################################################################################################
  ## CONFIGURATION OF GRID OF FRAMES - PLOTDIAGTAB ##
  ###################################################
  
  
  #########################
  #### THE GRID MATRIX ####
  #########################
  
  
  grid.config <- .grid.matrix(input=input,c("summarybuttonframe","fstatentryframe","fstatbuttonframe",
                                            "bootstrapentryframe",NA,NA,
                                            "bootstrapreplacementframe","bootstrapbuttonframe","bootstrapvisualbuttonframe" ,
                                            "pplottypeframe","pplotentryframe","parallelbuttonframe",
                                            "pplotcheckframe",NA,NA,
                                            "heatplotcheckframe","heatplotentryframe","heatbuttonframe",
                                            "mplotcheckframe","mplotentryframe","memberbuttonframe",
                                            "colnoiseentryframe","colnoisebutton",NA,
                                            "bibitworkflow",NA,NA),
                              byrow=TRUE,nrow=9,ncol=3,grid.config=grid.config)
  
  
  
  ########################
  #### COMBINING ROWS ####
  ########################
  
  grid.rows <- .combine.rows(input=input,rows=c(1,2,3),title="Summary & Diagnostics",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
  grid.rows <- .combine.rows(input=input,rows=c(4,5),title="Parallel Coordinate Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
  grid.rows <- .combine.rows(input=input,rows=c(6),title="Heatmap Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
  grid.rows <- .combine.rows(input=input,rows=c(7),title="Biclustmember Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
  grid.rows <- .combine.rows(input=input,rows=c(8),title="Column Noise Plot",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
  grid.rows <- .combine.rows(input=input,rows=c(9),title="BiBit Workflow (Note: Only for zero noise BiBit)",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
  
  
  
  #########################################################################
  ## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL CLUSTERTEMPLATE FUNCTION ##
  #########################################################################
  
  cluster_template(methodname=methodname,methodfunction=methodfunction,methodhelp=methodhelp,data.arg=data.arg,other.arg=other.arg,methodseed=methodseed,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames,superbiclust.comp=superbiclust.comp,bcdiag.comp=bcdiag.comp,data.discr=data.discr,data.bin=data.bin,extrabiclustplot=extrabiclustplot)
  
}






















bibitworkflow_WIN <- function(methodname){  
  
  globalobjects <- ls(envir=.GlobalEnv)
  
  
  if(!("BiBit" %in% globalobjects)){
    stop("Apply BiBit First",call. = FALSE)
  }
  
  if(get("BiBit",envir=.GlobalEnv)@Parameters$Call$noise!=0){stop("Apply BiBit without noise!",call. = FALSE)}  
  
  
  new.frames <- .initialize.new.frames()
  grid.config <- .initialize.grid.config()
  grid.rows <- .initialize.grid.rows()
  
  
  #####################################################
  ## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
  #####################################################
  
  toolname <- "BiBit Workflow"
  
  toolhelp <- "BiBitWorkflow"
  
  
  # Do not change this line:
  input <- "plotdiagTab"
  
  #######################
  ## MAKING THE WINDOW ##
  #######################
  
  ### ADDING FRAMES ####
  
  
  #### STEP 1 ####
  
  
  #### RADIO BUTTONS FRAME 	####
  
  type <- "radiobuttons"
  
  # Change variables accordingly:
  frame.name <- "similaritytype"
  argument.names <- c("Column (Recommended!)","Row & Column")  
  arguments <- c("similarity_type")		
  argument.types <- "char" 
  argument.values <- c("col","both") 
  initial.values <- "col"
  title <- "Similarity Type"
  border <- FALSE
  
  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,argument.values=argument.values
                           ,initial.values=initial.values,title=title,border=border
                           ,new.frames=new.frames,argument.types=argument.types)	
  
  
  
  
  #### RADIO BUTTONS FRAME 	####
  
  type <- "radiobuttons"
  
  # Change variables accordingly:
  frame.name <- "initialcuttingtype"
  argument.names <- c("Number","Dissimilarity")  
  arguments <- c("cut_type")		
  argument.types <- "char" 
  argument.values <- c("number","height") 
  initial.values <- "number"
  title <- "Initial Cutting Type"
  border <- FALSE
  
  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,argument.values=argument.values
                           ,initial.values=initial.values,title=title,border=border
                           ,new.frames=new.frames,argument.types=argument.types)	
  
  
  
  #### ENTRY FIELDS FRAME ####
  
  type <- "entryfields"
  
  # Change variables accordingly:
  frame.name <- "initialcuttingpm"  
  argument.names <- c("Initial Cutting Parameter") 
  argument.types <- c("num") 
  arguments <- c("cut_pm") 
  initial.values <- c(10)
  title <- ""
  border <- FALSE
  entry.width <- c("4")
  
  # Do not change this line:
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,entry.width=entry.width
                           ,argument.types=argument.types  ,new.frames=new.frames)
  
  
  #### ENTRY FIELDS FRAME ####
  
  type <- "entryfields"
  
  # Change variables accordingly:
  frame.name <- "initialnoise"  
  argument.names <- c("Initial Noise Level") 
  argument.types <- c("num") 
  arguments <- c("noise") 
  initial.values <- c(0.1)
  title <- "Growing Rows"
  border <- FALSE
  entry.width <- c("4")
  
  # Do not change this line:
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,entry.width=entry.width
                           ,argument.types=argument.types  ,new.frames=new.frames)
  
  

  
  #### MANUAL BUTTONS FRAME ####
  
  type <- "buttons"
  
  # Change variables accordingly:
  frame.name <- "workflow"  
  button.name <- "BiBitWorkflow Result"  
  button.function <- "BiBitWorkflow.GUI" 
  button.data <- "matrix" 
  button.biclust <-  "BCresult" 
  button.width <- "20"
  button.data.transf <- "matrix"
  
  arg.frames <- c("similaritytype","initialcuttingtype","initialcuttingpm","initialnoise")
  
  save <- FALSE 
  show <- FALSE
  button.otherarg <- ""
  
  # Do not change this line: 
  new.frames <- .add.frame(input=input,frame.name=frame.name,
                           type=type,button.name=button.name,
                           button.function=button.function,button.data=button.data,
                           button.biclust=button.biclust,button.otherarg=button.otherarg,
                           button.width=button.width,button.data.transf=button.data.transf,
                           arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
  
  #### STEP 2 ####
  
  
  
  #### MANUAL BUTTONS FRAME ####
  
  type <- "buttons"
  
  # Change variables accordingly:
  frame.name <- "tree"  
  button.name <- "Dendrogram"  
  button.function <- "BiBitDendrogram.GUI" 
  button.data <- "matrix" 
  button.biclust <-  "BCresult" 
  button.width <- "20"
  button.data.transf <- "matrix"
  
  arg.frames <- c("dendrocuttingtype","dendrocuttingpm")
  
  save <- FALSE 
  show <- FALSE
  button.otherarg <-""
  
  # Do not change this line: 
  new.frames <- .add.frame(input=input,frame.name=frame.name,
                           type=type,button.name=button.name,
                           button.function=button.function,button.data=button.data,
                           button.biclust=button.biclust,button.otherarg=button.otherarg,
                           button.width=button.width,button.data.transf=button.data.transf,
                           arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
  
  
  
  
  #### RADIO BUTTONS FRAME 	####
  
  type <- "radiobuttons"
  
  # Change variables accordingly:
  frame.name <- "dendrocuttingtype"
  argument.names <- c("Number","Dissimilarity")  
  arguments <- c("cut_type")		
  argument.types <- "char" 
  argument.values <- c("number","height") 
  initial.values <- "number"
  title <- "Dendrogram Cutting Type"
  border <- FALSE
  
  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,argument.values=argument.values
                           ,initial.values=initial.values,title=title,border=border
                           ,new.frames=new.frames,argument.types=argument.types)	
  
  
  
  #### ENTRY FIELDS FRAME ####
  
  type <- "entryfields"
  
  # Change variables accordingly:
  frame.name <- "dendrocuttingpm"  
  argument.names <- c("Dendrogram Cutting Parameter") 
  argument.types <- c("num") 
  arguments <- c("cut_pm") 
  initial.values <- c(10)
  title <- ""
  border <- FALSE
  entry.width <- c("4")
  
  # Do not change this line:
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,entry.width=entry.width
                           ,argument.types=argument.types  ,new.frames=new.frames)
  
  
  
  
  #### ENTRY FIELDS FRAME ####
  
  type <- "entryfields"
  
  # Change variables accordingly:
  frame.name <- "maxClusterframe"  
  argument.names <- c("Max Clusters","Growing Rows Noise") 
  argument.types <- c("num","num") 
  arguments <- c("maxCluster","noise") 
  initial.values <- c(20,0.1)
  title <- "Cluster Row Coverage Options"
  border <- FALSE
  entry.width <- c("4","4")
  
  # Do not change this line:
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,entry.width=entry.width
                           ,argument.types=argument.types  ,new.frames=new.frames)
  
  
  
  
  
  #### MANUAL BUTTONS FRAME ####
  
  type <- "buttons"
  
  # Change variables accordingly:
  frame.name <- "rowcov"  
  button.name <- "Cluster Row Coverage"  
  button.function <- "ClusterRowCoverage.GUI" 
  button.data <- "matrix" 
  button.biclust <-  "" 
  button.width <- "20"
  button.data.transf <- "matrix"
  
  arg.frames <- c("maxClusterframe")
  
  save <- FALSE 
  show <- FALSE
  button.otherarg <- ""
  
  # Do not change this line: 
  new.frames <- .add.frame(input=input,frame.name=frame.name,
                           type=type,button.name=button.name,
                           button.function=button.function,button.data=button.data,
                           button.biclust=button.biclust,button.otherarg=button.otherarg,
                           button.width=button.width,button.data.transf=button.data.transf,
                           arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
  
  
  
  #### STEP 3 ####
  
  #### RADIO BUTTONS FRAME 	####
  
  type <- "radiobuttons"
  
  # Change variables accordingly:
  frame.name <- "updatecuttingtype"
  argument.names <- c("Number","Dissimilarity")  
  arguments <- c("cut_type")		
  argument.types <- "char" 
  argument.values <- c("number","height") 
  initial.values <- "number"
  title <- "Cutting Type"
  border <- FALSE
  
  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,argument.values=argument.values
                           ,initial.values=initial.values,title=title,border=border
                           ,new.frames=new.frames,argument.types=argument.types)	
  
  
  
  #### ENTRY FIELDS FRAME ####
  
  type <- "entryfields"
  
  # Change variables accordingly:
  frame.name <- "updatecuttingpm"  
  argument.names <- c("Cutting Parameter") 
  argument.types <- c("num") 
  arguments <- c("cut_pm") 
  initial.values <- c(10)
  title <- ""
  border <- FALSE
  entry.width <- c("4")
  
  # Do not change this line:
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,entry.width=entry.width
                           ,argument.types=argument.types  ,new.frames=new.frames)
  
  
  #### ENTRY FIELDS FRAME ####
  
  type <- "entryfields"
  
  # Change variables accordingly:
  frame.name <- "updatenoise"  
  argument.names <- c("Noise Level") 
  argument.types <- c("num") 
  arguments <- c("noise") 
  initial.values <- c(0.1)
  title <- "Growing Rows"
  border <- FALSE
  entry.width <- c("4")
  
  # Do not change this line:
  new.frames <- .add.frame(input=input,type=type
                           ,frame.name=frame.name,argument.names=argument.names
                           ,arguments=arguments,initial.values=initial.values
                           ,title=title,border=border,entry.width=entry.width
                           ,argument.types=argument.types  ,new.frames=new.frames)
  
  
  
  
  
  #### MANUAL BUTTONS FRAME ####
  
  type <- "buttons"
  
  # Change variables accordingly:
  frame.name <- "updatebutton"  
  button.name <- "Update"  
  button.function <- "Update.GUI" 
  button.data <- "matrix" 
  button.biclust <-  "" 
  button.width <- "16"
  button.data.transf <- "matrix"
  
  arg.frames <- c("updatecuttingtype","updatecuttingpm","updatenoise")
  
  save <- FALSE 
  show <- FALSE
  button.otherarg <- ""
  
  # Do not change this line: 
  new.frames <- .add.frame(input=input,frame.name=frame.name,
                           type=type,button.name=button.name,
                           button.function=button.function,button.data=button.data,
                           button.biclust=button.biclust,button.otherarg=button.otherarg,
                           button.width=button.width,button.data.transf=button.data.transf,
                           arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
  
  
  
  #### STEP 4 ####
  
  
  
  #### MANUAL BUTTONS FRAME ####
  
  type <- "buttons"
  
  # Change variables accordingly:
  frame.name <- "summarybutton"  
  button.name <- "Summary"  
  button.function <- "summaryworkflow.GUI" 
  button.data <- "matrix" 
  button.biclust <-  "" 
  button.width <- "14"
  button.data.transf <- "matrix"
  
  arg.frames <- c()
  
  save <- FALSE 
  show <- FALSE
  button.otherarg <- ""
  
  # Do not change this line: 
  new.frames <- .add.frame(input=input,frame.name=frame.name,
                           type=type,button.name=button.name,
                           button.function=button.function,button.data=button.data,
                           button.biclust=button.biclust,button.otherarg=button.otherarg,
                           button.width=button.width,button.data.transf=button.data.transf,
                           arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
  
  
  #### MANUAL BUTTONS FRAME ####
  
  type <- "buttons"
  
  # Change variables accordingly:
  frame.name <- "savebutton"  
  button.name <- "Save to BiBit"  
  button.function <- "saveworkflow.GUI" 
  button.data <- "matrix" 
  button.biclust <-  "" 
  button.width <- "20"
  button.data.transf <- "matrix"
  
  arg.frames <- c()
  
  save <- FALSE 
  show <- FALSE
  button.otherarg <- ""
  
  # Do not change this line: 
  new.frames <- .add.frame(input=input,frame.name=frame.name,
                           type=type,button.name=button.name,
                           button.function=button.function,button.data=button.data,
                           button.biclust=button.biclust,button.otherarg=button.otherarg,
                           button.width=button.width,button.data.transf=button.data.transf,
                           arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
  
  #### MANUAL BUTTONS FRAME ####
  
  type <- "buttons"
  
  # Change variables accordingly:
  frame.name <- "resetbutton"  
  button.name <- "Reset"  
  button.function <- "resetworkflow.GUI" 
  button.data <- "matrix" 
  button.biclust <-  "" 
  button.width <- "20"
  button.data.transf <- "matrix"
  
  arg.frames <- c()
  
  save <- FALSE 
  show <- FALSE
  button.otherarg <- ""
  
  # Do not change this line: 
  new.frames <- .add.frame(input=input,frame.name=frame.name,
                           type=type,button.name=button.name,
                           button.function=button.function,button.data=button.data,
                           button.biclust=button.biclust,button.otherarg=button.otherarg,
                           button.width=button.width,button.data.transf=button.data.transf,
                           arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
  
  
  
  
  ### CONFIGURING GRID ###
  grid.config <- .grid.matrix(input=input,c("similaritytype",NA,
                                            "initialcuttingtype","initialnoise",
                                            "initialcuttingpm",NA,
                                            "workflow",NA,
                                            "dendrocuttingtype",NA,
                                            "dendrocuttingpm","tree",
                                            "maxClusterframe","rowcov",
                                            "updatecuttingtype","updatenoise",
                                            "updatecuttingpm","updatebutton",
                                            "summarybutton",NA,
                                            "savebutton","resetbutton"
                                            ),
                              
                              nrow=11,ncol=2,byrow=TRUE,grid.config=grid.config)
  
  
  

  ### COMBINING ROWS ###
  grid.rows <- .combine.rows(input=input,rows=c(1,2,3,4),title="Step 1 - Initial BiBitWorkflow",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
  grid.rows <- .combine.rows(input=input,rows=c(5,6,7),title="Step 2 - Plots",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
  grid.rows <- .combine.rows(input=input,rows=c(8,9),title="Step 3 (Optional) - Update BiBitWorkflow Result with different cut/rowgrowth",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
  grid.rows <- .combine.rows(input=input,rows=c(10,11),title="Step 4 - Summary & Save to BiBit Result",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
  
  
  ############################################################
  ## USE ALL THE ARGUMENTS IN THE GENERAL NEW TOOL FUNCTION ##
  ############################################################
  
  newtool_template(toolname=toolname,methodname=methodname,toolhelp=toolhelp,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames)
  
  
}




BiBitWorkflow.GUI <- function(matrix,BCresult,similarity_type,cut_type,cut_pm,noise){
  
  if(tclvalue(get("bin.checkVariable",envir=.GetEnvBiclustGUI("biclustering.objects")$ENVIR$BiBit))=="1"){
    matrixname <- "x"
  }else{
    matrixname <- ActiveDataSet()
  }
  
  doItAndPrint(paste0("BiBitWorkflowResult <- BiBitWorkflow(matrix=",matrixname,",BCresult=BiBit,similarity_type='",similarity_type,"',cut_type='",cut_type,"',cut_pm=",cut_pm,",noise=",noise,",plots=c())"))
  
}


BiBitDendrogram.GUI <- function(matrix,BCresult,cut_type,cut_pm){
  if(tclvalue(get("bin.checkVariable",envir=.GetEnvBiclustGUI("biclustering.objects")$ENVIR$BiBit))=="1"){
    matrixname <- "x"
  }else{
    matrixname <- ActiveDataSet()
  }
  
  doItAndPrint(paste0("dendro_temp <- BiBitWorkflow(matrix=",matrixname,",BCresult=BiBit,cut_type='",cut_type,"',cut_pm=",cut_pm,",noise=0,plots=3,simmatresult=BiBitWorkflowResult$info$BiclustSimInitial,treeresult=BiBitWorkflowResult$info$Tree,verbose=FALSE,plot.type='other')"))
  
}


ClusterRowCoverage.GUI <- function(matrix,maxCluster,noise){
  if(tclvalue(get("bin.checkVariable",envir=.GetEnvBiclustGUI("biclustering.objects")$ENVIR$BiBit))=="1"){
    matrixname <- "x"
  }else{
    matrixname <- ActiveDataSet()
  }
  
  doItAndPrint(paste0("ClusterRowCoverage(result=BiBitWorkflowResult,matrix=",matrixname,",maxCluster=",maxCluster,",noise=",noise,",plots=2,plot.type='other')"))
  
}


Update.GUI <- function(matrix,cut_type,cut_pm,noise){
  if(tclvalue(get("bin.checkVariable",envir=.GetEnvBiclustGUI("biclustering.objects")$ENVIR$BiBit))=="1"){
    matrixname <- "x"
  }else{
    matrixname <- ActiveDataSet()
  }
  
  doItAndPrint(paste0("BiBitWorkflowResult <- BiBitWorkflow(matrix=",matrixname,",BCresult=BiBit,cut_type='",cut_type,"',cut_pm=",cut_pm,",noise=",noise,",plots=c(),simmatresult=BiBitWorkflowResult$info$BiclustSimInitial,treeresult=BiBitWorkflowResult$info$Tree)"))
}

summaryworkflow.GUI <- function(matrix){
  doItAndPrint(paste0("summary(BiBitWorkflowResult$Biclust)"))
}


saveworkflow.GUI <- function(matrix){
  doItAndPrint(paste0("BiBit_Original <- BiBit"))
  doItAndPrint(paste0("BiBit <- BiBitWorkflowResult$Biclust"))
  
}

resetworkflow.GUI <- function(matrix){
  doItAndPrint(paste0("BiBit <- BiBit_Original"))
}


ColNoiseBC.GUI <- function(result,BC){
  if(tclvalue(get("bin.checkVariable",envir=.GetEnvBiclustGUI("biclustering.objects")$ENVIR$BiBit))=="1"){
    matrixname <- "x"
  }else{
    matrixname <- ActiveDataSet()
  }
  doItAndPrint(paste0("ColNoiseBC(result=BiBit,matrix=",matrixname,",BC=",BC,",plot.type='other')"))
}


