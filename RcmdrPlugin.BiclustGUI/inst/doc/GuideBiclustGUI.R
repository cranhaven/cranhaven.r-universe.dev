## ----newmethod_script1,echo=TRUE,eval=FALSE-----------------------------------
#  newmethod_WINDOW <- function(){
#  	
#  	new.frames <- .initialize.new.frames()
#  	grid.config <- .initialize.grid.config()
#  	grid.rows <- .initialize.grid.rows()
#  		
#  	#####################################################
#  	## GENERAL INFORMATION ABOUT THE NEW METHOD/WINDOW ##
#  	#####################################################
#  	
#  	methodname <- "A new method"
#  	
#  	methodfunction <- "methodfunction"
#  	data.arg <- "d"
#  	methodshow <- TRUE
#  	methodsave <- TRUE
#  	other.arg <- ""
#  	methodhelp <- ""
#  
#  	# Transform the data from data.arg
#  	data.transf <- "matrix" # Values: "matrix" (default), "ExprSet"
#  	
#  	# Extra Data Conversion Boxes
#  	data.discr <- FALSE
#  	data.bin <- FALSE
#  	
#  	# Possibility to give a seed ?
#  	methodseed <- TRUE
#  	
#  	## COMPATIBILITY? ##
#  	
#  	# BcDiag
#  	bcdiag.comp <- FALSE
#  	
#  	# SuperBiclust
#  	superbiclust.comp <- FALSE
#  
#  	
#  } # Note: The curly bracket is placed here for syntax reasons.
#    #       It should be placed after the call of cluster_template.

## ----newmethod_script2,eval=FALSE,echo=TRUE-----------------------------------
#  ########################
#  #### CLUSTERING TAB ####
#  ########################
#  
#  input <- "clusterTab"
#  
#  ### 1. ADDING THE FRAMES ###
#  
#  # Add frames here
#  
#  ### 2. CONFIGURING THE GRID ###
#  
#  grid.config <- .grid.matrix(input=input,c("frame1","frame2","frame3",NA,"frame4",NA),
#  		    nrow=3,ncol=2,byrow=TRUE,grid.config=grid.config)
#  
#  
#  ### 3. COMBING THE ROWS ###
#  
#  grid.rows <- .combine.rows(input=input,rows=c(1),title="A nice box: ",
#  		    border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
#  grid.rows <- .combine.rows(input=input,rows=c(2,3),title="A nice box: ",
#  		    border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
#  

## ----newmethod_script3,eval=FALSE,echo=TRUE-----------------------------------
#  ####################################
#  #### PLOTTING & DIAGNOSTICS TAB ####
#  ####################################
#  { # ignore this bracket
#  	
#  input <- "plotdiagTab"
#  
#  ### 1. ADDING THE FRAMES ###
#  
#  # Add frames here
#  
#  ### 2. CONFIGURING THE GRID ###
#  
#  grid.config <- .grid.matrix(input=input,c("frame5","frame6"),nrow=1,ncol=2,
#     byrow=TRUE,grid.config=grid.config)
#  
#  ### 3. COMBING THE ROWS ###
#  
#  grid.rows <- .combine.rows(input=input,rows=c(1),title="Plot 1",border=TRUE,
#     grid.rows=grid.rows,grid.config=grid.config)
#  
#  ###################################################################
#  ## USE ALL THE ARGUMENTS IN THE GENERAL CLUSTERTEMPLATE FUNCTION ##
#  ###################################################################
#  
#  cluster_template(methodname=methodname,methodfunction=methodfunction,
#     methodhelp=methodhelp,data.arg=data.arg,other.arg=other.arg,
#     methodseed=methodseed,grid.config=grid.config,grid.rows=grid.rows,
#     new.frames=new.frames,superbiclust.comp=superbiclust.comp,
#     bcdiag.comp=bcdiag.comp,data.transf=data.transf,
#     data.discr=data.discr,data.bin=data.bin,methodshow=methodshow,
#     methodsave=methodsave)
#  }

## ----plaid_script1,eval=FALSE,echo=TRUE---------------------------------------
#  #####################################################
#  ## GENERAL INFORMATION ABOUT THE NEW METHOD/WINDOW ##
#  #####################################################
#  
#  methodname <- "Plaid"
#  methodfunction <- "biclust"
#  data.arg <- "x"
#  other.arg <- ",method=BCPlaid()"
#  methodhelp <- "BCPlaid"
#  methodseed <- TRUE
#  data.discr <- FALSE
#  data.bin <- FALSE
#  bcdiag.comp <- TRUE
#  superbiclust.comp <- TRUE
#  
#  # Biclust only (Not for public use)
#  extrabiclustplot <- TRUE

## ----plaid_script2,eval=FALSE,echo=TRUE---------------------------------------
#  ### 2. CONFIGURING THE GRID ###
#  grid.config <- .grid.matrix(input=input,c("toclusterframe","modelframe",
#     "backgroundcheckframe",NA,"backgroundentryframe1","backgroundentryframe2"),
#     byrow=TRUE,nrow=3,ncol=2,grid.config=grid.config)
#  
#  ### 3. COMBING THE ROWS ###
#  grid.rows <- .combine.rows(input=input,rows=c(1),title="Plaid Specifications",
#     border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
#  grid.rows <- .combine.rows(input=input,rows=c(2,3),title="Layer Specifications",
#     border=TRUE,grid.rows=grid.rows,grid.config=grid.config)

## ----newtool_script,eval=FALSE,echo=TRUE--------------------------------------
#  newtool_WINDOW <- function(methodname){
#  	
#  	new.frames <- .initialize.new.frames()
#  	grid.config <- .initialize.grid.config()
#  	grid.rows <- .initialize.grid.rows()
#  		
#  	#####################################################
#  	## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
#  	#####################################################
#  	
#  	toolname <- "A new tool"
#  	toolhelp <- "helppage"
#  	
#  	# Do not change this line:
#  	input <- "plotdiagTab"
#  	
#  	### ADDING FRAMES ####
#  	
#  	# Analogous to plotdiag tab.
#  		
#  	### CONFIGURING GRID ###
#  	grid.config <- .grid.matrix(input=input,c(),nrow=1,ncol=2,
#  	   byrow=TRUE,grid.config=grid.config)
#  	
#  	
#  	### COMBINING ROWS ###
#  	grid.rows <- .combine.rows(input=input,rows=c(),title="Plot 1",
#  	   border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
#  	
#  	############################################################
#  	## USE ALL THE ARGUMENTS IN THE GENERAL NEW TOOL FUNCTION ##
#  	############################################################
#  	
#  	newtool_template(toolname=toolname,methodname=methodname,
#  	   toolhelp=toolhelp,grid.config=grid.config,
#  	   grid.rows=grid.rows,new.frames=new.frames)
#  }

## ----bcdiag_script,eval=FALSE,echo=TRUE---------------------------------------
#  bcdiag_WINDOW <- function(methodname){
#  	
#  	new.frames <- .initialize.new.frames()
#  	grid.config <- .initialize.grid.config()
#  	grid.rows <- .initialize.grid.rows()
#  		
#  	# Some extra code to determine the input type: "biclust", "fabia", "isa2"
#  	biclust.names <- c("Bimax","CC","Plaid","Questmotif","Spectral",
#  	   "XMotifs","IBBIG")
#  	fabia.names <- c("Fabia Laplace Prior","Fabia Post-Projection",
#  	    "Fabia Sparseness Projection","Fabia SPARSE")
#  	isa.names <- c("ISA")
#  	
#  	if(methodname %in% biclust.names){
#  		extra.arg <- ",mname='biclust'"
#  	}
#  	if(methodname %in% fabia.names){
#  		extra.arg <- ",mname='fabia'"
#  	}
#  	if(methodname %in% isa.names){
#  		extra.arg <- ",mname='isa2'"
#  	}
#  	
#  	#####################################################
#  	## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
#  	#####################################################
#  	
#  	toolname <- "BCDIAG"
#  	toolhelp <- "BcDiag-package"	
#  
#  }#ignore

