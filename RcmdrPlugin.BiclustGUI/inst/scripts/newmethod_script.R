# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


newmethod_WINDOW <- function(){     # Change newmethod to your own method name
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	
	#####################################################
	## GENERAL INFORMATION ABOUT THE NEW METHOD/WINDOW ##
	#####################################################

	methodname <- "A new method"

	methodfunction <- "methodfunction"
	data.arg <- "d"
	methodshow <- TRUE
	methodsave <- TRUE
	other.arg <- ""
	methodhelp <- ""

	# Transform the data from data.arg
	data.transf <- "matrix" # Values: "matrix" (default), "ExprSet"
	
	# Extra Data Conversion Boxes
	data.discr <- FALSE
	data.bin <- FALSE
		
	# Possibility to give a seed ?
	methodseed <- TRUE
	
	## COMPATIBILITY? ##
	
	# BcDiag
	bcdiag.comp <- FALSE
	
	# SuperBiclust
	superbiclust.comp <- FALSE
	
		
	########################
	#### CLUSTERING TAB ####
	########################
	
	input <- "clusterTab"
	
	### 1. ADDING THE FRAMES ###
	
	# Add frames here
	
	### 2. CONFIGURING THE GRID ###
	
	grid.config <- .grid.matrix(input=input,c("frame1","frame2","frame3",NA,"frame4",NA),nrow=3,ncol=2,byrow=TRUE,grid.config=grid.config)
	
	
	### 3. COMBING THE ROWS ###

	grid.rows <- .combine.rows(input=input,rows=c(1),title="A nice box: ",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(2,3),title="A nice box: ",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	
	####################################
	#### PLOTTING & DIAGNOSTICS TAB ####
	####################################
	
	input <- "plotdiagTab"
		
	### 1. ADDING THE FRAMES ###
	
	# Add frames here
	
	### 2. CONFIGURING THE GRID ###
	
	grid.config <- .grid.matrix(input=input,c("frame5","frame6"),nrow=1,ncol=2,byrow=TRUE,grid.config=grid.config)
		
	### 3. COMBING THE ROWS ###
	
	grid.rows <- .combine.rows(input=input,rows=c(1),title="Plot 1",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
		
	###################################################################
	## USE ALL THE ARGUMENTS IN THE GENERAL CLUSTERTEMPLATE FUNCTION ##
	###################################################################
	
	cluster_template(methodname=methodname,methodfunction=methodfunction,methodhelp=methodhelp,data.arg=data.arg,other.arg=other.arg,methodseed=methodseed,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames,superbiclust.comp=superbiclust.comp,bcdiag.comp=bcdiag.comp,data.transf=data.transf,data.discr=data.discr,data.bin=data.bin,methodshow=methodshow,methodsave=methodsave)
	
}
