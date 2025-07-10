# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


newtool_WINDOW <- function(methodname){  
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	
	#####################################################
	## GENERAL INFORMATION ABOUT THE NEW TOOL		   ##
	#####################################################
		
	toolname <- "A new tool"
	
	toolhelp <- "helppage"
	
	
	# Do not change this line:
	input <- "plotdiagTab"
	
	#######################
	## MAKING THE WINDOW ##
	#######################
	
	### ADDING FRAMES ####
	
	# Analogous to plotdiag tab.
	
	
	### CONFIGURING GRID ###
	grid.config <- .grid.matrix(input=input,c(),nrow=1,ncol=2,byrow=TRUE,grid.config=grid.config)
	
	
	### COMBINING ROWS ###
	grid.rows <- .combine.rows(input=input,rows=c(),title="Plot 1",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	############################################################
	## USE ALL THE ARGUMENTS IN THE GENERAL NEW TOOL FUNCTION ##
	############################################################
	
	newtool_template(toolname=toolname,methodname=methodname,toolhelp=toolhelp,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames)
	
	
}
