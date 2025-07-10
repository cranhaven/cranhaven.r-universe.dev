# Project: Master Thesis
# 
# Author: Gebruiker
###############################################################################


# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


bibit3_WIN <- function(){     # Change newmethod to your own method name
  
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
  methodname <- "BiBit Patterns"
  
  # Define the function (as it is named in your package)
  # Note: If you have got a support function which is used for iterations, use it in this 'mainfunction'
  methodfunction <- "bibit3"
  
  
  # Define the name of the data argument for your function
  data.arg <- "matrix"
  
  
  # Define any other arguments in the function, which should not be changed by the user.
  # These arguments may also include a certain method for your function, since it is the idea to give each method a separate window.
  other.arg <- ""  # Comma in the beginning but not at the end !
  
  # Help Object
  methodhelp <- "bibit3"
  
  # Possibility to give a seed ?
  methodseed <- FALSE
  
  # Add a discretize box?
  data.discr <- FALSE
  
  # Add a binarize box?
  data.bin <- TRUE
  
  ## COMPATIBILITY? ##
  
  # BcDiag
  bcdiag.comp <- FALSE
  
  # SuperBiclust
  superbiclust.comp <- FALSE
  
  # Biclust only (Not for public use)
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
  
  
  ######		  ENTRY FIELDS FRAME 				#####
  #							    		 			#
  
  type <- "entryfields"
  
  # Change variables accordingly:
  frame.name <- "entryframe1"  
  argument.names <- c("Minimum Row Size","Minimum Column Size","Noise") 
  argument.types <- c("num","num","num")
  arguments <- c("minr","minc","noise")
  initial.values <- c(1,2,0)
  title <- ""
  border <- FALSE
  entry.width <- c("4","4","4")  
  
  # Do not change this line:
  new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
  
  
  
  ######		  ENTRY FIELDS FRAME 				#####
  #							    		 			#
  
  type <- "entryfields"
  
  # Change variables accordingly:
  frame.name <- "entryframe2"  
  argument.names <- c("Pattern Matrix") 
  argument.types <- c("num")
  arguments <- c("pattern_matrix")
  initial.values <- c("ADD_MATRIX_HERE")
  title <- ""
  border <- FALSE
  entry.width <- c("20")  
  
  # Do not change this line:
  new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
  
  
   
  
  
  ####		CHECK BOXES FRAME 			  ####
  #                               			 #
  
  type <- "checkboxes"
  
  # Change variables accordingly:
  frame.name <-  "patterncheck"
  argument.names <- c("Sub Patterns?","Extend Columns?","Pattern Combinations?") 
  arguments <- c("subpattern","extend_columns","pattern_combinations") 
  initial.values <- c(1,0,0) 
  title <- ""
  border <- FALSE
  
  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
  


  
  ###############################################################################################################################################################################
  ## CONFIGURATION OF GRID OF FRAMES - CLUSTERTAB ##
  ##################################################
  
  #########################
  #### THE GRID MATRIX ####
  #########################
  
  grid.config <- .grid.matrix(input=input,c("entryframe1",NA,"entryframe2","patterncheck"),byrow=TRUE,nrow=2,ncol=2,grid.config=grid.config)
  
  
  ####################################
  #### COMBINING ROWS -CLUSTERTAB ####
  ####################################
  
  grid.rows <- .combine.rows(input=input,rows=c(1),title="BiBit Specifications",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
  grid.rows <- .combine.rows(input=input,rows=c(2),title="Pattern Specifications",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
  
  
  
  ###############################################################################################################################################################################
  ###############################################################################################################################################################################
  
  
  
  
  ####################################
  #### PLOTTING & DIAGNOSTICS TAB ####
  ####################################
  
  input <- "plotdiagTab"
  
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
  
  
  ######		  ENTRY FIELDS FRAME 				#####
  #							    		 			#
  
  type <- "entryfields"
  
  # Change variables accordingly:
  frame.name <- "entryframe3"  
  argument.names <- c("Patterns","Biclusters") 
  argument.types <- c("char","char")
  arguments <- c("pattern","BC")
  initial.values <- c("1","1,2")
  title <- ""
  border <- FALSE
  entry.width <- c("10","10")  
  
  # Do not change this line:
  new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,entry.width=entry.width,argument.types=argument.types  ,new.frames=new.frames)
  
  
  
  
  ####		CHECK BOXES FRAME 			  ####
  #                               			 #
  
  type <- "checkboxes"
  
  # Change variables accordingly:
  frame.name <-  "patterncheck2"
  argument.names <- c("Full Pattern","Sub Pattern","Extended") 
  arguments <- c("fullpat","subpat","ext") 
  initial.values <- c(1,1,1) 
  title <- ""
  border <- FALSE
  
  # DO NOT CHANGE THIS LINE:
  new.frames <- .add.frame(input=input,type=type,frame.name=frame.name,argument.names=argument.names,arguments=arguments,initial.values=initial.values,title=title,border=border,new.frames=new.frames)
  

  
  
  ####	    	MANUAL BUTTONS FRAME 	  ####
  #             								 #
  
  type <- "buttons"
  
  # Change variables accordingly:
  frame.name <- "bibit3BCbuttonframe"  
  button.name <- "Show BC"  
  button.function <- "bibit3_BC_GUI" 
  button.data <- "data" 
  button.biclust <-  "object" 
  arg.frames <- c("entryframe3","patterncheck2")
  save <- FALSE
  show <- FALSE
  
  # Do not change this line: (without button.otherarg)
  new.frames <- .add.frame(input=input,frame.name=frame.name,save=save,show=show,type=type,button.name=button.name,button.function=button.function,button.data=button.data,button.biclust=button.biclust,arg.frames=arg.frames,new.frames=new.frames)
  
  
  
  ###############################################################################################################################################################################
  ## CONFIGURATION OF GRID OF FRAMES - PLOTDIAGTAB ##
  ###################################################
  
  
  #########################
  #### THE GRID MATRIX ####
  #########################
  
  
  grid.config <- .grid.matrix(input=input,c("summarybuttonframe",NA,"entryframe3","patterncheck2","bibit3BCbuttonframe",NA),byrow=TRUE,nrow=3,ncol=2,grid.config=grid.config)
  
  
  
  ########################
  #### COMBINING ROWS ####
  ########################
  
  grid.rows <- .combine.rows(input=input,rows=c(1),title="Summary",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
  grid.rows <- .combine.rows(input=input,rows=c(2,3),title="Print BC with Pattern",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
  
  
  
  #########################################################################
  ## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL CLUSTERTEMPLATE FUNCTION ##
  #########################################################################
  
  cluster_template(methodname=methodname,methodfunction=methodfunction,methodhelp=methodhelp,data.arg=data.arg,other.arg=other.arg,methodseed=methodseed,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames,superbiclust.comp=superbiclust.comp,bcdiag.comp=bcdiag.comp,data.discr=data.discr,data.bin=data.bin,extrabiclustplot=extrabiclustplot)
  
}


bibit3_BC_GUI <- function(data,object,pattern,BC,fullpat,subpat,ext){
  
  type <- c("full","sub","ext")[c(fullpat,subpat,ext)]
  type2 <- "c("
  for(i in 1:length(type)){
    if(i>1){type2 <- paste0(type2,",")}
    type2 <- paste0(type2,"'",type[i],"'") 
  }
  type2 <- paste0(type2,")")
  
  pm <- match.call()
  
  command <- paste0("bibit3_patternBC(result=",as.character(pm$object),",matrix=as.matrix(x),
                    pattern=c(",pattern," ),type=",type2,",BC=c(",BC,"))")
  
  doItAndPrint(command)
}


