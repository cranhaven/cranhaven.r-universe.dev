# Project: BiclustGUI
# 
# Author: Gebruiker
###############################################################################



# as.char!!: eSetData.name, blocGene , blocSample, r

bicare_WINDOW <- function(){     # Change newmethod to your own method name
	
	new.frames <- .initialize.new.frames()
	grid.config <- .initialize.grid.config()
	grid.rows <- .initialize.grid.rows()
	
	
	#####################################################
	## GENERAL INFORMATION ABOUT THE NEW METHOD/WINDOW ##
	#####################################################
	
	methodname <- "BICARE"
	
	methodfunction <- "bicare.GUI"
	data.arg <- "Data"
	methodshow <- TRUE
	methodsave <- FALSE
	other.arg <- ""
	methodhelp <- "BicARE"
	
	# Transform the data from data.arg
	data.transf <- "" # Values: "matrix" (default), "ExprSet",""
	
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
	
	
	########################
	#### CLUSTERING TAB ####
	########################
	
	input <- "clusterTab"
	
	### 1. ADDING THE FRAMES ###
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "eSetentry"  
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
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "optionsentry"  
	argument.names <- c("Number of biclusters searched","Genes init. membership prob.","Samples init/ membership prob.","Residue Threshold ( NULL=(Data Residue)/10 )","Min. number genes per bicluster","Min. number cond. per bicluster","Number of iterations") 
	argument.types <- c("num","num","char","char","num","num","num") 
	arguments <- c("k","pGene","pSample","r","N","M","t") 
	initial.values <- c("20","0.5","pGene","NULL","8","6","500")
	title <- ""
	border <- FALSE
	entry.width <- c("6","6","6","6","6","6","6")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "initentry"  
	argument.names <- c("Direct Init. matrix for genes","Direct Init. matrix for cond.") 
	argument.types <- c("char","char") 
	arguments <- c("blocGene","blocSample") 
	initial.values <- c("NULL","NULL")
	title <- "Direct Initialisations? (NULL=Random)"
	border <- FALSE
	entry.width <- c("15","15")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	
	### 2. CONFIGURING THE GRID ###
	
	grid.config <- .grid.matrix(input=input,c("eSetentry","optionsentry","initentry"),nrow=3,ncol=1,byrow=TRUE,grid.config=grid.config)
	
	
	### 3. COMBING THE ROWS ###
	
	grid.rows <- .combine.rows(input=input,rows=c(1),title="Expression Set Input (NULL='Active Dataset') ",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(2,3),title="BicARE Options ",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	
	####################################
	#### PLOTTING & DIAGNOSTICS TAB ####
	####################################
	
	input <- "plotdiagTab"
	
	### 1. ADDING THE FRAMES ###
	
	#### MANUAL BUTTONS FRAME ####
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "summarybutton"  
	button.name <- "Summary"  
	button.function <- "summaryBICARE" 
	button.data <- "" 
	button.biclust <-  "x" 
	button.width <- "12"
	button.data.transf <- ""
	
	arg.frames <- c()
	
	save <- FALSE 
	show <- TRUE
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
	frame.name <- "residuplotbutton"  
	button.name <- "Residue Plot"  
	button.function <- "bicare.residuplot" 
	button.data <- "" 
	button.biclust <-  "result" 
	button.width <- "12"
	button.data.transf <- ""
	
	arg.frames <- c()
	
	save <- FALSE 
	show <- TRUE
	button.otherarg <- "" 
	
	# Do not change this line: 
	new.frames <- .add.frame(input=input,frame.name=frame.name,
			type=type,button.name=button.name,
			button.function=button.function,button.data=button.data,
			button.biclust=button.biclust,button.otherarg=button.otherarg,
			button.width=button.width,button.data.transf=button.data.transf,
			arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "biclusterentry"  
	argument.names <- c("Bicluster Number") 
	argument.types <- c("num") 
	arguments <- c("k") 
	initial.values <- c(1)
	title <- ""
	border <- FALSE
	entry.width <- c("2")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	
	#### CHECK BOXES FRAME  ####
	
	type <- "checkboxes"
	
	# Change variables accordingly:
	frame.name <-  "biclustercheck"
	argument.names <- c("Plot?")  
	arguments <- c("graph") 
	initial.values <- c(0)  
	title <- ""
	border <- FALSE
	
	# DO NOT CHANGE THIS LINE:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,new.frames=new.frames)
	
	#### MANUAL BUTTONS FRAME ####
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "biclusterbutton"  
	button.name <- "Extract Bicluster"  
	button.function <- "bicluster" 
	button.data <- "" 
	button.biclust <-  "biclustering" 
	button.width <- "16"
	button.data.transf <- ""
	
	arg.frames <- c("biclusterentry","biclustercheck")
	
	save <- TRUE 
	show <- TRUE
	button.otherarg <- "" 
	
	# Do not change this line: 
	new.frames <- .add.frame(input=input,frame.name=frame.name,
			type=type,button.name=button.name,
			button.function=button.function,button.data=button.data,
			button.biclust=button.biclust,button.otherarg=button.otherarg,
			button.width=button.width,button.data.transf=button.data.transf,
			arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "genesetentry"  
	argument.names <- c("Geneset Type") 
	argument.types <- c("char") 
	arguments <- c("setType") 
	initial.values <- c("GOCollection()")
	title <- ""
	border <- FALSE
	entry.width <- c("15")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	#### MANUAL BUTTONS FRAME ####
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "genesethelp"  
	button.name <- "Genesets Help"  
	button.function <- "help" 
	button.data <- "" 
	button.biclust <-  "" 
	button.width <- "16"
	button.data.transf <- ""
	
	arg.frames <- c()
	
	save <- FALSE 
	show <- TRUE
	button.otherarg <- "'CollectionType-class'" 
	
	# Do not change this line: 
	new.frames <- .add.frame(input=input,frame.name=frame.name,
			type=type,button.name=button.name,
			button.function=button.function,button.data=button.data,
			button.biclust=button.biclust,button.otherarg=button.otherarg,
			button.width=button.width,button.data.transf=button.data.transf,
			arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "genesetentry2"  
	argument.names <- c("From","To") 
	argument.types <- c("num","num") 
	arguments <- c("gene.from","gene.to") 
	initial.values <- c("1","50")
	title <- "Which genes?"
	border <- FALSE
	entry.width <- c("3","3")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	#### MANUAL BUTTONS FRAME ####
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "genesetbutton"  
	button.name <- "Genes Enrichment"  
	button.function <- "bicare.genesetenrichment" 
	button.data <- "" 
	button.biclust <-  "result" 
	button.width <- "16"
	button.data.transf <- ""
	
	arg.frames <- c("genesetentry","genesetentry2")
	
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
	frame.name <- "pdatabutton"  
	button.name <- "pData"  
	button.function <- "bicare.pdata" 
	button.data <- "" 
	button.biclust <-  "result" 
	button.width <- "18"
	button.data.transf <- ""
	
	arg.frames <- c()
	
	save <- FALSE 
	show <- TRUE
	button.otherarg <- "" 
	
	# Do not change this line: 
	new.frames <- .add.frame(input=input,frame.name=frame.name,
			type=type,button.name=button.name,
			button.function=button.function,button.data=button.data,
			button.biclust=button.biclust,button.otherarg=button.otherarg,
			button.width=button.width,button.data.transf=button.data.transf,
			arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "pdatacovariates"  
	argument.names <- c("Columns in pData") 
	argument.types <- c("char") 
	arguments <- c("covariates") 
	initial.values <- c("c(1)")
	title <- "Covariates to use? (e.g. c(1,2) )"
	border <- FALSE
	entry.width <- c("20")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	#### MANUAL BUTTONS FRAME ####
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "samplesbutton"  
	button.name <- "Samples Enrichment"  
	button.function <- "bicare.samplesenrichment" 
	button.data <- "" 
	button.biclust <-  "result" 
	button.width <- "18"
	button.data.transf <- ""
	
	arg.frames <- c("pdatacovariates")
	
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
	
	#### ENTRY FIELDS FRAME ####
	
	type <- "entryfields"
	
	# Change variables accordingly:
	frame.name <- "reportentry"  
	argument.names <- c("Report Name") 
	argument.types <- c("char") 
	arguments <- c("dirName") 
	initial.values <- c("name")
	title <- ""
	border <- FALSE
	entry.width <- c("15")
	
	# Do not change this line:
	new.frames <- .add.frame(input=input,type=type
			,frame.name=frame.name,argument.names=argument.names
			,arguments=arguments,initial.values=initial.values
			,title=title,border=border,entry.width=entry.width
			,argument.types=argument.types  ,new.frames=new.frames)
	
	#### MANUAL BUTTONS FRAME ####
	
	type <- "buttons"
	
	# Change variables accordingly:
	frame.name <- "reportbutton"  
	button.name <- "Make Report"  
	button.function <- "bicare.makereport" 
	button.data <- "" 
	button.biclust <-  "" 
	button.width <- "12"
	button.data.transf <- ""
	
	arg.frames <- c("reportentry")
	
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
	frame.name <- "biclustplotbutton"  
	button.name <- "Biclust Plots"  
	button.function <- "bicarebiclust_WINDOW" 
	button.data <- "" 
	button.biclust <-  "" 
	button.width <- "12"
	button.data.transf <- ""
	
	arg.frames <- c()
	
	save <- FALSE 
	show <- FALSE
	button.otherarg <- paste("methodname='",methodname,"'",sep="") 
	
	# Do not change this line: 
	new.frames <- .add.frame(input=input,frame.name=frame.name,
			type=type,button.name=button.name,
			button.function=button.function,button.data=button.data,
			button.biclust=button.biclust,button.otherarg=button.otherarg,
			button.width=button.width,button.data.transf=button.data.transf,
			arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)
	
	
	### 2. CONFIGURING THE GRID ###
	
	grid.config <- .grid.matrix(input=input,c("summarybutton",NA,NA,"residuplotbutton",NA,NA,"biclusterentry","biclustercheck","biclusterbutton","genesetentry","genesethelp",NA,"genesetentry2","genesetbutton",NA,"pdatabutton","pdatacovariates",NA,"samplesbutton",NA,NA,"reportentry","reportbutton",NA,"biclustplotbutton",NA,NA),nrow=9,ncol=3,byrow=TRUE,grid.config=grid.config)
	 
	
	
	### 3. COMBING THE ROWS ###
	
	grid.rows <- .combine.rows(input=input,rows=c(1,2,3),title="General Diagnostics & Plots",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(4,5),title="Genesets Enrichment (Need complete eSet)",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(6,7),title="Samples Enrichment (Need complete eSet)",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(8),title="Make Report",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	grid.rows <- .combine.rows(input=input,rows=c(9),title="Extra Plots",border=TRUE,grid.rows=grid.rows,grid.config=grid.config)
	
	
	#########################################################################
	## USE ALL THE ARGUMENTS ABOUT IN THE GENERAL CLUSTERTEMPLATE FUNCTION ##
	#########################################################################
	
	cluster_template(methodname=methodname,methodfunction=methodfunction,methodhelp=methodhelp,data.arg=data.arg,other.arg=other.arg,methodseed=methodseed,grid.config=grid.config,grid.rows=grid.rows,new.frames=new.frames,superbiclust.comp=superbiclust.comp,bcdiag.comp=bcdiag.comp,data.transf=data.transf,data.discr=data.discr,data.bin=data.bin,methodshow=methodshow,methodsave=methodsave)
	
}