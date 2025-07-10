# Project: BiclustGUI
# 
# Author: lucp8394
###############################################################################


image_WINDOW <- function(){
	
	initializeDialog(title = gettextRcmdr("Drawing Heatmaps"),use.tabs=TRUE,tabs=c("tab1","tab2")) 
	
	AllResults <- .makeResultList()
	
	onOK <- function(){}
	
	onCancel <- function() {
		if (GrabFocus()) 
			tkgrab.release(top)
		tkdestroy(top)
		tkfocus(CommanderWindow())
	}
	
	onDataImage <- function(){
		
		transf <- tclvalue(trans_vars)
		
		if(transf=="none"){
			# still do a binary check here !! if the case, use color scheme of bin  | .is.binary.matrix
			
#			eval(parse(text=paste0("temp.data <-",ActiveDataSet())))
			temp.data <- get(ActiveDataSet(),envir=.GlobalEnv)
			
			if(.is.binary.matrix(as.matrix(temp.data))){
				col="c('grey','blue')"
				image.command <- paste0("image(c(1:dim(",ActiveDataSet(),")[2]),c(1:dim(",ActiveDataSet(),")[1]),t(as.matrix(",ActiveDataSet(),")),col=",col,",axes=FALSE,useRaster=TRUE,ylab='Genes',xlab='Samples')")
				doItAndPrint(image.command)
			}
			else{
				
				image.command <- paste0("image(c(1:dim(",ActiveDataSet(),")[2]),c(1:dim(",ActiveDataSet(),")[1]),t(as.matrix(",ActiveDataSet(),")),col=viridis(511),axes=FALSE,useRaster=TRUE,ylab='Genes',xlab='Samples')")
								
				doItAndPrint(image.command)
	
				
			}
			
		}
		if(transf=="bin"){
			col="c('grey','blue')"
			
			thres <- tclvalue(thres_vars)
			trans.command <- paste0("x <- binarize(x=as.matrix(",ActiveDataSet(),"),threshold=",thres,")")
			doItAndPrint(trans.command)
						
			image.command <- paste0("image(c(1:dim(x)[2]),c(1:dim(x)[1]),t(x),col=",col,",axes=FALSE,useRaster=TRUE,ylab='Genes',xlab='Samples')")
			doItAndPrint(image.command)
			
		}
		
		if(transf=="disc"){
			
			nlvl <- tclvalue(level_vars)
			quan <- ifelse(tclvalue(quantile_vars)=='1',TRUE,FALSE)
			trans.command <- paste0("x <- discretize(x=as.matrix(",ActiveDataSet(),"),nof=",nlvl,",quant=",quan,")")
			doItAndPrint(trans.command)
			
			image.command <- paste0("image(c(1:dim(x)[2]),c(1:dim(x)[1]),t(x),col=viridis(511,begin=1,end=0),axes=FALSE,useRaster=TRUE,ylab='Genes',xlab='Samples')")
			doItAndPrint(image.command)
		}
		
	}
	
	onResultImage <- function(){
		
		sel <- as.integer(tkcurselection(resultBox))+1
		if(length(AllResults)==0){
			justDoIt(paste0("warning('No available results',call.=FALSE)"))
		}
		else if(length(sel)==0){
			justDoIt(paste0("warning('No result selected',call.=FALSE)"))
		}
		else{
			
			SelResult <- AllResults[sel]
			
			# Do Data Check
			eval(parse(text=paste0("temp.correct <- .correctdataforresult(",SelResult,")")))
			
			if(temp.correct){
				
				transf <- tclvalue(trans_vars2)
				bin.thres <- tclvalue(thres_vars2)
				disc.nof <- tclvalue(level_vars2)
				disc.quant <- ifelse(tclvalue(quantile_vars)=='1',TRUE,FALSE)
				BC <- tclvalue(BC_vars)
				reorder <- ifelse(tclvalue(reorder_vars)=='1',TRUE,FALSE)
				background <- ifelse(tclvalue(background_vars)=='1',TRUE,FALSE)
				zeroBC <- ifelse(tclvalue(zeroBC_vars)=='1',TRUE,FALSE)
				thresZ <- tclvalue(thresZ_vars)
				thresL <- tclvalue(thresL_vars)
				BCResult <- .tobiclust_transf(SelResult,thresZ=paste0(thresZ),thresL=paste0(thresL))	
				BC.highlight <- tclvalue(BChighlightSel_vars)
				if(length(BC.highlight)==0){BC.highlight <- NULL}
				BC.highlight.opacity <- tclvalue(BChighlightOpa_vars)
				
				image.command <- paste0("HeatmapBC.GUI(data=",ActiveDataSet(),",res=",BCResult,",BC=",BC,",reorder=",reorder,",background=",background,",zeroBC=",zeroBC,",transf='",transf,"',bin.thres=",bin.thres,",disc.nof=",disc.nof,",disc.quant=",disc.quant,",BC.highlight=",BC.highlight,",BC.highlight.opacity=",BC.highlight.opacity,")")
				doItAndPrint(image.command)
				
			}
		}
	}
	
	#############
	### TAB 1 ###
	#############
	
	tab1Frame <- tkframe(tab1)
	transformFrame <- tkframe(tab1Frame)
	
	radioButtons(transformFrame,name="radioTransform",buttons=c("bin","disc","none"),values=c("bin","disc","none"),labels=gettextRcmdr(c("Binarization:","Discretation:","None")),initialValue="none",title="")
	
	trans_vars <- radioTransformVariable
	
	tkgrid(labelRcmdr(transformFrame,fg=getRcmdr("title.color"),font="RcmdrTitleFont" ,text=gettextRcmdr("Data Manipulation")),sticky="nw")
	

	transformEntry <- tkframe(transformFrame)
	
	
	thres_entry <- tkframe(transformEntry)
	thres_vars <- tclVar("NA")
	thres_field <- ttkentry(thres_entry,width=3,textvariable=thres_vars)
	tkgrid(labelRcmdr(thres_entry,text=gettextRcmdr("Threshold (NA=median)")),thres_field,sticky="nw")
	tkgrid(thres_entry,stick="nw")
	
	disc_pms <- tkframe(transformEntry)
	
	level_entry <- tkframe(disc_pms)
	level_vars <- tclVar("10")
	level_field <- ttkentry(level_entry,width=3,textvariable=level_vars)
	tkgrid(labelRcmdr(level_entry,text=gettextRcmdr("Number of Levels")),level_field,sticky="nw")
	

	checkBoxes(disc_pms,frame="quantilesCheck",boxes=paste("quantile"),initialValues=0,labels=gettextRcmdr("Use quantiles? (else equally spaced)"))
	quantile_vars <- quantileVariable
	
	tkgrid(level_entry,quantilesCheck,sticky="nw")
	tkgrid.configure(quantilesCheck,padx="10")
	tkgrid(disc_pms,sticky="nw")
	
	tkgrid(radioTransformFrame,transformEntry,stick='nw')
	tkgrid(transformFrame,stick="nw",padx="6",pady="6")
	
	drawdataButton <- buttonRcmdr(tab1Frame,command=onDataImage,text=gettextRcmdr("Heatmap"),foreground="darkgreen",default="active",width="12",borderwidth=3)
	tkgrid(drawdataButton,sticky="w",pady="15",padx="10")
	
	
	#############
	### TAB 2 ###
	#############
	tab2Frame <- tkframe(tab2)
	
	ResultTransFrame <- tkframe(tab2Frame)
	
	resultFrame <- tkframe(ResultTransFrame)
	
	resultBox <- tklistbox( resultFrame , height=5, exportselection="FALSE",
			selectmode="single", background="white")
	for (result in AllResults) tkinsert(resultBox, "end", result)
	resultScroll <- ttkscrollbar(resultFrame,command=function(...) tkyview(resultBox, ...))
	tkconfigure(resultBox, yscrollcommand=function(...) tkset(resultScroll, ...))
	if(length(AllResults)!=0){tkselection.set(resultBox,0)}
	
	tkgrid(labelRcmdr(resultFrame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Biclustering Results:")),sticky="nw")
	tkgrid(resultBox,resultScroll) #,sticky="ns"
	tkgrid.configure(resultScroll,sticky="ns")
	
	
	transformFrame2 <- tkframe(ResultTransFrame)
	
	radioButtons(transformFrame2,name="radioTransform2",buttons=c("bin","disc","none"),values=c("bin","disc","none"),labels=gettextRcmdr(c("Binarization:","Discretation:","None")),initialValue="none",title="")
	
	trans_vars2 <- radioTransform2Variable
	
	tkgrid(labelRcmdr(transformFrame2,fg=getRcmdr("title.color"),font="RcmdrTitleFont" ,text=gettextRcmdr("Data Manipulation")),sticky="nw")
	
	
	transformEntry2 <- tkframe(transformFrame2)
	
	
	thres_entry2 <- tkframe(transformEntry2)
	thres_vars2 <- tclVar("NA")
	thres_field2 <- ttkentry(thres_entry2,width=3,textvariable=thres_vars2)
	tkgrid(labelRcmdr(thres_entry2,text=gettextRcmdr("Threshold (NA=median)")),thres_field2,sticky="nw")
	tkgrid(thres_entry2,stick="nw")
	
	disc_pms2 <- tkframe(transformEntry2)
	
	level_entry2 <- tkframe(disc_pms2)
	level_vars2 <- tclVar("10")
	level_field2 <- ttkentry(level_entry2,width=3,textvariable=level_vars2)
	tkgrid(labelRcmdr(level_entry2,text=gettextRcmdr("Number of Levels")),level_field2,sticky="nw")
	
	
	checkBoxes(disc_pms2,frame="quantilesCheck2",boxes=paste("quantile2"),initialValues=0,labels=gettextRcmdr("Use quantiles? (else equally spaced)"))
	quantile_vars2 <- quantile2Variable
	
	tkgrid(level_entry2,quantilesCheck2,sticky="nw")
	tkgrid.configure(quantilesCheck2,padx="10")
	tkgrid(disc_pms2,sticky="nw")
	
	tkgrid(radioTransform2Frame,transformEntry2,stick='nw')
	
	
	tkgrid(resultFrame,sticky="nw",padx="6",pady="6")
	tkgrid(transformFrame2,sticky="nw",padx="6",pady="6")
	tkgrid(ResultTransFrame,sticky="nw")
	
	plotOptions <- tkframe(tab2Frame)
	
	tkgrid(labelRcmdr(plotOptions,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Heatmap Options")),sticky="nw")
	
	BC_entry <- tkframe(plotOptions)
	BC_vars <- tclVar("c()")
	BC_field <- ttkentry(BC_entry,width=15,textvariable=BC_vars)
	tkgrid(labelRcmdr(BC_entry,text=gettextRcmdr("Biclusters Selection ('c()' = All): ")),BC_field,sticky="nw")
	tkgrid(BC_entry,sticky="nw")
	
	checkBoxes(plotOptions,frame="backgroundCheck",boxes=paste("background"),initialValues=0,labels=gettextRcmdr("Add data heatmap on background?"))
	background_vars <- backgroundVariable
	tkgrid(backgroundCheck,sticky="nw")
	
	checkBoxes(plotOptions,frame="reorderCheck",boxes=paste("reorder"),initialValues=0,labels=gettextRcmdr("Reorder rows and columns for Bicluster Visualization?"))
	reorder_vars <- reorderVariable
	tkgrid(reorderCheck,sticky="nw")
	
	checkBoxes(plotOptions,frame="zeroBCCheck",boxes=paste("zeroBC"),initialValues=1,labels=gettextRcmdr("Also color genes of Biclusters which have '0' as response?"))
	zeroBC_vars <- zeroBCVariable
	tkgrid(zeroBCCheck,sticky="nw")
	
	# Highlight BC's
	BChighlight <- tkframe(plotOptions)
	
	BChighlightSel_entry <- tkframe(BChighlight)
	BChighlightSel_vars <- tclVar("")
	BChighlightSel_field <- ttkentry(BChighlightSel_entry,width=4,textvariable=BChighlightSel_vars)
	tkgrid(labelRcmdr(BChighlightSel_entry,text=gettextRcmdr("Bicluster Highlight: ")),BChighlightSel_field,sticky="nw")
	
	
	BChighlightOpa_entry <- tkframe(BChighlight)
	BChighlightOpa_vars <- tclVar("0.4")
	BChighlightOpa_field <- ttkentry(BChighlightOpa_entry,width=4,textvariable=BChighlightOpa_vars)
	tkgrid(labelRcmdr(BChighlightOpa_entry,text=gettextRcmdr("Opacity [0;1]: ")),BChighlightOpa_field,sticky="nw")
	
	tkgrid(BChighlightSel_entry,BChighlightOpa_entry,sticky="nw")
	tkgrid(BChighlight,sticky="nw")
	
	
	tkgrid(plotOptions,sticky="nw",padx="6",pady="6")
	
	
	### FABIA OPTIONS FRAME ###
	
	fabiaoptionsFrame <- tkframe(tab2Frame)
	tkgrid(labelRcmdr(fabiaoptionsFrame,fg=getRcmdr("title.color"),font="RcmdrTitleFont",text=gettextRcmdr("Fabia Result Options")),sticky="nw")
	
	
	thresZ_entry <- tkframe(fabiaoptionsFrame)
	thresZ_vars <- tclVar("0.5")
	thresZ_field <- ttkentry(thresZ_entry,width=6,textvariable=thresZ_vars)
	tkgrid(labelRcmdr(thresZ_entry,text=gettextRcmdr("Threshold Bicluster Sample: ")),thresZ_field,sticky="nw")
	tkgrid(thresZ_entry,sticky="ne")
	
	thresL_entry <- tkframe(fabiaoptionsFrame)
	thresL_vars <- tclVar("NULL")
	thresL_field <- ttkentry(thresL_entry,width=6,textvariable=thresL_vars)
	tkgrid(labelRcmdr(thresL_entry,text=gettextRcmdr("Threshold Bicluster Loading: ")),thresL_field,sticky="nw")
	tkgrid(thresL_entry,sticky="ne")
	
	tkgrid(fabiaoptionsFrame,padx="6",pady="6",sticky="nw")
	
	## Button
	drawresultButton <- buttonRcmdr(tab2Frame,command=onResultImage,text=gettextRcmdr("Heatmap"),foreground="darkgreen",default="active",width="12",borderwidth=3)
	tkgrid(drawresultButton,sticky="w",pady="15",padx="10")
	
	####################
	### GRID BUTTONS ###
	####################
	
	buttonsFrame <- tkframe(top)
	exitButton <- buttonRcmdr(buttonsFrame,command=onCancel,text=gettextRcmdr("Exit"),foreground="darkgreen",width="8",borderwidth=3)
	tkgrid(exitButton,sticky="es")
	
	tkgrid(tab1Frame)
	tkgrid(tab2Frame)
	dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE,onOK=onOK,tabs=c("tab1","tab2"),tab.names=c("Data","Biclustering Results"),preventGrabFocus=TRUE)
		
}
