# RclusTool: clustering of items in datasets
#
# Copyright 2013 Guillaume Wacquet, Pierre-Alexandre Hebert, Emilie Poisson-Caillault
#                
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' function to create the 'supTab' for supervised classification
#' @title Supervised tab 
#' @description Generate the supervised classification tab of the \code{\link{RclusToolGUI}}, in which the user can choose and configure the supervised method to apply.
#' @param RclusTool.env : environment in which data and intermediate results are stored.
#' @return None
#' @importFrom utils alarm
#' @import tcltk tcltk2
#' @keywords internal
#' 
buildsupTab <- function(RclusTool.env) {
    sup.env <- RclusTool.env$gui$tabs.env$sup
	
    fontFrame <- tkfont.create(family = RclusTool.env$param$visu$font, weight = "bold", size = RclusTool.env$param$visu$size)
    fontTitleFrame <- tkfont.create(family = RclusTool.env$param$visu$titlefont, weight = "bold", size = RclusTool.env$param$visu$titlesize)
    padx = "6m"
    pady = "1m"

	sup.env$tcl.export.clustering <-tclVar("1")
    sup.env$tcl.classif.imgsig <- tclVar("0")
    sup.env$tcl.export.calcul <- tclVar("0")
    sup.env$tcl.extract.protos <- tclVar("0")
    sup.env$tcl.method.select <- tclVar("RF")
    
    win1.nb <- RclusTool.env$gui$win1$env$nb
    win2.nb <- RclusTool.env$gui$win2$env$nb

    # Frames
    method.title <- c("RF"="Random Forest (RF)",
                      "K-NN"="K-Nearest-Neighbor (K-NN)",
                      "MLP"="MultiLayer Perceptron (MLP)",
                      "SVM"="Support Vector Machine (SVM)")
                      
    method.description <- c("RF"="Method: Construction of a multitude of decision trees\nTechnique: majority vote on predictions from all\n\tclassification trees\nResults: partition of N items into K clusters\nAdvantage: reduction in overfitting\n\n",
                            "K-NN"="Method: non-parametric\nTechnique: item is classified by a majority vote\n\tof its neighbors, with the object being assigned\n\tto the class most common among its k nearest\n\tneighbors mean (randomly initialized)\nResults: partition of N items into K clusters\nAdvantage: among the simplest of supervised methods",
                            "MLP"="Method:  feedforward artificial neural network\nTechnique: at least 3 layers of nodes for which each\n\tnode is a neuron that uses a nonlinear\n\tactivation function\nResults: partition of N items into K clusters\nAdvantage: processing of non-linearly separable data\n",
                            "SVM"="Method: Construction of a multitude of decision trees\nTechnique: majority vote on predictions from all\n\tclassification trees\nResults: partition of N items into K clusters\nAdvantage: reduction in overfitting\n\n")
	
	MethodFrametext <- makeTitle("CLASSIFICATION")
    MethodFrame <- tkwidget(win1.nb$env$sup, "labelframe", text = MethodFrametext, font = fontTitleFrame, padx = padx, pady = pady, relief = "flat")
    AdviceFrame <- tkwidget(MethodFrame, "labelframe", text = "method name", font = fontFrame, padx = padx, pady = pady, relief = "groove")
    tkgrid(AdviceFrame, columnspan = 3, rowspan = 5, column = 10, row = 2, padx = padx)
    AdviceFrameText <- tk2label(AdviceFrame, text = "method description", width=50)
    tkgrid(AdviceFrameText, sticky = "w")
    MethodFrameExpert <- tkwidget(MethodFrame, "labelframe", font = fontFrame, padx = padx, relief = "flat")
    MethodFrameStandard <- tkwidget(MethodFrame, "labelframe", font = fontFrame, padx = padx, relief = "flat")
    
    sup.env$onMethodDescription <- function()
    {
        tkconfigure(AdviceFrame, text=method.title[tclvalue(sup.env$tcl.method.select)], font=fontFrame)
        tkconfigure(AdviceFrameText, text=method.description[tclvalue(sup.env$tcl.method.select)], font=fontFrame)
    } 
    
    # method selection buttons
    rb_methods <- sapply(names(method.title), function(name) {
                             tkr <- tkradiobutton(MethodFrameExpert, variable=sup.env$tcl.method.select, value=name, text=method.title[name])
                             tkbind(tkr, "<ButtonRelease-1>", sup.env$onMethodDescription)
                             tkr
                             }, simplify=FALSE)
    
    # prototypes importation
    OnLoadDir <- function() {
        protos.directory.default <- RclusTool.env$gui$protos.dir
        if (is.null(protos.directory.default) || !dir.exists(protos.directory.default))
            protos.directory.default <- getwd()
        sup.env$protos.directory <- tk_choose.dir(default = protos.directory.default, caption = "Select training set base dir.")
        sup.env$refreshTrainingSetName()
        if (is.na(sup.env$protos.directory))
            return()
        RclusTool.env$gui$protos.dir <- sup.env$protos.directory

        if (nchar(RclusTool.env$gui$protos.dir)) {
            sup.env$prototypes <- readTrainSet(traindir = sup.env$protos.directory, operations=RclusTool.env$data.sample$config$operations, RclusTool.env=RclusTool.env)
            if (any(grepl(RclusTool.env$data.sample$name, sup.env$prototypes$Id)))
                sup.env$prototypes <- sup.env$prototypes[-grep(RclusTool.env$data.sample$name, sup.env$prototypes$Id), ]
            sup.env$id.clean.proto <- 1:NROW(sup.env$prototypes)
        }
    }

    ## Export summary
    summaryConfig <- function() {
        summarytt <- tktoplevel()
        tktitle(summarytt) <- "Summaries"
        # Summaries frame
        summaryFrame <- tkwidget(summarytt, "labelframe", font = fontFrame, text = "SUMMARIES", padx = padx, pady = pady, relief = "flat")

        summaries <- c("Min", "Max", "Sum", "Average", "SD")

        config.env <- new.env()
        config.env$summariesList <- summaries %in% names(RclusTool.env$param$analysis$summary.functions)

        names(config.env$summariesList) <- summaries
        functionsList <- c("min", "max", "sum", "mean", "sd")
        names(functionsList) <- summaries

        # Export min(parameters) per cluster
        OnMinCheck <- function() {
            config.env$summariesList["Min"] <- tclvalue(tcl.min.check)=="1"
        }
        # Export max(parameters) per cluster
        OnMaxCheck <- function() {
            config.env$summariesList["Max"] <- tclvalue(tcl.max.check)=="1"
        }
        # Export sum(parameters) per cluster
        OnSumCheck <- function() {
           config.env$summariesList["Sum"] <- tclvalue(tcl.sum.check)=="1"
        }
        # Export mean(parameters) per cluster
        OnMeanCheck <- function() {
            config.env$summariesList["Average"] <- tclvalue(tcl.mean.check)=="1"
        }
        # Export std(parameters) per cluster
        OnStdCheck <- function() {
            config.env$summariesList["SD"] <- tclvalue(tcl.std.check)=="1"
        }

        tcl.min.check <- tclVar(as.character(as.numeric(config.env$summariesList["Min"])))
        tk.min.check <- tkcheckbutton(summaryFrame, text="", variable=tcl.min.check,
                                      command=OnMinCheck)
        tcl.max.check <- tclVar(as.character(as.numeric(config.env$summariesList["Max"])))
        tk.max.check <- tkcheckbutton(summaryFrame, text="", variable=tcl.max.check,
                                      command=OnMaxCheck)
        tcl.sum.check <- tclVar(as.character(as.numeric(config.env$summariesList["Sum"])))
        tk.sum.check <- tkcheckbutton(summaryFrame, text="", variable=tcl.sum.check,
                                      command=OnSumCheck)
        tcl.mean.check <- tclVar(as.character(as.numeric(config.env$summariesList["Average"])))
        tk.mean.check <- tkcheckbutton(summaryFrame, text="", variable=tcl.mean.check,
                                       command=OnMeanCheck)
        tcl.std.check <- tclVar(as.character(as.numeric(config.env$summariesList["SD"])))
        tk.std.check <- tkcheckbutton(summaryFrame, text="", variable=tcl.std.check,
                                      command=OnStdCheck)

        tkgrid(tk2label(summarytt, text = "     "), row=1, sticky = "w")
        tkgrid(summaryFrame, row = 2, columnspan = 2, sticky = "w")
        tkgrid(tk2label(summaryFrame, text="Minimum"), row=2, column=0, sticky="w")
        tkgrid(tk.min.check, row=2, column=1, sticky="e")
        tkgrid(tk2label(summaryFrame, text="Maximum"), row=3, column=0, sticky="w")
        tkgrid(tk.max.check, row=3, column=1, sticky="e")
        tkgrid(tk2label(summaryFrame, text="Sum"), row=4, column=0, sticky="w")
        tkgrid(tk.sum.check, row=4, column=1, sticky="e")
        tkgrid(tk2label(summaryFrame, text="Average"), row=5, column=0, sticky="w")
        tkgrid(tk.mean.check, row=5, column=1, sticky="e")
        tkgrid(tk2label(summaryFrame, text="Standard deviation"), row=6, column=0, sticky="w")
        tkgrid(tk.std.check, row=6, column=1, sticky="e")

        # Apply operation
        onClose <- function() {
            RclusTool.env$param$analysis$summary.functions <- functionsList[config.env$summariesList]
            tkdestroy(summarytt)
        }
        butClose <- tk2button(summarytt, text = "Close", width = -6, command = onClose)
        tkgrid(butClose, row = 7, columnspan = 2)
        tkwait.window(summarytt)
    }
    
    # Compute the selected method
    OnCompute <- function() {
    	if (RclusTool.env$gui$protos.dir==""){
            utils::alarm()
            tkmessageBox(message="Please first select a training set.")
            OnLoadDir()
            return()
        }
        method.select <- tclvalue(sup.env$tcl.method.select)

        res <- computeSupervised(RclusTool.env$data.sample, 
                                 prototypes = sup.env$prototypes[sup.env$id.clean.proto, , drop=FALSE], 
                                 method.name = method.select, model=NULL, RclusTool.env=RclusTool.env)
        if (is.null(res))
            return()

        messageConsole(paste("----- Supervised Classification -----\n", 
                                       "Training set:", basename(RclusTool.env$gui$protos.dir), "\n",
                                       method.select, " computing\n\n", sep = ""), RclusTool.env=RclusTool.env)

        RclusTool.env$data.sample$clustering[[method.select]] <- list(label=res$label, summary=res$summary, K=sum(table(res$label)>0))
        prototypes <- res$prototypes # non global? useful?
        sup.env$label <- RclusTool.env$data.sample$clustering[[method.select]]$label
        sup.env$cluster.summary <- RclusTool.env$data.sample$clustering[[method.select]]$summary

        # Plot abundances from different methods
        tk2delete.notetab(win2.nb)
        abdPlotTabsGUI(RclusTool.env)
        #Keep only levels who are in clustering 
        sup.env$label=droplevels(sup.env$label)
        
        #visualize prototypes obtained from the sample used
        new.protos <- visualizeSampleClustering(RclusTool.env$data.sample, label=sup.env$label, clustering.name=method.select,
                                                selection.mode = "prototypes", cluster.summary=sup.env$cluster.summary,
                                                profile.mode="whole sample", wait.close=TRUE, 
                                                RclusTool.env=RclusTool.env, fontsize=RclusTool.env$param$visu$size)   
                                                
        new.protos$label <- new.protos$label[[method.select]]$label
        sup.env$cluster.summary <- clusterSummary(RclusTool.env$data.sample, new.protos$label, 
            									  summary.functions=RclusTool.env$param$analysis$summary.functions)
            									  
        # Save clustering and summary (csv files)
        if (tclvalue(sup.env$tcl.export.clustering)=="1") {
            fileClust.csv <- paste("clustering ", RclusTool.env$gui$user.name, " ",
                                             method.select, ".csv", sep="")
            saveClustering(fileClust.csv, new.protos$label, RclusTool.env$data.sample$files$results$clustering)
            fileSum.csv <- paste("results ", RclusTool.env$gui$user.name, " ",
                                           method.select, ".csv", sep="")
            saveSummary(fileSum.csv, sup.env$cluster.summary, RclusTool.env$data.sample$files$results$clustering)
        }

        # Classify images and signals (if available)
        if (tclvalue(sup.env$tcl.classif.imgsig)=="1") {
            imgClassif(data.sample = RclusTool.env$data.sample, 
                       imgdir = RclusTool.env$data.sample$files$images, 
                       method = method.select, user.name=RclusTool.env$gui$user.name)
		    sigClassif(data.sample = RclusTool.env$data.sample,
		    		   method = method.select, user.name=RclusTool.env$gui$user.name)
        }

        # Automatically extract protos
        if (tclvalue(sup.env$tcl.extract.protos)=="1") {
            extractProtos(data.sample = RclusTool.env$data.sample, method = method.select, K.max=RclusTool.env$param$classif$unsup$K.max, kmeans.variance.min=RclusTool.env$param$classif$unsup$kmeans.variance.min, user.name=RclusTool.env$gui$user.name)
        }

        # Give new names to clusters and summaries                              
        RclusTool.env$data.sample$clustering[[method.select]]$label <- new.protos$label  
        sup.env$cluster.summary <- RclusTool.env$data.sample$clustering[[method.select]]$summary
     
                                                 
        # Update labels with renamed clusters
        RclusTool.env$data.sample <- updateClustersNames(RclusTool.env$data.sample, new.protos$prototypes)
        # Update clusters names in plots (if necessary)
        tk2delete.notetab(win2.nb)
        abdPlotTabsGUI(RclusTool.env)
        # Save prototypes (csv + image files in 'prototypes' directory)
        if (length(new.protos$prototypes[[method.select]]>0)){
        saveManualProtos(RclusTool.env$data.sample, new.protos$prototypes)
        }
    }
    ProtoFrametext <- makeTitle("TRAINING SET")
    ProtoFrame <- tkwidget(win1.nb$env$sup, "labelframe", text = ProtoFrametext, font = fontTitleFrame, padx = padx, pady = pady, relief = "flat")
    tk.folder.but <- tk2button(ProtoFrame,text="Training set", image = "folder", compound = "left", width = 20, command=OnLoadDir)
    
    TrainingSetName <- tktext(ProtoFrame, bg="white", font="courier", width=7*RclusTool.env$param$visu$size, height=2, font = fontFrame, state="disabled")

    sup.env$refreshTrainingSetName <- function()
    {
 		tkconfigure(TrainingSetName, state="normal") 
        tkdelete(TrainingSetName, "1.0", "end")
        tkinsert(TrainingSetName,"end", sup.env$protos.directory)
        tkconfigure(TrainingSetName, state="disabled") 
    }
    
    
    # Positioning
    tkEmptyLine(win1.nb$env$sup, row=1)
    tkgrid(ProtoFrame, columnspan = 4, row = 2, sticky = "we", pady = pady)
    tkgrid(tk2label(ProtoFrame, text="     "), row = 1,column = 0)
    tkgrid(tk2label(ProtoFrame, text="Selection of required files and folder\n Format : separator = ',' decimal= '.'"), row = 2, column = 0)
    tkgrid(tk.folder.but, row = 3, column = 0)
    tkgrid(TrainingSetName, row = 3, column = 1)
    
    # Output frames
    OutputsFrametext <- makeTitle("OUTPUTS SELECTION")
    OutputsFrame <- tkwidget(win1.nb$env$sup, "labelframe", text = OutputsFrametext, font = fontTitleFrame, padx = padx, pady = pady, relief = "flat")
    
    tk.export.clustering <- tkcheckbutton(OutputsFrame, text="", variable=sup.env$tcl.export.clustering)
    tk.classif.imgsig <- tkcheckbutton(OutputsFrame, text="", variable=sup.env$tcl.classif.imgsig)
    tk.extract.protos <- tkcheckbutton(OutputsFrame, text="", variable=sup.env$tcl.extract.protos)
    
    butSummary <- tk2button(OutputsFrame, text = "Summary settings", width = 20, command = summaryConfig)
       
    tk.compute.but <- tkbutton(win1.nb$env$sup, text="COMPUTE", width = 10, command=OnCompute)
    
    # expert method frame layout
    #positioning radiobutton methods
    sapply(1:length(rb_methods), function(i) tkgrid(rb_methods[[i]], row=i+1, column=0, padx=padx, sticky="w"))
		
    # layout OutputsFrame
    tkgrid(tk2label(OutputsFrame, text="Export classification results"), row=9, column=0, sticky="w")
    tkgrid(tk.export.clustering, row=9, column=1, sticky="e")
    tkgrid(tk2label(OutputsFrame, text="Classify images/signals (if available)"), row=11, column=0, sticky="w")
    tkgrid(tk.classif.imgsig, row=11, column=1, sticky="e")
    tkgrid(tk2label(OutputsFrame, text="Extract prototypes automatically"), row=12, column=0, sticky="w")
    tkgrid(tk.extract.protos, row=12, column=1, sticky="e")
    tkgrid(tk2label(OutputsFrame, text="     "))
    tkgrid(butSummary, column = 0)
    
	# Standard method frame layout
    tkgrid(tk2label(MethodFrameStandard, text="Standard parameters:\n     - Random Forest\n     - Number of trees: 500"), row = 2, column = 0)
    tkgrid(tk2label(MethodFrameStandard, text="     "), row = 6, column = 0)
    
  	if (RclusTool.env$gui$user.type=="expert")
    {
        # method selection frame
        tkgrid(MethodFrameExpert, row=2, column=0)
        # secondary frames
        tkgrid(OutputsFrame, row = 7, columnspan = 4, sticky = "we", pady = pady)
    } else {
        tkgrid(MethodFrameStandard, row=2, column=0)
    }
    
    # Reset Sup Tab
    onReset <- function() {
		initSupTab(RclusTool.env = RclusTool.env, reset=TRUE)
    }
    butReset <- tk2button(win1.nb$env$sup, text = "Reset", image = "reset", compound = "left", width = -6, command = onReset)
    
     #positioning
    tkEmptyLine(win1.nb$env$sup, row=3)
    tkgrid(MethodFrame, columnspan = 4, row = 4, column = 0, sticky = "we", pady = pady)
    tkEmptyLine(win1.nb$env$sup, row=5)
    tkgrid(tk.compute.but, row=9, column = 0)
    tkgrid(butReset, row = 9, column = 2)
   

    invisible(list(label=sup.env$label, cluster.summary=sup.env$cluster.summary)) #value(s)
}

#' function to initialize (and to create) the 'supTab' for supervised classification
#' @title supervised tab 
#' @description This function generates the supervised classification tab of the \code{\link{RclusToolGUI}}, in which the user can choose and configure the classification method to apply.
#' @param RclusTool.env : environment in which data and intermediate results are stored.
#' @param reset : if TRUE the whole tab is reset, with default options
#' @return None
#' @import tcltk tcltk2
#' @keywords internal
#' 
initSupTab <- function(RclusTool.env, reset=FALSE)
{
    if (is.null(RclusTool.env$gui$tabs.env$sup) || !length(RclusTool.env$gui$tabs.env$sup))
    {
        RclusTool.env$gui$tabs.env$sup <- new.env()
        buildsupTab(RclusTool.env)
        reset <- TRUE
    }

    sup.env <- RclusTool.env$gui$tabs.env$sup

    if (reset)
    {
    	tclvalue(sup.env$tcl.export.clustering) <- "1"
        tclvalue(sup.env$tcl.classif.imgsig) <- "0"
        tclvalue(sup.env$tcl.extract.protos) <- "0"
        tclvalue(sup.env$tcl.method.select) <- "RF"
        sup.env$onMethodDescription()
    	sup.env$prototypes <- NULL
    	sup.env$id.clean.proto <- NULL
    	sup.env$export.clustering <- TRUE
    	sup.env$export.calcul <- FALSE
    	sup.env$classif.imgsig <- FALSE
    	sup.env$extract.protos <- FALSE
    	sup.env$selectedvar <- NULL
    	sup.env$protoclean <- NULL
    	sup.env$datenames <- NULL        
    	sup.env$protodatelist <- NULL
    	sup.env$classnames <- NULL
    	sup.env$protoclasslist <- NULL
    	sup.env$tcl.lowerthreshold <- NULL
    	sup.env$tk.lowerthreshold <- NULL
    	sup.env$tcl.upperthreshold <- NULL
    	sup.env$tk.upperthreshold <- NULL
    	sup.env$label <- NULL
    	sup.env$cluster.summary <- NULL
    	sup.env$protos.directory <- ""
    	sup.env$refreshTrainingSetName()
    }
}
