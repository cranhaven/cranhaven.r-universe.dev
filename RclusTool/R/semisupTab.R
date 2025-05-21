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

#' function to build the 'semisupTab' for semi-supervised classification
#' @title Semi-Supervised tab 
#' @description Generate the semi-supervised classification tab of the \code{\link{RclusToolGUI}}, in which the user can choose and configure the semi-supervised method to apply.
#' @param RclusTool.env environment in which data and intermediate results are stored.
#' @return None
#' @import tcltk tcltk2
#' @keywords internal
#' 
buildSemisupTab <- function(RclusTool.env) {
    semi.env <- RclusTool.env$gui$tabs.env$semisup

    fontFrame <- tkfont.create(family = RclusTool.env$param$visu$font, weight = "bold", size = RclusTool.env$param$visu$size)
    fontTitleFrame <- tkfont.create(family = RclusTool.env$param$visu$titlefont, weight = "bold", size = RclusTool.env$param$visu$titlesize)
    padx = "6m"
    pady = "1m"

    semi.env$tcl.method.select <- tclVar("Constrained_KM")
    semi.env$tcl.K <- tclVar("0")
    semi.env$tcl.export.clustering <- tclVar("1")
    semi.env$tcl.export.calcul <- tclVar("0")
    semi.env$tcl.classif.imgsig <- tclVar("0")
    semi.env$tcl.extract.protos <- tclVar("0")
    semi.env$tcl.rename.clusters <- tclVar("0")
    semi.env$tcl.sampling.check <- tclVar("0")

    win1.nb <- RclusTool.env$gui$win1$env$nb
    win2.nb <- RclusTool.env$gui$win2$env$nb

    #non-necessary initializations
    cluster.summary <- NULL
    K <- 0
    method.select <- NULL

    method.title <- c("Constrained_KM"="Constrained K-Means (CKM)",
                      "Constrained_SC"="Constrained Spectral Clustering (CSC)")


    method.description <- c("Constrained_KM"="Method: vector quantization\nTechnique: item belongs to cluster with the nearest\n\tmean (randomly initialized)\nResults: partition of N items into K clusters\nDisadvantage: computationally difficult (NP-hard)\n",
                            "Constrained_SC"="Method: spectrum of data similarity matrix\nTechnique: evaluate the relative similarity of each pair\nResults: partition of N items into K clusters\nDisadvantage: slow for large datasets\nAdvantage: processing of non convex data\n")

	MethodFrametext <- makeTitle("CLUSTERING")
    MethodFrame <- tkwidget(win1.nb$env$semisup, "labelframe", text = MethodFrametext, font = fontTitleFrame, padx = padx, pady = pady, relief = "flat")
    AdviceFrame <- tkwidget(MethodFrame, "labelframe", text = "method name", padx = padx, pady = pady, relief = "groove")
    tkgrid(AdviceFrame, columnspan = 3, rowspan = 5, column = 2, row = 2, padx = padx)
    AdviceFrameText <- tk2label(AdviceFrame, text = "method description", width=50)
    tkgrid(AdviceFrameText, sticky = "w")
    MethodFrameExpert <- tkwidget(MethodFrame, "labelframe", font = fontFrame, padx = padx, relief = "flat")
    MethodFrameStandard <- tkwidget(MethodFrame, "labelframe", font = fontFrame, padx = padx, relief = "flat")

    semi.env$onMethodDescription <- function()
    {
        tkconfigure(AdviceFrame, text=method.title[tclvalue(semi.env$tcl.method.select)], font=fontFrame)
        tkconfigure(AdviceFrameText, text=method.description[tclvalue(semi.env$tcl.method.select)], font=fontFrame)
    } 
                            
    # method selection buttons
    rb_methods <- sapply(names(method.title), function(name) {
                             tkr <- tkradiobutton(MethodFrameExpert, variable=semi.env$tcl.method.select, value=name, text=method.title[name])
                             tkbind(tkr, "<ButtonRelease-1>", semi.env$onMethodDescription)
                             tkr
                            }, simplify=FALSE)
    
    # Space selection frame for expert mode
    # First listbox with all spaces
    SpaceFrametext <- makeTitle("FEATURE SPACE SELECTION")
    SpaceFrame <- tkwidget(win1.nb$env$semisup, "labelframe", text = SpaceFrametext, font = fontTitleFrame, padx = padx, pady = pady, relief = "flat")
    semi.env$spaceList <- tk2listbox(SpaceFrame, selectmode = "single", activestyle = "dotbox",
                            height = 5, width = 45, autoscroll = "none", background = "white")

    # Space frame
    tkgrid(tk2label(SpaceFrame, text="Apply sampling"), row=11, column=0, sticky="w", pady=pady)
    tk.sampling.check <- tkcheckbutton(SpaceFrame, text="", variable=semi.env$tcl.sampling.check, state="disabled")
    tkgrid(semi.env$spaceList, row = 2, column = 1)
    tkgrid(tk.sampling.check, row=11, column=1, sticky="e")

    semi.env$initSamplingCheck <- function()
    {
        state.sampling.button <- "disabled"
        if (!is.null(RclusTool.env$data.sample$config$sampling.size)){ 
            state.sampling.button <- "normal"
            semi.env$tcl.sampling.check <- tclVar("1")
        }
        tkconfigure(tk.sampling.check, variable=semi.env$tcl.sampling.check, state=state.sampling.button)
    }

    initAvailableSpaces <- function()
    {
        spaces <- names(RclusTool.env$data.sample$features) 
        spaces <- spaces[!grepl("pca_full", spaces, fixed=TRUE)]
        spaces <- spaces[spaces!="initial"]
        semi.env$available.spaces <- sapply(spaces, featSpaceNameConvert, short2long=TRUE)
    }

    eraseSpaceList <- function()
    {
        sapply(1:size(semi.env$spaceList), function(x) tkdelete(semi.env$spaceList, "end"))
    }

    semi.env$updateSpaceList <- function(reset=FALSE) {
        eraseSpaceList()

        initAvailableSpaces()

        sapply(semi.env$available.spaces, function(s) tkinsert(semi.env$spaceList, "end", s))
        ind <- which(semi.env$featSpace==featSpaceNameConvert(semi.env$available.spaces, short2long=FALSE))
        if (!length(ind))
            ind <- 1
        tkselection.set(semi.env$spaceList, ind-1)
    }

 
    getSelectedSpace <- function()
    {
        space <- NULL
        selection <- tclvalue((tkcurselection(semi.env$spaceList)))
        if (selection!="")
            space <- featSpaceNameConvert(semi.env$available.spaces[as.numeric(selection)+1], short2long=FALSE)
        space
    }

    OnSpaceSelection <- function()
    {
        semi.env$featSpace <- getSelectedSpace()
    }

    tkbind(semi.env$spaceList, "<ButtonRelease-1>", OnSpaceSelection)

    ## Export summary
    summaryConfig <- function() {
        summarytt <- tktoplevel()
        tktitle(summarytt) <- "Summaries"
        # Summaries frame
        summaryFrame <- tkwidget(summarytt, "labelframe", text = "SUMMARIES", padx = padx, pady = pady, relief = "flat")

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

        tcl.min.check <- tclVar(as.character(as.integer(config.env$summariesList["Min"])))
        tk.min.check <- tkcheckbutton(summaryFrame, text="", variable=tcl.min.check,
                                      command=OnMinCheck)
        tcl.max.check <- tclVar(as.character(as.integer(config.env$summariesList["Max"])))
        tk.max.check <- tkcheckbutton(summaryFrame, text="", variable=tcl.max.check,
                                      command=OnMaxCheck)
        tcl.sum.check <- tclVar(as.character(as.integer(config.env$summariesList["Sum"])))
        tk.sum.check <- tkcheckbutton(summaryFrame, text="", variable=tcl.sum.check,
                                      command=OnSumCheck)
        tcl.mean.check <- tclVar(as.character(as.integer(config.env$summariesList["Average"])))
        tk.mean.check <- tkcheckbutton(summaryFrame, text="", variable=tcl.mean.check,
                                       command=OnMeanCheck)
        tcl.std.check <- tclVar(as.character(as.integer(config.env$summariesList["SD"])))
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
        if (is.null(RclusTool.env$data.sample)){	
            msg <- paste("Please, load a file.")
            tkmessageBox(message=msg)
            return()
        }

        if (is.null(semi.env$pairs.abs))
            semi.env$pairs.abs <- list(ML=list(), CNL=list())

        is.empty <- function(pairs) {
            all(sapply(pairs, length)==0)
        }

        is.equal <- function(pairs.1, pairs.2) {
            if (length(pairs.1) != length(pairs.2))
                return(FALSE)

            if (length(pairs.1)==0)
                return(TRUE)

            all(unlist(pairs.1)==unlist(pairs.2))
        }

        if (sum(sapply(semi.env$pairs.abs, length))==0) {
            profile.mode <- "whole sample"
        } else profile.mode <- "constrained pairs"

        K <- as.integer(tclvalue(semi.env$tcl.K))
        if (is.null(K) || (K<0) || (K>length(RclusTool.env$data.sample$id.clean)) || (K>RclusTool.env$param$classif$unsup$K.max)){
            tkmessageBox(message=paste("Please enter a valid number of clusters <=", RclusTool.env$param$classif$unsup$K.max))
            return()
        } 
        
        classif.space <- semi.env$featSpace
        decomposition.space <- unlist(strsplit(classif.space, split=".", fixed=TRUE))
        pca <- ("pca" %in% decomposition.space) || ("pca_full" %in% decomposition.space)
        spec <- "spectral" %in% decomposition.space
        use.sampling <- tclvalue(semi.env$tcl.sampling.check)=="1"
        use.scaling <- "scaled" %in% decomposition.space
        pca.nb.dims <- RclusTool.env$data.sample$config$pca.nb.dims
        method.select <- tclvalue(semi.env$tcl.method.select)
        method.space.name <- paste(method.select,classif.space,sep="_")
		sampling.size.max <-  RclusTool.env$data.sample$config$sampling.size.max
        if (!is.null(RclusTool.env$data.sample$sampling))
        	sampling.size.max <- RclusTool.env$data.sample$config$sampling.size.max
        features.mode <- classif.space
		semi.env$label <- NULL
        semi.env$pairs.abs <- visualizeSampleClustering(RclusTool.env$data.sample, label=semi.env$label, 
                                                        clustering.name= method.space.name, profile.mode=profile.mode,
                                                        selection.mode = "pairs", pairs=semi.env$pairs.abs, wait.close=TRUE,
                                                        RclusTool.env=RclusTool.env, features.mode=features.mode, fontsize=RclusTool.env$param$visu$size)

        RclusTool.env$data.sample$clustering[[method.space.name]] <- computeSemiSupervised(data.sample=RclusTool.env$data.sample, ML=semi.env$pairs.abs$ML, CNL=semi.env$pairs.abs$CNL,
                                                                                           K=K, method.name=method.select, use.sampling=use.sampling, sampling.size.max=sampling.size.max, 
                                                                                           pca=pca, scaling=use.scaling, pca.nb.dims=pca.nb.dims, spec=spec, RclusTool.env=RclusTool.env)

        messageConsole(paste("----- Constrained clustering -----\n", #pcaMsg, samplingMsg,
                                       "# 'Must-Link' constraints:\n",
                                       "# 'Cannot-Link' constraints:\n",
                                       method.select, " computing\n",
                                       "Obtained K:  ", length(unique(RclusTool.env$data.sample$clustering[[method.space.name]]$label)), "\n\n", sep = ""), RclusTool.env=RclusTool.env)

        semi.env$label <- RclusTool.env$data.sample$clustering[[method.space.name]]$label
        cluster.summary <- RclusTool.env$data.sample$clustering[[method.space.name]]$summary

        # Automatically rename clusters from prototypes
        if (tclvalue(semi.env$tcl.rename.clusters)=="1") {
            new.data.sample <- nameClusters(data.sample = RclusTool.env$data.sample, 
            								method = method.space.name, RclusTool.env=RclusTool.env)
 	        	if (!is.null(new.data.sample)){
					RclusTool.env$data.sample <- new.data.sample
					# Update labels and summaries with new clusters names
					semi.env$label <- RclusTool.env$data.sample$clustering[[method.space.name]]$label
					semi.env$cluster.summary <- RclusTool.env$data.sample$clustering[[method.space.name]]$summary
				}
        }

        # Save calculations (rdata files)
        if (tclvalue(semi.env$tcl.export.calcul)=="1") {
            if (method.select == "Constrained_SC") {
                fileSpec <- paste(RclusTool.env$data.sample$name, " constrained spectral ", RclusTool.env$operator.name, " ",
                                  method.space.name, ".rdata", sep="")
                saveCalcul(fileSpec, RclusTool.env$data.sample$features$`spectral embedding`, RclusTool.env$data.sample$files$results$rdata)
            }
        }

        # Plot abundances from different methods
        tk2delete.notetab(win2.nb)
        abdPlotTabsGUI(RclusTool.env)
        # Elbow Plot
        if (K==0 && !grepl("Constrained_SC_",method.space.name)){
        	ElbowPlot(win2.nb, method.space.name, RclusTool.env, hscale = RclusTool.env$param$visu$hscale, charsize=RclusTool.env$param$visu$size) 
        }
        new.protos <- visualizeSampleClustering(RclusTool.env$data.sample, label=semi.env$label, clustering.name=method.space.name,
                                                selection.mode = "prototypes", cluster.summary=cluster.summary, 
                                                profile.mode="whole sample", wait.close=TRUE, features.mode=features.mode,
                                                RclusTool.env=RclusTool.env, fontsize=RclusTool.env$param$visu$size)
                                                
        new.protos$label <- new.protos$label[[method.space.name]]$label 
        
		# Give new names to clusters and summaries                              
        RclusTool.env$data.sample$clustering[[method.space.name]]$label <- new.protos$label                                    
        semi.env$cluster.summary <- clusterSummary(RclusTool.env$data.sample, new.protos$label, 
            									   summary.functions=RclusTool.env$param$analysis$summary.functions)
            											
        # Automatically extract protos
        if (tclvalue(semi.env$tcl.extract.protos)=="1") {
            extractProtos(data.sample = RclusTool.env$data.sample, method = method.space.name, K.max=RclusTool.env$param$classif$unsup$K.max, kmeans.variance.min=RclusTool.env$param$classif$unsup$kmeans.variance.min, user.name=RclusTool.env$gui$user.name)
        }

        # Update labels with renamed clusters
        RclusTool.env$data.sample <- updateClustersNames(RclusTool.env$data.sample, new.protos$prototypes)
        # Update clusters names in plots (if necessary)
        tk2delete.notetab(win2.nb)
        abdPlotTabsGUI(RclusTool.env)

        # Elbow Plot
        if (K==0 && !grepl("Constrained_SC_",method.space.name)){
        	ElbowPlot(win2.nb, method.space.name, RclusTool.env, hscale = RclusTool.env$param$visu$hscale, charsize=RclusTool.env$param$visu$size) 
        }
        
                # Save clustering and summary (csv files)
        if (tclvalue(semi.env$tcl.export.clustering)=="1") {
            fileClust.csv <- paste("clustering ", RclusTool.env$gui$user.name, " ",
                                             method.space.name, ".csv", sep="")
            saveClustering(fileClust.csv, new.protos$label, RclusTool.env$data.sample$files$results$clustering)
            fileSum.csv <-  paste("results ", RclusTool.env$gui$user.name, " ",
                                           method.space.name, ".csv", sep="")
            saveSummary(fileSum.csv, semi.env$cluster.summary, RclusTool.env$data.sample$files$results$clustering)
        }

        # Classify images and signals (if available)
        if (tclvalue(semi.env$tcl.classif.imgsig)=="1") {
            imgClassif(data.sample = RclusTool.env$data.sample, 
                       imgdir = RclusTool.env$data.sample$files$images, 
                       method = method.space.name, user.name=RclusTool.env$gui$user.name)
            sigClassif(data.sample = RclusTool.env$data.sample,
                       method = method.space.name, user.name=RclusTool.env$gui$user.name)
        }
        
        # Save prototypes (csv + image files in 'prototypes' directory)
        if (length(new.protos$prototypes[[method.space.name]]>0)){
        saveManualProtos(RclusTool.env$data.sample, new.protos$prototypes)

		}
        RclusTool.env$data.sample$clustering[[method.space.name]]$label <- semi.env$label
        
        #}
    }

    tk.K <- tkentry(MethodFrameExpert, textvariable=semi.env$tcl.K, width=2, background = "white")
    tkconfigure(tk.K,font = fontFrame)

    # Output frames
    OutputsFrametext <- makeTitle("OUTPUTS SELECTION")
    OutputsFrame <- tkwidget(win1.nb$env$semisup, "labelframe", text = OutputsFrametext, font = fontTitleFrame, padx = padx, pady = pady, relief = "flat")

    tk.export.clustering <- tkcheckbutton(OutputsFrame, text="", variable=semi.env$tcl.export.clustering)
    tk.classif.imgsig <- tkcheckbutton(OutputsFrame, text="", variable=semi.env$tcl.classif.imgsig)
    tk.extract.protos <- tkcheckbutton(OutputsFrame, text="", variable=semi.env$tcl.extract.protos)
    tk.rename.clusters <- tkcheckbutton(OutputsFrame, text="", variable=semi.env$tcl.rename.clusters)

    butSummary <- tk2button(OutputsFrame, text = "Summary settings", width = 20, command = summaryConfig)

    tk.compute.but <- tkbutton(win1.nb$env$semisup, text="COMPUTE", width = 10, command=OnCompute)

    # expert method frame layout
    #positioning radiobutton methods
    sapply(1:length(rb_methods), function(i) tkgrid(rb_methods[[i]], row=i-1, column=0, columnspan=2, padx=padx, sticky="w"))

    #positioning number of clusters wanted
    tkgrid(tk2label(MethodFrameExpert, text="Number of clusters (0=auto)"), row=6, column=0, sticky="w", pady = pady)
    tkgrid(tk.K, row=6, column=1, sticky="e", pady = pady)

    # layout OutputsFrame
    tkgrid(tk2label(OutputsFrame, text="Export clustering results"), row=11, column=0, sticky="w")
    tkgrid(tk.export.clustering, row=11, column=1, sticky="e")
    tkgrid(tk2label(OutputsFrame, text="Classify images/signals (if available)"), row=13, column=0, sticky="w")
    tkgrid(tk.classif.imgsig, row=13, column=1, sticky="e")
    tkgrid(tk2label(OutputsFrame, text="Extract prototypes automatically"), row=14, column=0, sticky="w")
    tkgrid(tk.extract.protos, row=14, column=1, sticky="e")
    tkgrid(tk2label(OutputsFrame, text="Rename clusters automatically"), row=15, column=0, sticky="w")
    tkgrid(tk.rename.clusters, row=15, column=1, sticky="e", pady=pady)
    tkgrid(butSummary, column = 0)

    # Standard method layout
    tkgrid(tk2label(MethodFrameStandard, text="Standard parameters:\n     - K estimation: method Elbow\n     - Constrained K-means"), row = 0, column = 0)#, padx = c(200, 200))

    #positioning items
    tkEmptyLine(win1.nb$env$semisup, row=1)
    tkgrid(MethodFrame, columnspan = 3, row = 2, sticky = "we", pady = pady)
    if (RclusTool.env$gui$user.type=="expert")
    {
        # method selection frame
        tkgrid(MethodFrameExpert, row=2, column=0)
        # secondary frames
        tkEmptyLine(win1.nb$env$semisup, row=15)
        tkgrid(SpaceFrame, row = 17, columnspan = 3, sticky = "we", pady=pady)
        tkEmptyLine(win1.nb$env$semisup, row=20)
        tkgrid(OutputsFrame, row = 22, columnspan = 3, sticky = "we", pady=pady)
    } else {
        tkgrid(MethodFrameStandard, row=2, column=0)
    }
    tkEmptyLine(win1.nb$env$semisup)
    tkgrid(tk.compute.but, column = 0)#, columnspan=2, sticky="we")
}


#' function to initialize (and to create) the 'semisupTab' for data clustering
#' @title Semi-Supervised tab 
#' @description Generate the semi-supervised classification tab of the \code{\link{RclusToolGUI}}, in which the user can choose and configure the semi-supervised method to apply.
#' @param RclusTool.env environment in which data and intermediate results are stored.
#' @param reset : if TRUE the whole tab is reset, with default options
#' @return None
#' @import tcltk tcltk2
#' @keywords internal
#' 
initSemisupTab <- function(RclusTool.env, reset=FALSE)
{
    if (is.null(RclusTool.env$gui$tabs.env$semisup) || !length(RclusTool.env$gui$tabs.env$semisup))
    {
        RclusTool.env$gui$tabs.env$semisup <- new.env()
        buildSemisupTab(RclusTool.env)
        reset <- TRUE
    }

    semi.env <- RclusTool.env$gui$tabs.env$semisup

    if (reset)
    {
        tclvalue(semi.env$tcl.method.select) <- "Constrained_KM"
        tclvalue(semi.env$tcl.K) <- "0"
        tclvalue(semi.env$tcl.export.clustering) <- "1"
        tclvalue(semi.env$tcl.export.calcul) <- "0"
        tclvalue(semi.env$tcl.classif.imgsig) <- "0"
        tclvalue(semi.env$tcl.extract.protos) <- "0"
        tclvalue(semi.env$tcl.rename.clusters) <- "0"
        tclvalue(semi.env$tcl.sampling.check) <- "0"  
        semi.env$available.spaces <- NULL
        semi.env$featSpace <- RclusTool.env$data.sample$config$default.classif.feature.space
        semi.env$scaling <- RclusTool.env$data.sample$config$scaling
        semi.env$label <- NULL
        semi.env$pairs.abs <- NULL
        semi.env$onMethodDescription()
    }

    if (RclusTool.env$gui$user.type=="expert")
    {
    	semi.env$updateSpaceList()
    	semi.env$initSamplingCheck()
    } else {
    	semi.env$featSpace <- 'preprocessed'
    }
}
