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

#' function to create the 'unsupTab' for data clustering
#' @title Unsupervised tab 
#' @description This function generates the unsupervised classification tab of the \code{\link{RclusToolGUI}}, in which the user can choose and configure the clustering method to apply.
#' @param RclusTool.env : environment in which data and intermediate results are stored.
#' @return None
#' @import tcltk tcltk2
#' @keywords internal
#' 
buildUnsupTab <- function(RclusTool.env) {
    unsup.env <- RclusTool.env$gui$tabs.env$unsup

    fontFrame <- tkfont.create(family = RclusTool.env$param$visu$font, weight = "bold", size = RclusTool.env$param$visu$size)
    fontTitleFrame <- tkfont.create(family = RclusTool.env$param$visu$titlefont, weight = "bold", size = RclusTool.env$param$visu$titlesize)
    padx = "6m"
    pady = "1m"

    unsup.env$tcl.export.clustering <-tclVar("1")
    unsup.env$tcl.classif.imgsig <- tclVar("0")
    unsup.env$tcl.extract.protos <- tclVar("0")
    unsup.env$tcl.rename.clusters <- tclVar("0")
    unsup.env$tcl.sampling.check <- tclVar("0")
    unsup.env$tcl.method.select <- tclVar("K-means")
    unsup.env$tcl.K <- tclVar("0")

    win1.nb <- RclusTool.env$gui$win1$env$nb
    win2.nb <- RclusTool.env$gui$win2$env$nb

    # Frames
    method.title <- c("K-means"="K-Means (KM)",
                      "PAM"="Partitioning Around Medoids (PAM)",
                      "HC"="Hierarchical Clustering (HC)",
                      "Spectral"="Spectral Clustering (SC)",
                      "EM"="Expectation-Maximization (EM)")


    method.description <- c("K-means"="Method: vector quantization\nTechnique: item belongs to cluster with the nearest\n\tmean (randomly initialized)\nResults: partition of N items into K clusters\nDisadvantage: computationally difficult (NP-hard)\nAdvantage: fast clustering",
                             "PAM"="Method: vector quantization\nTechnique: item belongs to cluster with the nearest\n\tmean (initially selected among items)\nResults: partition of N items into K clusters\nDisadvantage: computationally difficult (NP-hard)\nAdvantage: robust to noise and outliers",
                             "HC"="Method: building a hierarchy of clusters\nTechnique: compare dissimilarity measures\n\tbetween items\nResults: partition of N items into K clusters\nDisadvantage: slow for large datasets\n",
                             "Spectral"="Method: spectrum of data similarity matrix\nTechnique: evaluate the relative similarity of each pair\nResults: partition of N items into K clusters\nDisadvantage: slow for large datasets\nAdvantage: processing of non convex data\n",
                             "EM"="Method: maximum likelihood estimation\nTechnique: evaluate the expectation and maximize\nResults: partition of N items into K clusters\nDisadvantage: slow convergence\n\n")
	
	MethodFrametext <- makeTitle("CLUSTERING")
    MethodFrame <- tkwidget(win1.nb$env$unsup, "labelframe", text = MethodFrametext, font = fontTitleFrame, padx = padx , pady = pady, relief = "flat")
    AdviceFrame <- tkwidget(MethodFrame, "labelframe", text = "method name", padx = padx, pady = pady, relief = "groove")
    tkgrid(AdviceFrame, columnspan = 3, rowspan = 5, column = 2, row = 2, padx = padx)
    AdviceFrameText <- tk2label(AdviceFrame, text = "method description", width=50)
    tkgrid(AdviceFrameText, sticky = "w")
    MethodFrameExpert <- tkwidget(MethodFrame, "labelframe", font = fontFrame, padx = padx, relief = "flat")
    MethodFrameStandard <- tkwidget(MethodFrame, "labelframe", font = fontFrame, padx = padx, relief = "flat")

    unsup.env$onMethodDescription <- function()
    {
        tkconfigure(AdviceFrame, text=method.title[tclvalue(unsup.env$tcl.method.select)], font=fontFrame)
        tkconfigure(AdviceFrameText, text=method.description[tclvalue(unsup.env$tcl.method.select)], font=fontFrame)
    } 

    # method selection buttons
    rb_methods <- sapply(names(method.title), function(name) {
                             tkr <- tkradiobutton(MethodFrameExpert, variable=unsup.env$tcl.method.select, value=name, text=method.title[name])
                             tkbind(tkr, "<ButtonRelease-1>", unsup.env$onMethodDescription)
                             tkr
                             }, simplify=FALSE)
    

    # Space selection frame for expert mode
    # First listbox with all spaces
    SpaceFrametext <- makeTitle("FEATURE SPACE SELECTION")
    SpaceFrame <- tkwidget(win1.nb$env$unsup, "labelframe", text = SpaceFrametext, font = fontTitleFrame, padx = padx, pady = pady, relief = "flat")
    unsup.env$spaceList <- tk2listbox(SpaceFrame, selectmode = "single", activestyle = "dotbox",
                            height = 5, width = 45, autoscroll = "none", background = "white")
    # Space frame
    tkgrid(tk2label(SpaceFrame, text="Apply sampling"), row=11, column=0, sticky="w", pady = pady)
    tk.sampling.check <- tkcheckbutton(SpaceFrame, text="", variable=unsup.env$tcl.sampling.check, state="disabled")
    tkgrid(unsup.env$spaceList, row = 2, column = 1)
    tkgrid(tk.sampling.check, row=11, column=1, sticky="e")
    
    unsup.env$initSamplingCheck <- function()
    {
        state.sampling.button <- "disabled"
        if (!is.null(RclusTool.env$data.sample$config$sampling.size)){
            state.sampling.button <- "normal"
            unsup.env$tcl.sampling.check <- tclVar("1")
        }
        tkconfigure(tk.sampling.check,variable=unsup.env$tcl.sampling.check, state=state.sampling.button)

    }

    initAvailableSpaces <- function()
    {
        spaces <- names(RclusTool.env$data.sample$features) 
        spaces <- spaces[!grepl("pca_full", spaces, fixed=TRUE)]
        spaces <- spaces[spaces!="initial"]
        unsup.env$available.spaces <- sapply(spaces, featSpaceNameConvert, short2long=TRUE)
    }

    eraseSpaceList <- function()
    {
        sapply(1:size(unsup.env$spaceList), function(x) tkdelete(unsup.env$spaceList, "end"))
    }

    unsup.env$updateSpaceList <- function(reset=FALSE) {
        eraseSpaceList()

        initAvailableSpaces()

        sapply(unsup.env$available.spaces, function(s) tkinsert(unsup.env$spaceList, "end", s))
        ind <- which(unsup.env$featSpace==featSpaceNameConvert(unsup.env$available.spaces, short2long=FALSE))
        if (!length(ind))
            ind <- 1
        tkselection.set(unsup.env$spaceList, ind-1)
    }


    getSelectedSpace <- function()
    {
        space <- NULL
        selection <- tclvalue((tkcurselection(unsup.env$spaceList)))
        if (selection!="")
            space <- featSpaceNameConvert(unsup.env$available.spaces[as.numeric(selection)+1], short2long=FALSE)  
        space
    }

    OnSpaceSelection <- function()
    {
        unsup.env$featSpace <- getSelectedSpace()
    }

    tkbind(unsup.env$spaceList, "<ButtonRelease-1>", OnSpaceSelection)

    ## Export summary
    summaryConfig <- function() {
        summarytt <- tktoplevel()
        tktitle(summarytt) <- "Summaries"
        # Summaries frame
        summaryFrame <- tkwidget(summarytt, "labelframe", text = "SUMMARIES", padx = padx, pady = pady, relief = "groove")

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

        K <- as.integer(tclvalue(unsup.env$tcl.K))
        if (is.null(K) || (K<0) || (K>length(RclusTool.env$data.sample$id.clean)) || (K>RclusTool.env$param$classif$unsup$K.max)){
            tkmessageBox(message=paste("Please enter a valid number of clusters <=", RclusTool.env$param$classif$unsup$K.max))
            return()
        } 

        classif.space <- unsup.env$featSpace
        decomposition.space <- unlist(strsplit(classif.space, split=".", fixed=TRUE))
        pca <- ("pca" %in% decomposition.space) || ("pca_full" %in% decomposition.space)
        spec <- "spectral" %in% decomposition.space
        use.sampling <- tclvalue(unsup.env$tcl.sampling.check)=="1"
        use.scaling <- "scaled" %in% decomposition.space
        pca.nb.dims <- RclusTool.env$data.sample$config$pca.nb.dims
        method.select <- tclvalue(unsup.env$tcl.method.select)
        method.space.name <- paste(method.select,classif.space,sep="_")
        sampling.size.max <-  RclusTool.env$data.sample$config$sampling.size.max
        if (!is.null(RclusTool.env$data.sample$sampling))
            sampling.size.max <- RclusTool.env$data.sample$config$sampling.size.max

        tryCatch({
            new.data.sample <- computeUnSupervised(RclusTool.env$data.sample, K=K, 
                                                   method.name=method.select, 
                                                   pca=pca, pca.nb.dims=pca.nb.dims, spec=spec,
                                                   use.sampling=use.sampling, sampling.size.max=sampling.size.max,
                                                   scaling=use.scaling, RclusTool.env=RclusTool.env)
                     

            if (!is.null(new.data.sample)){                                              
                RclusTool.env$data.sample <- new.data.sample
            } else
            {
                tkmessageBox(message = "Unsupervised classification process failed.", icon = "warning", type = "ok")
                return(NULL)
            }

            spaceMsg <- paste("Working on ", classif.space, "\n", sep = "")

            pcaMsg <- ""
            if (pca)
            {
                pca.nb.dims <- length(RclusTool.env$data.sample$features[[classif.space]]$inertia.prop)
                pcaMsg <- paste("Working on PCA, nb of principal components = ", pca.nb.dims, "\n",
                                "Working on Principal Component Analysis (PCA)\n",
                                "Information lost:  ", round(100-(RclusTool.env$data.sample$features[[classif.space]]$inertia.prop[pca.nb.dims]*100),2),"%\n")
            }


            samplingMsg <- ""            
            if (use.sampling && !is.null(RclusTool.env$data.sample$sampling)) {
                samplingMsg <- paste("Sampling size = ", length(RclusTool.env$data.sample$sampling$selection.ids), "\n", sep = "")
            }

			if (method.select=="Spectral") {
				method.K="Gap"
			} else {
				method.K="Elbow"
			}
			 
            messageConsole(paste("----- Unsupervised Clustering -----\n", spaceMsg, pcaMsg, samplingMsg,
                                           method.select, " computing and K estimation: method ", method.K, "\n",
                                           "Obtained K:  ", length(unique(RclusTool.env$data.sample$clustering[[method.space.name]]$label)), "\n\n", sep = ""), RclusTool.env=RclusTool.env)

            unsup.env$label <- RclusTool.env$data.sample$clustering[[method.space.name]]$label
            unsup.env$cluster.summary <- RclusTool.env$data.sample$clustering[[method.space.name]]$summary
			
            # Automatically rename clusters
            if (tclvalue(unsup.env$tcl.rename.clusters)=="1") {
                new.data.sample <- nameClusters(data.sample = RclusTool.env$data.sample, 
                                                method = method.space.name, RclusTool.env=RclusTool.env)
                if (!is.null(new.data.sample))
                {
                    RclusTool.env$data.sample <- new.data.sample
                    # Update labels and summaries with new clusters names
                    unsup.env$label <- RclusTool.env$data.sample$clustering[[method.space.name]]$label
                    unsup.env$cluster.summary <- RclusTool.env$data.sample$clustering[[method.space.name]]$summary
                }
            }

            # Plot abundances from different methods
            tk2delete.notetab(win2.nb)
            abdPlotTabsGUI(RclusTool.env)
            # Elbow Plot
            if (K==0 && !grepl("Spectral_",method.space.name)){
            	ElbowPlot(win2.nb, method.space.name, RclusTool.env, hscale = RclusTool.env$param$visu$hscale, charsize=RclusTool.env$param$visu$size) 
            }
            
            features.mode <- classif.space
            # Visualization in PCA space
            new.protos <- visualizeSampleClustering(RclusTool.env$data.sample, label=unsup.env$label, clustering.name=method.space.name,
                                                    selection.mode = "prototypes", cluster.summary=unsup.env$cluster.summary,
                                                    profile.mode="whole sample", features.mode=features.mode, wait.close=TRUE,
                                                    RclusTool.env=RclusTool.env, fontsize=RclusTool.env$param$visu$size)
                                                    
            new.protos$label <- new.protos$label[[method.space.name]]$label
                                                  
            # Give new names to clusters and summaries                              
            RclusTool.env$data.sample$clustering[[method.space.name]]$label <- new.protos$label                                    
            unsup.env$cluster.summary <- clusterSummary(RclusTool.env$data.sample, new.protos$label, 
            											summary.functions=RclusTool.env$param$analysis$summary.functions)
    
            # Automatically extract protos
            if (tclvalue(unsup.env$tcl.extract.protos)=="1") {
                extractProtos(data.sample = RclusTool.env$data.sample, method = method.space.name, K.max=RclusTool.env$param$classif$unsup$K.max, kmeans.variance.min=RclusTool.env$param$classif$unsup$kmeans.variance.min, user.name=RclusTool.env$gui$user.name)
            }
			
            # Update labels with renamed clusters
            RclusTool.env$data.sample <- updateClustersNames(RclusTool.env$data.sample, new.protos$prototypes)
            # Update clusters names in plots (if necessary)
            tk2delete.notetab(win2.nb)
            abdPlotTabsGUI(RclusTool.env)
            # Elbow Plot
            if (K==0 && !grepl("Spectral_",method.space.name)){
            	ElbowPlot(win2.nb, method.space.name, RclusTool.env, hscale = RclusTool.env$param$visu$hscale, charsize=RclusTool.env$param$visu$size)
            }
            
            # Save clustering and summary (csv files)
            if (tclvalue(unsup.env$tcl.export.clustering)=="1") {
                fileClust.csv <- paste("clustering ", RclusTool.env$gui$user.name, " ",
                                                 method.space.name, ".csv", sep="")
                saveClustering(fileClust.csv, new.protos$label, RclusTool.env$data.sample$files$results$clustering)

                fileSum.csv <- paste("results ", RclusTool.env$gui$user.name, " ",
                                               method.space.name, ".csv", sep="")
                saveSummary(fileSum.csv, unsup.env$cluster.summary, RclusTool.env$data.sample$files$results$clustering)

            }

            # Classify images and signals (if available)
            if (tclvalue(unsup.env$tcl.classif.imgsig)=="1") {
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

        }, error=function(err){tkmessageBox(err$message);warning(err$message)})
    }

    tk.K <- tkentry(MethodFrameExpert, textvariable=unsup.env$tcl.K, width=2, background = "white")
	tkconfigure(tk.K,font = fontFrame)

    # Output frames
    OutputsFrametext <- makeTitle("OUTPUTS SELECTION")
    OutputsFrame <- tkwidget(win1.nb$env$unsup, "labelframe", text = OutputsFrametext, font = fontTitleFrame, padx = padx, pady = pady, relief = "flat")

    tk.export.clustering <- tkcheckbutton(OutputsFrame, text="", variable=unsup.env$tcl.export.clustering)
    tk.classif.imgsig <- tkcheckbutton(OutputsFrame, text="", variable=unsup.env$tcl.classif.imgsig)
    tk.extract.protos <- tkcheckbutton(OutputsFrame, text="", variable=unsup.env$tcl.extract.protos)
    tk.rename.clusters <- tkcheckbutton(OutputsFrame, text="", variable=unsup.env$tcl.rename.clusters)

    butSummary <- tk2button(OutputsFrame, text = "Summary settings", width = 20, command = summaryConfig)


    tk.compute.but <- tkbutton(win1.nb$env$unsup, text="COMPUTE", width = 10, command=OnCompute)


    # expert method frame layout
    #positioning radiobutton methods
    sapply(1:length(rb_methods), function(i) tkgrid(rb_methods[[i]], row=i-1, column=0, columnspan=2, padx= padx, sticky="w"))

    #positioning number of clusters wanted
    tkgrid(tk2label(MethodFrameExpert, text="Number of clusters (0=auto)"), row=6, column=0, sticky="w", pady=pady)
    tkgrid(tk.K, row=6, column=1, sticky="e")

    # layout OutputsFrame
    tkgrid(tk2label(OutputsFrame, text="     "))
    tkgrid(tk2label(OutputsFrame, text="Export clustering results"), row=11, column=0, sticky="w")
    tkgrid(tk.export.clustering, row=11, column=1, sticky="e")
    tkgrid(tk2label(OutputsFrame, text="Classify images/signals (if available)"), row=13, column=0, sticky="w")
    tkgrid(tk.classif.imgsig, row=13, column=1, sticky="e")
    tkgrid(tk2label(OutputsFrame, text="Extract prototypes automatically"), row=14, column=0, sticky="w")
    tkgrid(tk.extract.protos, row=14, column=1, sticky="e")
    tkgrid(tk2label(OutputsFrame, text="Rename clusters automatically"), row=15, column=0, sticky="w")
    tkgrid(tk.rename.clusters, row=15, column=1, sticky="e")
    tkgrid(tk2label(OutputsFrame, text="     "))
    tkgrid(butSummary, column = 0)

    # Standard method layout
    tkgrid(tk2label(MethodFrameStandard, text="Standard parameters:\n     - PCA\n     - K estimation: method Elbow\n     - K-means"), row = 0, column = 0)#, padx = c(200, 200))

    #positioning items
    tkEmptyLine(win1.nb$env$unsup, row=1)
    tkgrid(MethodFrame, columnspan = 3, row = 2, sticky = "we", pady=pady)
    if (RclusTool.env$gui$user.type=="expert")
    {
        # method selection frame
        tkgrid(MethodFrameExpert, row=2, column=0)
        # secondary frames
        tkEmptyLine(win1.nb$env$unsup, row=16)
        tkgrid(SpaceFrame, row = 17, columnspan = 3, sticky = "we", pady=pady)
        tkEmptyLine(win1.nb$env$unsup, row=21)
        tkgrid(OutputsFrame, row = 22, columnspan = 3, sticky = "we", pady=pady)
    } else {
        tkgrid(MethodFrameStandard, row=2, column=0)
    }
    tkEmptyLine(win1.nb$env$unsup)
    tkgrid(tk.compute.but, column = 0)#, columnspan=2, sticky="we")
}

#' function to initialize (and to create) the 'unsupTab' for data clustering
#' @title Unsupervised tab 
#' @description This function generates the unsupervised classification tab of the \code{\link{RclusToolGUI}}, in which the user can choose and configure the clustering method to apply.
#' @param RclusTool.env : environment in which data and intermediate results are stored.
#' @param reset : if TRUE the whole tab is reset, with default options
#' @return None
#' @import tcltk tcltk2
#' @keywords internal
#' 
initUnsupTab <- function(RclusTool.env, reset=FALSE)
{
    if (is.null(RclusTool.env$gui$tabs.env$unsup) || !length(RclusTool.env$gui$tabs.env$unsup))
    {
        RclusTool.env$gui$tabs.env$unsup <- new.env()
        buildUnsupTab(RclusTool.env)
        reset <- TRUE
    }

    unsup.env <- RclusTool.env$gui$tabs.env$unsup

    if (reset)
    {
        tclvalue(unsup.env$tcl.export.clustering) <- "1"
        tclvalue(unsup.env$tcl.classif.imgsig) <- "0"
        tclvalue(unsup.env$tcl.extract.protos) <- "0"
        tclvalue(unsup.env$tcl.rename.clusters) <- "0"
        tclvalue(unsup.env$tcl.sampling.check) <- "0"  
        tclvalue(unsup.env$tcl.method.select) <- "K-means"
        tclvalue(unsup.env$tcl.K) <- "0"
        unsup.env$label <- NULL
        unsup.env$cluster.summary <- NULL
        unsup.env$available.spaces <- NULL
        unsup.env$featSpace <- RclusTool.env$data.sample$config$default.classif.feature.space
        unsup.env$scaling <- RclusTool.env$data.sample$config$scaling
        unsup.env$onMethodDescription()
    }

    if (RclusTool.env$gui$user.type=="expert")
    {
        unsup.env$updateSpaceList()
        unsup.env$initSamplingCheck()
    } else {
    	unsup.env$featSpace <- 'pca'
    }
    
}
