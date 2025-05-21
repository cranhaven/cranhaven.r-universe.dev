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

#' function to build the 'preprocessTab' for data preprocessing (variables selection, transformation, creation, ...)
#' @title build Preprocess tab
#' @description Generate the data preprocessing tab of the \code{\link{RclusToolGUI}}, in which the user can select, transform, filter or create variables.
#' @param RclusTool.env environment in which data and intermediate results are stored.
#' @return None
#' @importFrom utils write.csv
#' @import tcltk tcltk2
#' @keywords internal
#' 
buildPreprocessTab <- function(RclusTool.env) {
    prepro.env <- RclusTool.env$gui$tabs.env$prepro

    prepro.env$tcl.pca.check <- tclVar("1") #mode standard non expert
    prepro.env$tcl.spectral.check <- tclVar("0")
    prepro.env$tcl.pca.nb.dims <- tclVar("0")
    prepro.env$tcl.sampling.check <- tclVar("0")
    prepro.env$tcl.scaling.check <- tclVar("1")
    prepro.env$tcl.sampling.size <- tclVar("0")

    win1.nb <- RclusTool.env$gui$win1$env$nb
    win2.nb <- RclusTool.env$gui$win2$env$nb

    creatVar.env <- new.env()
    signal.names <- colnames(RclusTool.env$data.sample$profiles[[1]])

    fontFrame <- tkfont.create(family = RclusTool.env$param$visu$font, weight = "bold", size = RclusTool.env$param$visu$size)
    fontTitleFrame <- tkfont.create(family = RclusTool.env$param$visu$titlefont, weight = "bold", size = RclusTool.env$param$visu$titlesize)
    padx = "6m"
    padx.m = "3m"
    pady = "1m"
    pady.s = "0.5m"

    space.title <- c("preprocessed"="Preprocessed Features",
                     "pca"="Principal Components Analysis",
                     "spectral"="Spectral Embedding")


    space.description <- c("preprocessed"="Statistical procedure: none\nResults: variables selected by user + created variables\nUtility: scatterplot visualization\n",
                           "pca"="Statistical procedure: linear transformation\nResults: decorrelated features\nUtility 1: dimensionality reduction\nUtility 2: remove redundant/uninformative features",
                           "spectral"="Statistical procedure: non linear transformation\nResults: spectrum of the data similarity matrix\nUtility 1: dimensionality reduction\nUtility 2: processing of non convex data")


    # Import Preprocessing frame
    preprocessingFrametext <- makeTitle("IMPORT PREPROCESSING / VISUALIZE DATA SAMPLE (OPTIONAL)")
    preprocessingFrame <- tkwidget(win1.nb$env$preprocess, "labelframe", text = preprocessingFrametext, font = fontTitleFrame, padx =  padx, pady = pady, relief = "flat")
    tkEmptyLine(win1.nb$env$preprocess, row=0)
    tkgrid(preprocessingFrame, row = 1, pady=pady, sticky = "we")

    # Select the Preprocessing file
    preprocessingName <- tktext(preprocessingFrame, bg="white", font="courier", width=7*RclusTool.env$param$visu$size, height=2, font = fontFrame, state="disabled")

    prepro.env$refreshPreprocessingName <- function()
    {
        tkconfigure(preprocessingName, state="normal") 
        tkdelete(preprocessingName, "1.0", "end")
        tkinsert(preprocessingName,"end", prepro.env$ProcessFile)
        tkconfigure(preprocessingName, state="disabled") 
    }

    # Import config file
    onImport <- function() {
        ProcessFile <- tclvalue(tkgetOpenFile(filetypes = "{{csv Files} {.csv}}"))
        prepro.env$ProcessFile <- ProcessFile
        message(paste("Import:", ProcessFile))
        if (!nchar(ProcessFile)) {
            tkmessageBox(message = "No file selected!")
        } else {
            # Call 'loadPreprocessFile' function
            operations <- loadPreprocessFile(ProcessFile)
            # Call 'applyPreprocessing' function
            new.data.sample <- applyPreprocessing(RclusTool.env$data.sample, operations, RclusTool.env, reset=TRUE)
            if (!is.null(new.data.sample)) {
                RclusTool.env$data.sample <- new.data.sample
            } else {
                onReset()
                return()
            }

            # Refresh GUI
            initPreprocessTab(RclusTool.env = RclusTool.env, reset=TRUE, readConfig=TRUE)
            # Display informations in console
            messageConsole(paste("----- Preprocessing -----\n",
                                           "Filename:  ", basename(ProcessFile), "\n",
                                           "Number of objects:  ", dim(RclusTool.env$data.sample$features[["preprocessed"]]$x)[1], "\n",
                                           "Number of features:  ", dim(RclusTool.env$data.sample$features[["preprocessed"]]$x)[2], 
                                           "\n\n", sep = ""), RclusTool.env=RclusTool.env)

            initUnsupTab(RclusTool.env = RclusTool.env, reset=TRUE)

            initSemisupTab(RclusTool.env = RclusTool.env, reset=TRUE)

            initSupTab(RclusTool.env = RclusTool.env, reset=TRUE)
        }
    }

    butImport <- tk2button(preprocessingFrame, text = "Import Preprocessing", image = "csvFile", compound = "left", width = 20, command = onImport)
    tkgrid(butImport, row = 2, column = 0, padx = padx)
    tkgrid(preprocessingName, padx = padx.m, row = 2, column= 1)

    # Parameter selection frame
    parameterFrametext <- makeTitle("VARIABLE SELECTION")
    parameterFrame <- tkwidget(win1.nb$env$preprocess, "labelframe", text = parameterFrametext, font = fontTitleFrame, padx =  padx, pady = pady, relief = "flat")
    tkEmptyLine(win1.nb$env$preprocess, row=2)
    tkgrid(parameterFrame, row = 3, pady=pady, sticky = "we")
    #tkgrid.rowconfigure(win1.nb$env$preprocess, parameterFrame, weight=1)

    # First listbox with all parameters
    dataVarList <- tk2listbox(parameterFrame, selectmode = "multiple", activestyle = "dotbox",
                              height = 10, width = 27, autoscroll = "none", background = "white")
    tkgrid(tk2label(parameterFrame, text = "Dataset variables:"), row = 1, column = 0, padx = padx, pady = c(pady,0), sticky = "w")
    tkgrid(dataVarList, row = 2, column = 0, padx = padx)

    # Second listbox with selected parameters
    modelVarList <- tk2listbox(parameterFrame, selectmode = "multiple", activestyle = "dotbox", 
                               height = 10, width = 27, autoscroll = "none", background = "white")
    tkgrid(tk2label(parameterFrame, text = "Variables used for classification"), row = 1, column = 2, pady = c(pady,0), sticky = "w")
    tkgrid(modelVarList, row = 2, column = 2, sticky = "s")


    # What kind of CSV file for features
    FeaturesAdvice <- tkwidget(parameterFrame, "labelframe", 
                               text = "Variable statistics",
                               padx = padx, pady = pady, relief = "groove")
    FeaturesAdviceText <- tk2label(FeaturesAdvice, text = "Inspect variable statistics\nby double-clicking on its name\nin dataset variables section", width = 30)                   
    tkconfigure(FeaturesAdvice,font = fontFrame)
    tkconfigure(FeaturesAdviceText,font = fontFrame)
    tkgrid(FeaturesAdvice, columnspan = 1, column = 3, row = 2, padx=padx.m)
    tkgrid(FeaturesAdviceText)

    # remove all features from both lists: available and selected features
    eraseFeaturesList <- function()
    {
        if (length(prepro.env$featName))
        {
            sapply(1:size(dataVarList), function(x) tkdelete(dataVarList, "end"))
            sapply(1:size(modelVarList), function(x) tkdelete(modelVarList, "end"))
        }
    }

    # fill both feature lists
    prepro.env$buildFeaturesList <- function() {
        eraseFeaturesList()
        sapply(prepro.env$featName$unselect, function(x) tkinsert(dataVarList, "end", x))
        sapply(prepro.env$featName$select, function(x) tkinsert(modelVarList, "end", x))
        #tkselection.set(dataVarList, 0)
    }


    # select one or several feature(s) (from available features list)
    selectFeatures <- function(index=c(), all=FALSE)
    {
        if (all)
        {
            prepro.env$featName$select <- c(prepro.env$featName$select, prepro.env$featName$unselect)
            prepro.env$featName$unselect <- c()
        } else {
            if (!length(index))
                return()
            prepro.env$featName$select <- c(prepro.env$featName$unselect[index],prepro.env$featName$select)
            prepro.env$featName$unselect <- prepro.env$featName$unselect[-index]
        }
        prepro.env$buildFeaturesList()
    }

    # remove one or several feature(s)
    removeFeatures <- function(index=c(), all=FALSE)
    {
        if (all)
        {
            prepro.env$featName$unselect <- c(prepro.env$featName$select, prepro.env$featName$unselect)
            prepro.env$featName$select <- c()
        } else {
            if (!length(index))
                return()
            prepro.env$featName$unselect <- c(prepro.env$featName$select[index], prepro.env$featName$unselect)
            prepro.env$featName$select <- prepro.env$featName$select[-index]
        }
        prepro.env$buildFeaturesList()
    }


    # add operation
    addOperation <- function(parameterList)
    {
        mat <- formatParameterList(parameterList)
        name <- row.names(mat)
        matchName <- matchNames(row.names(mat), row.names(prepro.env$featureOperations))
        if (!is.null(matchName)) {
            prepro.env$featureOperations[name,] <- mat
        } else
            prepro.env$featureOperations <- rbind(prepro.env$featureOperations, mat)
        mat
    }

    # remove operations ; type may be equal to "select"
    removeOperations <- function(type)
    {
        ind <- which(tolower(prepro.env$featureOperations[,1])==tolower(type))
        if (length(ind))
            prepro.env$featureOperations <- prepro.env$featureOperations[-ind,,drop=FALSE]
    }

    # add a new feature by processing other features: new operation added
    addFeature <- function(parameterList)
    {
        mat <- addOperation(parameterList)
        name <- row.names(mat)
        ind <- which(prepro.env$featName$unselect==name)
        if (!length(ind))
            prepro.env$featName$unselect <- c(name, prepro.env$featName$unselect)
        prepro.env$buildFeaturesList()
    }

    # add default feature selections for scatterplots
    defaultFeatures <- function(feature1, feature2)
    {
        feature <- c(feature1, feature2)
        len <- sapply(feature, nchar)!=0
        feature <- feature[len]
        if (!length(feature))
            return()
        prepro.env$defaultFeaturesHV[c("parH","parV")] <- feature
    }

    # Arrows frame
    arrowFrame <- tkwidget(parameterFrame, "labelframe", relief = "groove", borderwidth = 0)
    tkgrid(arrowFrame, column = 1, row = 2, sticky = "w", padx = c(padx,padx)) 


    # Add all parameters from first listbox to second listbox
    addAllButton <- tk2button(arrowFrame, text = "ALL =>", command = function() {
                                  # Check if a value is selected
                                  selectFeatures(all=TRUE)
                               }) 
    tkgrid(addAllButton)
    # Add one or all parameters from first listbox to second listbox
    addButton <- tk2button(arrowFrame, text = "=>", command = function() {
                               # Check if a value is selected
                               selection <- as.numeric(tkcurselection(dataVarList)) + 1

                               if (length(selection)) { 
                                   selectFeatures(selection)
                               } else { 
                                   tkmessageBox(message = "No parameter selected!", type = "ok", icon = "info", 
                                                title = "Warning!") } #No parameter selected
                               }) 
    tkgrid(addButton)
    tkgrid(tklabel(arrowFrame, text = "                  "))

    # Remove one parameter from second listbox
    removeButton <- tk2button(arrowFrame, text = "<=", command = function() {
                                  #Check if a parameter is selected
                                  if (tclvalue(tkcurselection(modelVarList)) != "") {
                                      #Search the selected parameter
                                      selection <- as.numeric(tkcurselection(modelVarList)) + 1
                                      removeFeatures(selection)   
                                  } else { 
                                      tkmessageBox(message = "No parameter selected!", type = "ok", icon = "info", 
                                                   title = "Warning!") 
                                  } #No parameter selected
                               })
    tkgrid(removeButton)
    # Delete all parameters from second listbox
    removeAllButton <- tk2button(arrowFrame, text = "ALL <=", command = function() {
                                     # Check if a value is selected
                                     removeFeatures(all=TRUE)
                               })
    tkgrid(removeAllButton)

    # Plot boxplot and histogram for selected parameter (double-click)
    onVarDescription <- function() {
        if (tclvalue(tkcurselection(dataVarList)) != "") {
            #Search selected parameter
            selection <- as.numeric(tkcurselection(dataVarList)) + 1
            if (length(selection)>1) {
                selection <- selection[1]
                tkselection.clear(dataVarList, selection[1], "end")
                tkmessageBox(message = "Please select a unique feature to display.", icon = "warning", type = "ok")
            } 
            if (length(selection)==0) {
                tkmessageBox(message = "Please select a feature to display.", icon = "warning", type = "ok")
                return()
            }
            paramSelect <- prepro.env$featName$unselect[selection]

            if (!is.null(RclusTool.env$data.sample$features$preprocessed$x[[paramSelect]])){
                # Plot boxplot in tab
                tk2delete.notetab(win2.nb)
                analyzePlot(win2.nb, data.sample = RclusTool.env$data.sample, selectedVar = paramSelect, 
                            type = "boxplot", hscale = RclusTool.env$param$visu$hscale, fontsize=RclusTool.env$param$visu$size)
            } else {
                tkmessageBox(message = "Please valid the preprocessing to see the boxplot of this variable.", icon = "warning", type = "ok")
            }
        }
    }
    tkbind(dataVarList, "<Double-Button-1>", onVarDescription)

    # Create new parameters button
    createVar <- tkbutton(parameterFrame, text = "Create", width = 5, compound='center', command = function() {
                              # Create new window for the parameter creation
                              creatett <- tktoplevel()
                              createVar.env <- new.env()
                              createVar.env$opMode <- NULL

                              tktitle(creatett) <- "Create"
                              tkgrid(tk2label(creatett, text = "     "), row = 1, sticky = "w")

                              paramNamesTemp <- prepro.env$featName$unselect
                              # First listbox with all parameters
                              varlst1 <- tk2listbox(creatett, selectmode = "single", activestyle = "dotbox", 
                                                    height = 10, width = 27, autoscroll = "none", background = "white")
                              tkgrid(tk2label(creatett, text = "Select the first parameter", justify = "left"),
                                     row = 2, column = 0, sticky = "w")
                              tkgrid(varlst1, row = 3, column = 0, rowspan = 4, sticky = "w")

                              # Second listbox with all parameters
                              varlst2 <- tk2listbox(creatett, selectmode = "single", activestyle = "dotbox", 
                                                    height = 10, width = 27, autoscroll = "none", background = "white")
                              tkgrid(tk2label(creatett, text = "Select the second parameter", justify = "left"),
                                     row = 2, column = 2, sticky = "w")
                              tkgrid(varlst2, row = 3, column = 2, rowspan = 4, sticky = "w")
                              tkgrid(tk2label(creatett, text = "     "), row = 7, column = 0)

                              for (param in paramNamesTemp) {
                                  tkinsert(varlst1, "end", param)
                                  tkinsert(varlst2, "end", param)
                              }

                              # Initialize selection in the two listboxes
                              if (tclvalue(tkcurselection(dataVarList)) != "" && !grepl(" ", tclvalue(tkcurselection(dataVarList)))) {
                                  #Search selected parameter
                                  selection <- as.numeric(tkcurselection(dataVarList))
                                  tkselection.set(varlst1, selection)
                              }
                              tkselection.set(varlst2, 0)

                              # Select the first parameter (double-click)
                              onVarSelection1 <- function() {
                                  tkgrid(tk2label(creatett, text = "                    "), row = 7, column = 0)
                                  tkgrid(tk2label(creatett, text = "                    "), row = 7, column = 0, sticky = "e")
                                  tkgrid(tk2label(creatett, text = "                    "), row = 7, column = 0, sticky = "w")
                                  tkgrid(tk2label(creatett, text = paramNamesTemp[as.numeric(tkcurselection(varlst1)) + 1], justify = "left"),
                                         row = 7, column = 0, sticky = "e")
                              }
                              tkbind(varlst1, "<ButtonRelease-1>", onVarSelection1)

                              # Select the second parameter (double-click)
                              onVarSelection2 <- function() {
                                  tkgrid(tk2label(creatett, text = "                    "), row = 7, column = 2)
                                  tkgrid(tk2label(creatett, text = "                    "), row = 7, column = 2, sticky = "e")
                                  tkgrid(tk2label(creatett, text = "                    "), row = 7, column = 2, sticky = "w")
                                  tkgrid(tk2label(creatett, text = paramNamesTemp[as.numeric(tkcurselection(varlst2)) + 1], justify = "left"),
                                         row = 7, column = 2, sticky = "w")
                              }
                              tkbind(varlst2, "<ButtonRelease-1>", onVarSelection2)

                              # Select multiplication operation
                              onMultiplication <- function() {
                                  tkgrid(tk2label(creatett, text = "*", justify = "left"),
                                         row = 7, column = 1)
                                  creatVar.env$opMode <- "*"
                              }
                              butMultiplication <- tk2button(creatett, text = "*", width = -6, command = onMultiplication)
                              tkgrid(butMultiplication, row = 3, column = 1, sticky = "w")

                              # Select division operation
                              onDivision <- function() {
                                  tkgrid(tk2label(creatett, text = "/", justify = "left"),
                                         row = 7, column = 1)
                                  creatVar.env$opMode <- "/"
                              }
                              butDivision <- tk2button(creatett, text = "/", width = -6, command = onDivision)
                              tkgrid(butDivision, row = 4, column = 1, sticky = "w")

                              # Select addition operation
                              onAddition <- function() {
                                  tkgrid(tk2label(creatett, text = "+", justify = "left"),
                                         row = 7, column = 1)
                                  creatVar.env$opMode <- "+"
                              }
                              butAddition <- tk2button(creatett, text = "+", width = -6, command = onAddition)
                              tkgrid(butAddition, row = 5, column = 1, sticky = "w")

                              # Select substraction operation
                              onSoustraction <- function() {
                                  tkgrid(tk2label(creatett, text = "-", justify = "left"),
                                         row = 7, column = 1)
                                  creatVar.env$opMode <- "-"
                              }
                              butSoustraction <- tk2button(creatett, text = "-", width = -6, command = onSoustraction)
                              tkgrid(butSoustraction, row = 6, column = 1, sticky = "w")

                              # Apply operation
                              onOK <- function() {
                                  paramNameTemp1 <- paramNamesTemp[as.numeric(tkcurselection(varlst1)) + 1]
                                  paramNameTemp2 <- paramNamesTemp[as.numeric(tkcurselection(varlst2)) + 1]

                                  if ((!is.null(paramNameTemp1)) && (!is.null(paramNameTemp2)) && (!is.null(creatVar.env$opMode))) {
                                      # Check if parameter already exists
                                      addFeature(list(creatVar.env$opMode, paramNameTemp1, paramNameTemp2)) #no log
                                      tkdestroy(creatett)
                                  } else tkmessageBox(message = "Parameter already created!", icon = "warning", type = "ok")

                              }
                              butOK <- tk2button(creatett, text = "OK", width = -6, command = onOK)
                              tkgrid(butOK, row = 8, column = 0, sticky = "e")

                              onCancel <- function() {
                                  tkdestroy(creatett)
                              }
                              butCancel <- tk2button(creatett, text = "Cancel", width = -6, command = onCancel)
                              tkgrid(butCancel, row = 8, column = 2, sticky = "w")
                            })
    tkgrid(createVar, row = 3, column = 0, sticky = "ne")

    # Transform parameters button
    transformVar <- tkbutton(parameterFrame, text = "Transform", width = 8, command = function() {
                                 if (tclvalue(tkcurselection(dataVarList)) != "") {
                                     # Create new window for the parameter transformation
                                     transformtt <- tktoplevel()
                                     tktitle(transformtt) <- "Transform"
                                     #Search the selected parameter
                                     selection <- as.numeric(tkcurselection(dataVarList)) + 1
                                     texte <- prepro.env$featName$unselect[selection]
                                     tkgrid(tk2label(transformtt, text = "     ", justify = "left"),
                                            row = 1, columnspan = 3, sticky = "w")
                                     tkgrid(tk2label(transformtt, text = texte, justify = "left"),
                                            row = 2, columnspan = 3, sticky = "w")
                                     tkgrid(tk2label(transformtt, text = "     ", justify = "left"),
                                            row = 3, columnspan = 3, sticky = "w")

                                     # All parameters selected for transformation
                                     # ??? a modifier
                                     #if (!selection) {
                                     #selection <- 1:length(prepro.env$featName$unselect)
                                     #tkgrid(tk2label(transformtt, text = "All parameters selected for data transformation", justify = "left"),
                                     #row = 2, columnspan = 3, sticky = "w")
                                     #} else { # Only one parameter selected for transformation
                                     #        tkgrid(tk2label(transformtt, text = paramNames[selection], justify = "left"),
                                     #               row = 2, columnspan = 3, sticky = "w")
                                     # Display NA informations about parameters
                                     #        tkgrid(tk2label(transformtt, text = paste("-", length(which(is.na(RclusTool.env$data.sample$features[["preprocessed"]]$x[, selection-1]))), "NA value(s)", sep = " "), 
                                     #                        justify = "left"), row = 2, column = 4, sticky = "w")
                                     # Display <0 informations about parameters
                                     #       tkgrid(tk2label(transformtt, text = paste("-", length(which(RclusTool.env$data.sample$features[["preprocessed"]]$x[, selection-1]<0)), "negative value(s)", sep = " "), 
                                     #                        justify = "left"), row = 3, column = 4, sticky = "w")
                                     # Display Inf informations about parameters
                                     #        tkgrid(tk2label(transformtt, text = paste("-", length(which(is.infinite(RclusTool.env$data.sample$features[["preprocessed"]]$x[, selection-1]))), "infinite value(s)", sep = " "), 
                                     #                        justify = "left"), row = 4, column = 4, sticky = "w")
                                     #        tkgrid(tk2label(transformtt, text = "     ", justify = "left"), row = 5, columnspan = 3)
                                     #}
                                 }

                                 # Logarithm transformation
                                 onLog <- function() {
                                     op <- "log"
                                     sapply(prepro.env$featName$unselect[selection], function(feat) addFeature(list(op,feat)))
                                     tkdestroy(transformtt)
                                 }
                                 butLog <- tk2button(transformtt, text = "log10", width = -6, command = onLog)
                                 tkgrid(butLog, row = 6, column = 3, sticky = "e")
                            })
    tkgrid(transformVar, row = 3, column = 0, sticky = "n")

    # Filter button
    filterVar <- tkbutton(parameterFrame, text = "Filter", width = 5, command = function() {
                              filterVar.env <- new.env()
                              filterVar.env$opMode <- NULL

                              if (tclvalue(tkcurselection(dataVarList)) != "") {
                                  #Search the selected parameter
                                  selection <- as.numeric(tkcurselection(dataVarList)) + 1

                                  # Create new window for the parameter transformation
                                  filtertt <- tktoplevel()
                                  tktitle(filtertt) <- "Filter"
                                  tkgrid(tk2label(filtertt, text = "     "), row = 1, sticky = "w")
                                  tkgrid(tk2label(filtertt, text = "Select the values considered as outliers"), row = 2, columnspan = 3)
                                  tkgrid(tk2label(filtertt, text = "     "), row = 3, sticky = "w")
                                  tkgrid(tk2label(filtertt, text = prepro.env$featName$unselect[selection], justify = "left"),
                                         row = 5, column = 0, sticky = "w")

                                  # Display informations about parameter (min and max values)
                                  #        featuresClean <- RclusTool.env$data.sample$features[["preprocessed"]]$x[RclusTool.env$data.sample$id.clean, ,drop=FALSE]
                                  #        minVal <- min(featuresClean[, which(colnames(featuresClean) == paramNames[selection])])
                                  #        maxVal <- max(featuresClean[, which(colnames(featuresClean) == paramNames[selection])])
                                  #        tkgrid(tk2label(filtertt, text = paste("[", round(minVal, digits = 4), " ; ", 
                                  #                                              round(maxVal, digits = 4), "]", sep = ""), justify = "left"),
                                  #               row = 6, column = 2, sticky = "e")

                                  # Select equal operation
                                  onEqual <- function() {
                                      tkgrid(tk2label(filtertt, text = "=", justify = "left"),
                                             row = 4, column = 2)
                                      filterVar.env$opMode <- "="
                                  }
                                  butEqual <- tk2button(filtertt, text = "=", width = -6, command = onEqual)
                                  tkgrid(butEqual, row = 4, column = 1, sticky = "w")

                                  # Select 'less than' operation
                                  onLower <- function() {
                                      tkgrid(tk2label(filtertt, text = "<", justify = "left"),
                                             row = 4, column = 2)
                                      filterVar.env$opMode <- "<"
                                  }
                                  butLower <- tk2button(filtertt, text = "<", width = -6, command = onLower)
                                  tkgrid(butLower, row = 5, column = 1, sticky = "w")

                                  # Select 'greater than' operation
                                  onUpper <- function() {
                                      tkgrid(tk2label(filtertt, text = ">", justify = "left"),
                                             row = 4, column = 2)
                                      filterVar.env$opMode <- ">"
                                  }
                                  butUpper <- tk2button(filtertt, text = ">", width = -6, command = onUpper)
                                  tkgrid(butUpper, row = 6, column = 1, sticky = "w")

                                  # Enter the threshold value
                                  tcl.threshold <- tclVar(as.character(0))
                                  tk.threshold <- tkentry(filtertt, textvariable=tcl.threshold, width=6, state="normal", background = "white")
                                  tkconfigure(tk.threshold,font = fontFrame)
                                  tkgrid(tk.threshold, row = 5, column=2)
                                  tkgrid(tk2label(filtertt, text = "     "), row = 7, sticky = "w")

                                  # Apply filtering operation
                                  onOK <- function() {
                                      n <- as.numeric(tclvalue(tcl.threshold))
                                      param <- prepro.env$featName$unselect[as.numeric(tkcurselection(dataVarList)) + 1]

                                      if (!is.null(filterVar.env$opMode))
                                      {
                                          addOperation(list("outlier",param,filterVar.env$opMode,n))
                                          tkdestroy(filtertt)
                                      } else tkmessageBox(message = "You have to select a comparison operator!", icon = "warning", type = "ok")
                                  }
                                  butOK <- tk2button(filtertt, text = "OK", width = -6, command = onOK)
                                  tkgrid(butOK, row = 8, column = 0)

                                  onCancel <- function() {
                                      tkdestroy(filtertt)
                                  }
                                  butCancel <- tk2button(filtertt, text = "Cancel", width = -6, command = onCancel)
                                  tkgrid(butCancel, row = 8, column = 2)
                              }
                            })
    tkgrid(filterVar, row = 3, column = 0, sticky = "nw")
    colDefPlot <- 2  # Column for 'Configure default plot' button in parameterFrame

    # Space selection frame for expert mode
    # First listbox with all spaces
    spaceFrametext <- makeTitle("FEATURE SPACE TRANSFORMATION")
    spaceFrame <- tkwidget(win1.nb$env$preprocess, "labelframe", text = spaceFrametext, font = fontTitleFrame, padx = padx, pady = pady, relief = "flat")
    AdviceFrame <- tkwidget(spaceFrame, "labelframe", text = "", padx = padx.m, pady = pady, relief = "groove")
    tkgrid(AdviceFrame, rowspan = 3, column = 6, row = 6)


    AdviceFrameText <- tk2label(AdviceFrame, text = "Space description", width=0)
    tkgrid(AdviceFrameText, sticky = "w")

    spaceList <- tk2listbox(spaceFrame, selectmode = "single", activestyle = "dotbox",
                            height = 3, width = 45, autoscroll = "none", background = "white")

    getSelectedSpace <- function()
    {
        space <- NULL
        selection <- tclvalue((tkcurselection(spaceList)))
        if (selection!="")
            space <- prepro.env$available.spaces[as.numeric(selection)+1] 
        space
    }

    eraseSpaceList <- function()
    {
        sapply(1:size(spaceList), function(x) tkdelete(spaceList, "end"))
    }

    setSpaceList <- function(available.spaces, old)
    {
        sapply(available.spaces, function(s) tkinsert(spaceList, "end", s))
        ind <- which(old==available.spaces)
        if (!length(ind))
            ind <- 1
        tkselection.set(spaceList, ind-1)
    }


    prepro.env$updateSpaceList <- function(reset=FALSE) {
        prepro.env$available.spaces <- listDerivableFeatureSpaces(scaling=tclvalue(prepro.env$tcl.scaling.check)=="1",
                                                                  pca=tclvalue(prepro.env$tcl.pca.check) == "1",
                                                                  spectral=tclvalue(prepro.env$tcl.spectral.check) == "1",
                                                                  RclusTool.env=RclusTool.env)
        if (!reset) {
            old <- getSelectedSpace()
        } else {
            old <- prepro.env$featSpace
        }

        eraseSpaceList()
        setSpaceList(prepro.env$available.spaces, old)
        prepro.env$featSpace <- getSelectedSpace()
    }

    if (RclusTool.env$gui$user.type == "expert") {
        # Space selection frame
        tkEmptyLine(win1.nb$env$preprocess, row=4)
        tkgrid(spaceFrame, row = 5, sticky = "we", pady=pady)

        # Check scaling
        OnScalingCheck <- function() {
            prepro.env$updateSpaceList()
        }

        # Check sampling
        OnSamplingCheck <- function() {
            prepro.env$updateSpaceList()
            if (tclvalue(prepro.env$tcl.sampling.check)=="1") {
                tkconfigure(tk.sampling.size, state="normal")
            } else
                tkconfigure(tk.sampling.size, state="disabled")
        }

        # Describe Spaces
        prepro.env$onSpaceSelection <- function(pca.check=FALSE, spectral.check=FALSE, reset=FALSE) {
            prepro.env$updateSpaceList(reset=reset)

            space.type <- "preprocessed" 
            if (pca.check)
                space.type <- "pca"
            if (spectral.check)
                space.type <- "spectral"

            tkconfigure(AdviceFrame, text=space.title[space.type],font=fontFrame)
            tkconfigure(AdviceFrameText, text=space.description[space.type],font = fontFrame)
        }

        OnPcaCheck <- function()
        {
            selected <- tclvalue(prepro.env$tcl.pca.check)=="1"
            prepro.env$onSpaceSelection(pca.check=selected)
        }

        OnSpectralCheck <- function()
        {
            selected <- tclvalue(prepro.env$tcl.spectral.check)=="1"
            prepro.env$onSpaceSelection(spectral.check=selected)
        }

        #tkbind(spaceList, "<ButtonRelease-1>", OnModifSpace)

        tkgrid(tk2label(spaceFrame, text = "New spaces to build:"), row = 5, column = 1, pady = c(pady,0),padx =  padx, sticky = "w")

        # PCA check button
        rc_pca <- tkcheckbutton(spaceFrame, variable=prepro.env$tcl.pca.check, text = "PCA - Reduced features (0=auto)",
                                command=OnPcaCheck)
        tkgrid(tk2label(spaceFrame), rc_pca, row=6, column=1, padx = padx, sticky="w")

        # Spectral Embedding check button
        rc_spectral <- tkcheckbutton(spaceFrame, variable = prepro.env$tcl.spectral.check, text = "Spectral Embedding",
                                     command=OnSpectralCheck)

        tkgrid(tk2label(spaceFrame), rc_spectral, row=7, column=1, padx = padx, sticky="w")

        tkgrid(tk2label(spaceFrame, text = "Scaling:"), row = 8, column = 1, padx = padx, sticky = "w")

        # Scaling check button
        tk.scaling.check <- tkcheckbutton(spaceFrame, variable = prepro.env$tcl.scaling.check, text = "Scaling", 
                                          command=OnScalingCheck)
        tkgrid(tk2label(spaceFrame), tk.scaling.check, row=9, column=1, padx = padx, sticky="w")

        #pca
        tk.pca.nb.dims <- tkentry(spaceFrame, textvariable=prepro.env$tcl.pca.nb.dims, width=2, 
                                  state="normal", background = "white")
        tkconfigure(tk.pca.nb.dims,font = fontFrame)

        #sampling
        tkgrid(tk2label(spaceFrame, text = "Selection of a subset:"), row = 10, column = 1, pady = c(pady,0), padx = padx, sticky = "w")

        tk.sampling.check <- tkcheckbutton(spaceFrame, text="Sampling", variable=prepro.env$tcl.sampling.check,
                                           command=OnSamplingCheck)
        tkgrid(tk2label(spaceFrame), tk.sampling.check, row=11, column=1, padx = padx, sticky="w")

        tk.sampling.size <- tkentry(spaceFrame, textvariable=prepro.env$tcl.sampling.size, width=5, 
                                    state="disabled", background = "white")

        tkconfigure(tk.sampling.size,font = fontFrame)

        # Positioning
        tkgrid(tk.pca.nb.dims, row=6, column=2, sticky="w")
        tkgrid(tk.sampling.size, row=11, column=2, sticky="w")

        tkgrid(tk2label(spaceFrame, text = "Space used by classification processes:"), row = 13, column = 1, pady = c(pady,0), padx = padx, sticky = "w")
        tkgrid(spaceList, row = 14, column = 1, padx = padx)

    }


    # Configure the default plot
    defaultPlot <- tkbutton(parameterFrame, text = "Configure default plot", width = 20, command = function() {
                                # Create new window for plot configuration
                                defaulttt <- tktoplevel()
                                tktitle(defaulttt) <- "Configure"
                                tkgrid(tk2label(defaulttt, text = "     "), row = 1, sticky = "w")

                                # Default axes frame
                                axisFrame <- tkwidget(defaulttt, "labelframe", text = "X-Y SCATTER PLOT", font = fontFrame, padx = padx, pady = pady, relief = "groove")
                                tkgrid(axisFrame, row = 2, columnspan = 2, sticky = "w")
                                tkgrid(tk2label(axisFrame, text = "     "), row = 1, sticky = "w")

                                # First Listbox with all parameters
                                ## paramNamesTemp <- gsub(" \\(log\\)", "", prepro.env$featName$select) # est-ce encore utile ???
                                paramNamesTemp <- c(prepro.env$featName$select, prepro.env$featName$unselect)
                                varlst1 <- tk2listbox(axisFrame, selectmode = "single", activestyle = "dotbox", 
                                                      height = 10, width = 27, autoscroll = "none", background = "white")
                                tkgrid(tk2label(axisFrame, text = "Select the X-axis to plot", justify = "left"),
                                       row = 2, column = 0)
                                tkgrid(varlst1, row = 3, column = 0, rowspan = 4, sticky = "w")

                                # Second Listbox with all parameters
                                varlst2 <- tk2listbox(axisFrame, selectmode = "single", activestyle = "dotbox", 
                                                      height = 10, width = 27, autoscroll = "none", background = "white")
                                tkgrid(tk2label(axisFrame, text = "Select the Y-axis to plot", justify = "left"),
                                       row = 2, column = 2)
                                tkgrid(varlst2, row = 3, column = 2, rowspan = 4, sticky = "w")
                                tkgrid(tk2label(axisFrame, text = "     "), row = 7, column = 0)

                                for (param in paramNamesTemp) {
                                    tkinsert(varlst1, "end", param)
                                    tkinsert(varlst2, "end", param)
                                }

                                # Display and select default axes 
                                if (!is.null(prepro.env$defaultFeaturesHV["parH"])) {
                                    tkgrid(tk2label(axisFrame, text = paste("X-axis:", prepro.env$defaultFeaturesHV["parH"]), 
                                                    justify = "left"), row = 7, column = 0, sticky = "e")
                                    tkselection.set(varlst1, grep(prepro.env$defaultFeaturesHV["parH"], paramNamesTemp, fixed=TRUE)[1]-1)
                                } else tkselection.set(varlst1, 0)
                                if (!is.null(prepro.env$defaultFeaturesHV["parV"])) {
                                    tkgrid(tk2label(axisFrame, text = paste("Y-axis:", prepro.env$defaultFeaturesHV["parV"]), 
                                                    justify = "left"), row = 7, column = 2, sticky = "w")
                                    tkselection.set(varlst2, grep(prepro.env$defaultFeaturesHV["parV"], paramNamesTemp, fixed=TRUE)[1]-1)
                                } else tkselection.set(varlst2, 1)

                                # Validate the selection for default axes
                                onValidDef <- function() {
                                    paramNameTemp1 <- paramNamesTemp[as.numeric(tkcurselection(varlst1)) + 1]
                                    paramNameTemp2 <- paramNamesTemp[as.numeric(tkcurselection(varlst2)) + 1]
                                    defaultFeatures(paramNameTemp1,paramNameTemp2)
                                    tkdestroy(defaulttt)
                                }
                                # Select the axis (parH and X-axis)
                                #tkbind(varlst1, "<ButtonRelease-1>", onValidDef)
                                #tkbind(varlst2, "<ButtonRelease-1>", onValidDef)

                                butValidDef <- tk2button(axisFrame, text = "Valid & Close", width = -6, command = onValidDef)
                                tkgrid(butValidDef, row = 8, column = 1)

                                if (!is.null(RclusTool.env$data.sample$profiles)) {
                                    # Color frame (for signals only)
                                    colorFrame <- tkwidget(defaulttt, "labelframe", text = "SIGNALS COLORS", font = fontFrame, padx = padx, pady = pady, relief = "groove")
                                    tkgrid(tk2label(defaulttt, text = "     "), row = 1, column = 2, sticky = "w")
                                    tkgrid(colorFrame, row = 2, column = 3, columnspan = 2, sticky = "w")
                                    tkgrid(tk2label(colorFrame, text = "     "), row = 1, sticky = "w")

                                    # First listbox with all signals
                                    siglst <- tk2listbox(colorFrame, selectmode = "single", activestyle = "dotbox", 
                                                         height = 10, width = 27, autoscroll = "none", background = "white")
                                    tkgrid(tk2label(colorFrame, text = "Select the signal", justify = "left"),
                                           row = 2, column = 0)
                                    tkgrid(siglst, row = 3, column = 0, rowspan = 4, sticky = "w")

                                    # Second listbox with colors
                                    colors <- RclusTool.env$param$visu$palette.colors
                                    collst <- tk2listbox(colorFrame, selectmode = "single", activestyle = "dotbox", 
                                                         height = 10, width = 27, autoscroll = "none", background = "white")
                                    tkgrid(tk2label(colorFrame, text = "Select the color", justify = "left"),
                                           row = 2, column = 2)
                                    tkgrid(collst, row = 3, column = 2, rowspan = 4, sticky = "w")
                                    tkgrid(tk2label(colorFrame, text = "     "), row = 7, column = 0)

                                    for (signal in signal.names) 
                                        tkinsert(siglst, "end", signal)
                                    tkselection.set(siglst, 0)
                                    for (color in colors) 
                                        tkinsert(collst, "end", color)
                                    #tkselection.set(collst, 0)


                                    # Display signals and associated colors
                                    onColorDescription <- function() {
                                        tkgrid(tk2label(colorFrame, text = "     "), row = 3, column = 3)
                                        tkgrid(tk2label(colorFrame, text = paste(names(prepro.env$signalColor), 
                                                                                 collapse = "\n")), row = 3, column = 4)
                                        tkgrid(tk2label(colorFrame, text = paste(prepro.env$signalColor,
                                                                                 collapse = "\n")), row = 3, column = 5)
                                    }

                                    # Select the signal
                                    onSigSelection <- function() {
                                        sig <- signal.names[as.numeric(tkcurselection(siglst)) + 1]
                                        tkselection.clear(collst, 0, "end")
                                        tkselection.set(collst, grep(prepro.env$signalColor[sig], colors, fixed=TRUE)[1]-1)
                                        tkgrid(tk2label(colorFrame, text = "                    "), row = 7, column = 0)
                                        tkgrid(tk2label(colorFrame, text = "                    "), row = 7, column = 0, sticky = "w")
                                        tkgrid(tk2label(colorFrame, text = "                    "), row = 7, column = 0, sticky = "e")
                                        tkgrid(tk2label(colorFrame, text = sig, justify = "left"),
                                               row = 7, column = 0, sticky = "e")
                                    }
                                    tkbind(siglst, "<ButtonRelease-1>", onSigSelection)

                                    # Select the color
                                    onColSelection <- function() {
                                        tkgrid(tk2label(colorFrame, text = "                    "), row = 7, column = 2)
                                        tkgrid(tk2label(colorFrame, text = "                    "), row = 7, column = 2, sticky = "e")
                                        tkgrid(tk2label(colorFrame, text = "                    "), row = 7, column = 2, sticky = "w")
                                        tkgrid(tk2label(colorFrame, text = paste(colors[as.numeric(tkcurselection(collst)) + 1]), justify = "left"),
                                               row = 7, column = 2, sticky = "w")
                                    }
                                    tkbind(collst, "<ButtonRelease-1>", onColSelection)
                                    tkgrid(tk2label(colorFrame, text = " IN "), row = 7, column = 1)

                                    #if (is.null(prepro.env$signalColor))
                                    #    resetSignalColors()
                                    onColorDescription()
                                    onSigSelection()

                                    # Validate the choice of signal and color
                                    onValidCol <- function() {
                                        sigNameTemp <- signal.names[as.numeric(tkcurselection(siglst)) + 1]
                                        colNameTemp <- colors[as.numeric(tkcurselection(collst)) + 1]
                                        prepro.env$signalColor[sigNameTemp] <- colNameTemp
                                        onColorDescription()
                                    }
                                    butValidCol <- tk2button(colorFrame, text = "Valid", width = -6, command = onValidCol)
                                    tkgrid(butValidCol, row = 8, column = 1)
                                }
                                tkgrid(tk2label(defaulttt, text = "     "), row = 3)
                                    })
    tkgrid(defaultPlot, row = 3, column = colDefPlot)

    onVisu <- function()
    {
        # Plot the dataset (with possibility to extract prototypes manually and to rename clusters)
        new.protos <- visualizeSampleClustering(RclusTool.env$data.sample, selection.mode = "prototypes",
                                                profile.mode="whole sample", wait.close=TRUE, RclusTool.env=RclusTool.env, fontsize=RclusTool.env$param$visu$size)
        # Update labels with renamed clusters
        RclusTool.env$data.sample <- updateClustersNames(RclusTool.env$data.sample, new.protos$prototypes)
        # Update clusters names in plots (if necessary)
        #tk2delete.notetab(win2.nb)
        #abdPlotTabsGUI(RclusTool.env)
        # Save prototypes (csv + image files in 'prototypes' directory)
        saveManualProtos(RclusTool.env$data.sample, new.protos$prototypes)
    }

    butVisu <- tk2button(preprocessingFrame, text = "Visualize Data Sample", image = "visualize", compound = "left", width = 20, command = onVisu)
    tkgrid(butVisu, row = 3, column = 0)

    # Export config in a csv file
    onExport <- function() {
        #operations <- buildConfigFile(RclusTool.env$data.sample$config)
        newProcessFile <- paste("_config", format(Sys.time(),'_%Y%m%d_%Hh%Mm%Ss'), ".csv", sep = "")
        message(paste("Export:", file.path(RclusTool.env$data.sample$files$results$preprocess, newProcessFile)))
        savePreprocess(filename.csv = newProcessFile, prepro.env$featureOperations, RclusTool.env$data.sample$files$results$preprocess)

        messageConsole(paste("----- Save Preprocessing file -----\n",
                                       "Folder: ", RclusTool.env$data.sample$files$results$preprocess, "\n",
                                       "Filename: ", newProcessFile,
                                       "\n\n", sep = ""), RclusTool.env=RclusTool.env)
    }
    buttonsFrame <- tkwidget(win1.nb$env$preprocess, "labelframe", text = "", font = fontFrame, padx = padx, pady = pady, relief = "flat")
    butExport <- tk2button(buttonsFrame, text = "Export preprocessing", image = "csvFile", compound = "left", width = 30, command = onExport)

    # Valid preprocessing
    onValid <- function() {
        if (tclvalue(prepro.env$tcl.spectral.check)=="1" && tclvalue(prepro.env$tcl.sampling.check)=="0" && RclusTool.env$data.sample$size>5000){
            msg <- "Are you sure you want to make spectral projection without sampling (It can take a long time...)?"
            res <- tkmessageBox(message = msg, icon = "warning", type = "yesno", default = "no")
            if (tclvalue(res) == "no") return(invisible(NULL))  
        }
        removeOperations("default")
        for (feat in prepro.env$defaultFeaturesHV)
            addOperation(list("default", feat))

        removeOperations("select")
        for (feat in prepro.env$featName$select)
            addOperation(list("select", feat))

        removeOperations("signalColor")
        for (color.name in names(prepro.env$signalColor))
            addOperation(list("signalColor", color.name, prepro.env$signalColor[color.name]))

        #projections to compute
        removeOperations("projection")
        if (tclvalue(prepro.env$tcl.pca.check)=="1")
            addOperation(list("projection","pca", tclvalue(prepro.env$tcl.pca.nb.dims)))
        if (tclvalue(prepro.env$tcl.spectral.check)=="1")
            addOperation(list("projection","spectral"))

        #scaling
        removeOperations("scaling")
        if (tclvalue(prepro.env$tcl.scaling.check)=="1"){
            addOperation(list("scaling", "on"))
        }

        #sampling
        removeOperations("sampling")
        if (tclvalue(prepro.env$tcl.sampling.check)=="1"){
            addOperation(list("sampling", tclvalue(prepro.env$tcl.sampling.size)))
        }

        #projection space to select
        removeOperations("space")
        if (RclusTool.env$gui$user.type=="expert")
        {
            prepro.env$featSpace <- getSelectedSpace()
        }
        addOperation(list("space", prepro.env$featSpace))

        printFeatureOperations <- prepro.env$featureOperations
        printFeatureOperations[is.na(printFeatureOperations)] <- " "
        printFeatureOperations <- as.data.frame(printFeatureOperations)
        printFeatureOperations <- toStringDataFrame(printFeatureOperations)
        printFeatureOperations <- unlist(strsplit(printFeatureOperations, split='arg4\n ', fixed=TRUE))[2]

        messageConsole(paste("----- Preprocessing -----\n",printFeatureOperations,'\n\n'), RclusTool.env=RclusTool.env)

        new.data.sample <- applyPreprocessing(RclusTool.env$data.sample, prepro.env$featureOperations, RclusTool.env, reset=TRUE)

        if (is.null(new.data.sample)) {
            onReset()
            return()
        } 

        RclusTool.env$gui$win1$env$authorization$prepro <- TRUE
        RclusTool.env$gui$win1$env$authorization$classif <- TRUE

        RclusTool.env$data.sample <- new.data.sample

        initUnsupTab(RclusTool.env = RclusTool.env, reset=TRUE)

        initSemisupTab( RclusTool.env = RclusTool.env, reset=TRUE)

        initSupTab(RclusTool.env = RclusTool.env, reset=TRUE)

        #Vizualisation of Correlation 
        tkconfigure(win1.nb$env$import, cursor = "watch")
        tk2delete.notetab(win2.nb)
        if (length(prepro.env$featName$select)>0){
            analyzePlot(win2.nb, data.sample = RclusTool.env$data.sample, selectedVar = prepro.env$featName$select, 
                        type = "Corr",hscale = RclusTool.env$param$visu$hscale, fontsize=RclusTool.env$param$visu$size)
        }
        # Plot correlation circle and Variance in the tabs
        if ((tclvalue(prepro.env$tcl.pca.check)=="1")){
            space.name <- grep("pca_full", names(RclusTool.env$data.sample$features), value=TRUE, fixed=TRUE)[1]
            analyzePlot(win2.nb, data.sample = RclusTool.env$data.sample, selectedVar = space.name, 
                        type = "pcaCorr", hscale = RclusTool.env$param$visu$hscale, fontsize=RclusTool.env$param$visu$size)
            analyzePlot(win2.nb, data.sample = RclusTool.env$data.sample, selectedVar = space.name, 
                        type = "pcaVar", hscale = RclusTool.env$param$visu$hscale, fontsize=RclusTool.env$param$visu$size)
        }
        if ((tclvalue(prepro.env$tcl.spectral.check)=="1")){
            space.name <- grep("spectral", names(RclusTool.env$data.sample$features), value=TRUE, fixed=TRUE)[1]
            analyzePlot(win2.nb, data.sample = RclusTool.env$data.sample, selectedVar = space.name, 
                        type = "gapSE", hscale = RclusTool.env$param$visu$hscale, fontsize=RclusTool.env$param$visu$size)
        }
        tkconfigure(win1.nb$env$import, cursor = "left_ptr")

    }

    butValid <- tk2button(buttonsFrame, text = "Validate preprocessing", image = "check", compound = "left", width = 30, command = onValid)

    # Reset preprocessing
    onReset <- function() {
        message("Preprocessing Reset")
        RclusTool.env$data.sample <- applyPreprocessing(RclusTool.env$data.sample, operations=NULL, RclusTool.env, reset=TRUE)
        initPreprocessTab(RclusTool.env = RclusTool.env, reset=TRUE, readConfig=FALSE)
        RclusTool.env$gui$win1$env$authorization$prepro <- TRUE
        RclusTool.env$gui$win1$env$authorization$classif <- FALSE
    }
    butReset <- tk2button(buttonsFrame, text = "Reset", image = "reset", compound = "left", width = -6, command = onReset)

    tkgrid(butExport, row = 0, column = 1)
    tkgrid(butValid, row = 0, column = 3)
    tkgrid(butReset, row = 0, column = 5)
    tkgrid(buttonsFrame, row=6)

}

#' function to initialize/refresh the 'preprocessTab' for data preprocessing (variables selection, transformation, creation, ...)
#' @title build Preprocess tab
#' @description Generate the data preprocessing tab of the \code{\link{RclusToolGUI}}, in which the user can select, transform, filter or create variables.
#' @param RclusTool.env environment in which data and intermediate results are stored.
#' @param reset boolean to reset the whole interface
#' @param readConfig boolean to reset the whole interface according the config parameters of the data.sample
#' @return None
#' @keywords internal
#' 
initPreprocessTab <- function(RclusTool.env, reset=FALSE, readConfig=FALSE)
{
    if (is.null(RclusTool.env$gui$tabs.env$prepro) || !length(RclusTool.env$gui$tabs.env$prepro))
    {
        RclusTool.env$gui$tabs.env$prepro <- new.env()
        buildPreprocessTab(RclusTool.env)
        reset <- TRUE
    }

    prepro.env <- RclusTool.env$gui$tabs.env$prepro

    signal.names <- colnames(RclusTool.env$data.sample$profiles[[1]])

    # initialization of features names lists
    initFeaturesList <- function(reset=FALSE)
    {
        select <- c()
        unselect <- colnames(RclusTool.env$data.sample$features$preprocessed$x)
        if (!reset && !is.null(RclusTool.env$data.sample$config$selectFeat))
        {
            select <- RclusTool.env$data.sample$config$selectFeat
            unselect <- setdiff(unselect, select)
        }
        prepro.env$featName <- list(unselect=unselect, select=select)
    } 

    # reset signal colors
    resetSignalColors <- function() {
        prepro.env$signalColor <- RclusTool.env$param$visu$palette.colors[1:length(signal.names)]
        names(prepro.env$signalColor) <- signal.names
    }

    if (is.null(RclusTool.env$gui$tabs.env$prepro) || !length(RclusTool.env$gui$tabs.env$prepro))
    {
        RclusTool.env$gui$tabs.env$prepro <- new.env()
        buildPreprocessTab(RclusTool.env)
        reset <- TRUE
    }

    prepro.env <- RclusTool.env$gui$tabs.env$prepro

    if (reset || readConfig)
    {
        initFeaturesList(reset)
        prepro.env$featureOperations <- matrix(ncol=4,nrow=0)
        prepro.env$defaultFeaturesHV <- NULL
        tclvalue(prepro.env$tcl.pca.check) <- "1" #mode standard non expert
        tclvalue(prepro.env$tcl.spectral.check) <- "0"
        tclvalue(prepro.env$tcl.pca.nb.dims) <- "0"
        tclvalue(prepro.env$tcl.scaling.check) <- "1" #mode standard non expert
        tclvalue(prepro.env$tcl.sampling.check) <- "0"
        tclvalue(prepro.env$tcl.sampling.size) <- min(RclusTool.env$param$preprocess$sampling.size.max, length(RclusTool.env$data.sample$id.clean))
        prepro.env$available.spaces <- NULL
        prepro.env$featSpace <- "scaled.pca"
        prepro.env$ProcessFile <- ""
        resetSignalColors()

        if (RclusTool.env$gui$user.type=="expert")
        {
            prepro.env$featSpace <- "initial"
            tclvalue(prepro.env$tcl.pca.check) <- "0" #mode  expert
        }

        if (readConfig) {
            operations <- RclusTool.env$data.sample$config$operations
            if (!is.null(operations))
                prepro.env$featureOperations <- operations

            defaultFeat <- RclusTool.env$data.sample$config$defaultFeat
            if (!is.null(defaultFeat))
                prepro.env$defaultFeaturesHV <- defaultFeat

            if (any(grepl("pca", names(RclusTool.env$data.sample$features), fixed=TRUE)))
                tclvalue(prepro.env$tcl.pca.check) <- "1"

            tclvalue(prepro.env$tcl.pca.nb.dims) <- as.character(RclusTool.env$data.sample$config$pca.nb.dims)

            if (any(grepl("spectral", names(RclusTool.env$data.sample$features), fixed=TRUE)))
                tclvalue(prepro.env$tcl.spectral.check) <- "1"

            if (any(grepl("scaled", names(RclusTool.env$data.sample$features), fixed=TRUE)))
                tclvalue(prepro.env$tcl.scaling.check) <- "1"

            tclvalue(prepro.env$tcl.sampling.size) <- as.character(min(RclusTool.env$param$preprocess$sampling.size.max, length(RclusTool.env$data.sample$id.clean)))
            if (!is.null(RclusTool.env$data.sample$sampling))
            {
                tclvalue(prepro.env$tcl.sampling.check) <- "1"
                tclvalue(prepro.env$tcl.sampling.size) <- as.character(min(RclusTool.env$param$preprocess$sampling.size.max, length(RclusTool.env$data.sample$id.clean)))
            }

            if (!is.null(RclusTool.env$data.sample$config$default.classif.feature.space))
                prepro.env$featSpace <- RclusTool.env$data.sample$config$default.classif.feature.space

            if (grepl("pca", prepro.env$featSpace, fixed=TRUE))
                tclvalue(prepro.env$tcl.pca.check) <- "1"
            if (grepl("spectral", prepro.env$featSpace, fixed=TRUE))
                tclvalue(prepro.env$tcl.spectral.check) <- "1"

            selectFeat <- RclusTool.env$data.sample$config$selectFeat 
            if (!is.null(selectFeat))
            {
                prepro.env$featName$select <- selectFeat
                prepro.env$featName$unselect <- setdiff(colnames(RclusTool.env$data.sample$features$preprocessed$x), selectFeat)
            }

            signalColor <- RclusTool.env$data.sample$config$signalColor
            if (!is.null(signalColor))
                prepro.env$signalColor[names(signalColor)] <- signalColor
        }
    }

    prepro.env$buildFeaturesList()

    if (RclusTool.env$gui$user.type=="expert")
    {
        prepro.env$onSpaceSelection(reset=reset)
        prepro.env$refreshPreprocessingName()
    }
}


