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

#' function to create the 'importTab' for data importation (features, metadata, signals and images)
#' @title Build Import tab
#' @description Generate the files importation tab of the \code{\link{RclusToolGUI}}, in which the user can select original files (features, metadata, signals or images).
#' @param RclusTool.env environment in which data and intermediate results are stored.
#' @return None
#' @importFrom graphics plot
#' @importFrom tools file_path_sans_ext
#' @import tcltk tcltk2
#' @keywords internal

buildImportTab <- function(RclusTool.env) {
    win1.nb <- RclusTool.env$gui$win1$env$nb
    win2.nb <- RclusTool.env$gui$win2$env$nb
 
	import.env <- RclusTool.env$gui$tabs.env$import

    fontFrame <- tkfont.create(family = RclusTool.env$param$visu$font, weight = "bold", size = RclusTool.env$param$visu$size)
    fontTitleFrame <- tkfont.create(family = RclusTool.env$param$visu$titlefont, weight = "bold", size = RclusTool.env$param$visu$titlesize)
    padx = "6m"
    padx.m = "3m"
    pady = "2m"
	
    ## Build the 'Required files' frame
    RequiredFrametext <- makeTitle("REQUIRED INPUT DATA FILES")
    RequiredFrame <- tkwidget(win1.nb$env$import, "labelframe", text = RequiredFrametext , font = fontTitleFrame, padx = padx.m, pady=pady, relief = "flat")

    tkEmptyLine(win1.nb$env$import, row=1)
    tkgrid(RequiredFrame, columnspan = 3, row = 2, sticky = "we", pady = pady)

    # Select separator and decimal separator for csv (for features file)
    
    sepList <- c(",", ";", "\\s", "\\t")
    sepValues <- c(",", ";", "", "\t")
    names(sepValues) <- sepList
    decList <- c(".", ",")
    misList <- c("", "NA", "9999")
    
    # Import Features parameters 

    opt1 <- tklabel(RequiredFrame, text="sep")
    opt2 <- tklabel(RequiredFrame, text="dec")
    opt3 <- tklabel(RequiredFrame, text="missing")
    sepSelectFeat <- tclVar(",")
    decSelectFeat <- tclVar(".")
    misSelectFeat <- tclVar("")
    combo.Sep.Feat <- ttkcombobox(RequiredFrame, values=sepList, textvariable=sepSelectFeat, state="readonly", width = 2) 
    combo.Dec.Feat <- ttkcombobox(RequiredFrame, values=decList, textvariable=decSelectFeat, state="readonly", width = 2) 
    combo.Mis.Feat <- ttkcombobox(RequiredFrame, values=misList, textvariable=misSelectFeat, state="readonly", width = 2) 
    
    # Select the features file (.csv)
    featuresName <- tktext(RequiredFrame, bg="white", font="courier", width=7*RclusTool.env$param$visu$size, height=2, font = fontFrame, state="disabled")

    import.env$refreshFeaturesName <- function()
    {
 		tkconfigure(featuresName, state="normal") 
        tkdelete(featuresName, "1.0", "end")
        tkinsert(featuresName,"end", import.env$featuresFile)
        tkconfigure(featuresName, state="disabled") 
    }

    featuresButton <- tk2button(RequiredFrame, text = "FEATURES / RDS FILE", image = "csvRDSFile", compound = "left", width = 17, 
                                command = function() {
                                    import.env$featuresFile <- tclvalue(tkgetOpenFile(filetypes = "{ {csv Files} {.csv} } { {RClusTool RDS Files} {.RDS} }"))
                                    import.env$refreshFeaturesName()
                                    if (!nchar(import.env$featuresFile)){
                                        tkmessageBox(message = "No file selected!")
                                    } else {
                                    	import.env$Dir <- substr(import.env$featuresFile, 1, (nchar(import.env$featuresFile)-nchar(basename(import.env$featuresFile)))-1)
                                    	import.env$DirDefault <- import.env$Dir 
                                    	import.env$refreshDirectoryName()
                                        ## From there, we define the local working directory
                                        ##setwd(dirname(import.env$featuresFile))
                                        if (grepl('.RDS',import.env$featuresFile)){
                                            import.env$rdsFile <- import.env$featuresFile
                                            ## Sauvegarde le repertoire du fichier par defaut
                                            import.env$featuresFile <- ""
                                        }
                                    }
                                })

 
    previewCSVfileFeat <- function() {
       previewCSVfile (file=import.env$featuresFile, sep = sepValues[tclvalue(sepSelectFeat)], dec = tclvalue(decSelectFeat), 
                       na.strings = tclvalue(misSelectFeat), RclusTool.env = RclusTool.env)
    }

    previewCSVfileSig <- function() {
       previewCSVfile (file=import.env$signalFile, sep = sepValues[tclvalue(sepSelectSig)], dec = tclvalue(decSelectSig), 
                       na.strings = tclvalue(misSelectSig), RclusTool.env = RclusTool.env)
    }
                                                  
    featuresPreviewButton <- tk2button(RequiredFrame, text="Preview", compound = "left", width="7", command = previewCSVfileFeat) 

    tkconfigure(opt1,font = fontFrame)
    tkconfigure(opt2,font = fontFrame)
    tkconfigure(opt3,font = fontFrame)
    tkgrid(featuresButton, row = 1, column = 1, padx = padx.m, sticky = "w")
    tkgrid(featuresName, row = 1, column= 2)
    tkgrid(opt1, row = 1, column = 3, sticky = "e")
    tkgrid(combo.Sep.Feat, row = 1, column = 4, sticky = "w")
    tkgrid(opt2, row = 1, column = 5, sticky = "e")
    tkgrid(combo.Dec.Feat, row = 1, column = 6, sticky = "w")
    tkgrid(opt3, row = 1, column = 7, sticky = "e")
    tkgrid(combo.Mis.Feat, row = 1, column = 8, sticky = "w")
    tkgrid(featuresPreviewButton, row = 1, column = 9, padx = padx.m, sticky = "w")

    ## Build the 'Optional files' frame
    OptionalFrametext <- makeTitle("OPTIONAL INPUT DATA FILES")
    OptionalFrame <- tkwidget(win1.nb$env$import, "labelframe", text = OptionalFrametext, font = fontTitleFrame, padx = padx.m, pady = pady, relief = "flat")
    tkEmptyLine(win1.nb$env$import, row=3)
    tkgrid(OptionalFrame, columnspan = 3, row = 6, sticky = "we", pady=pady)
    
    # Import signals parameters 
        
    opt1 <- tklabel(OptionalFrame, text="sep")
    opt2 <- tklabel(OptionalFrame, text="dec")
    opt3 <- tklabel(OptionalFrame, text="missing")
    sepSelectSig <- tclVar(",")
    decSelectSig <- tclVar(".")
    misSelectSig <- tclVar("")
    combo.Sep.Sig <- ttkcombobox(OptionalFrame, values=sepList, textvariable=sepSelectSig, state="readonly", width = 2) 
    combo.Dec.Sig <- ttkcombobox(OptionalFrame, values=decList, textvariable=decSelectSig, state="readonly", width = 2) 
    combo.Mis.Sig <- ttkcombobox(OptionalFrame, values=misList, textvariable=misSelectSig, state="readonly", width = 2) 
    
    # Select the signal file (.csv)
        
    signalsName <- tktext(OptionalFrame, bg="white", font="courier", width=7*RclusTool.env$param$visu$size, height=2, font = fontFrame, state="disabled")

    import.env$refreshSignalsName <- function()
    {
 		tkconfigure(signalsName, state="normal") 
        tkdelete(signalsName, "1.0", "end")
        tkinsert(signalsName,"end", import.env$signalFile)
        tkconfigure(signalsName, state="disabled") 
    }

    signalsButton <- tk2button(OptionalFrame, text = "SIGNALS", image = "csvFile", compound = "left", width = 20, 
                              command = function() {
                                  import.env$signalFile <- tclvalue(tkgetOpenFile(filetypes = "{{csv Files} {.csv}}"))
                                  import.env$refreshSignalsName()
                                  if (!nchar(import.env$signalFile)) {
                                      tkmessageBox(message = "No file selected!")
                              	  }
                               })
    signalsPreviewButton <- tk2button(OptionalFrame, text="Preview", compound = "left", width="7", command = previewCSVfileSig)

    
    tkconfigure(opt1,font = fontFrame)
    tkconfigure(opt2,font = fontFrame)
    tkconfigure(opt3,font = fontFrame)
    tkgrid(signalsButton, row = 1, column = 1, padx = padx.m, sticky = "w")
    tkgrid(signalsName, row = 1, column = 2)
    tkgrid(opt1, row = 1, column = 3, sticky = "e")
    tkgrid(combo.Sep.Sig, row = 1, column = 4, sticky = "w")
    tkgrid(opt2, row = 1, column = 5, sticky = "e")
    tkgrid(combo.Dec.Sig, row = 1, column = 6, sticky = "w")
    tkgrid(opt3, row = 1, column = 7, sticky = "e")
    tkgrid(combo.Mis.Sig, row = 1, column = 8, sticky = "w")
    tkgrid(signalsPreviewButton, row = 1, column = 9, padx = padx.m, sticky = "w")
  
    # Select the images directory
    imagesName <- tktext(OptionalFrame, bg="white", font="courier", width=7*RclusTool.env$param$visu$size, height=2, font = fontFrame, state="disabled")

    import.env$refreshImagesName <- function()
    {
 		tkconfigure(imagesName, state="normal") 
        tkdelete(imagesName, "1.0", "end")
        if (dir.exists(import.env$imageDir)) {
            tkinsert(imagesName,"end", import.env$imageDir)
        }
        tkconfigure(imagesName, state="disabled") 
    }
    
    imageButton <- tk2button(OptionalFrame, text = "IMAGES", image = "folder", compound = "left", width = 20, command = function() {
                                 imageDir <- tk_choose.dir(default = import.env$imageDirDefault, caption = "Select folder for images.")
                                 if (is.na(imageDir)) {
                                     tkmessageBox(message = "No folder selected!")
                                 } else {
                                     import.env$imageDir <- imageDir
                                     import.env$refreshImagesName()
                                     import.env$imageDirDefault <- import.env$imageDir
                                 }
                            })
    tkgrid(imageButton, row = 2, column = 1, padx = padx.m, sticky = "w")
    tkgrid(imagesName, row = 2, column = 2)

    # Select the metadata file (.txt)
    metadataFileName <- tktext(OptionalFrame, bg="white", font="courier", width=7*RclusTool.env$param$visu$size, height=2, font = fontFrame, state="disabled")
    
        import.env$refreshMetadataFileName <- function()
    {
 		tkconfigure(metadataFileName, state="normal") 
        tkdelete(metadataFileName, "1.0", "end")
        tkinsert(metadataFileName,"end", import.env$metadataFile)
        tkconfigure(metadataFileName, state="disabled") 
    }

    metadataButton <- tk2button(OptionalFrame, text = "METADATA", image = "txtFile", compound = "left", width = 20,
                                command = function() {
                                    import.env$metadataFile <- tclvalue(tkgetOpenFile(filetypes = "{{txt Files} {.txt}}"))
                                    import.env$refreshMetadataFileName()
                                    if (!nchar(import.env$metadataFile)) {
                                        tkmessageBox(message = "No file selected!")
                                    } 
                                })
    tkgrid(metadataButton, row = 3, column = 1, padx = padx.m, sticky = "w")
	tkgrid(metadataFileName, row = 3, column = 2)
	
    ## Build the dataset with features, metadata, signals and images
    import.env$OnCompute <- function() {
        if (!nchar(import.env$featuresFile) && !nchar(import.env$rdsFile)) {
            tkmessageBox(message = "No data to import.\nPlease select a features file!", 
                         type = "ok", icon = "info", title = "Error")
        } else {
            tkconfigure(win1.nb$env$import, cursor = "watch")
            # Building step
            system.time(importSample(file.features = import.env$featuresFile, file.meta = import.env$metadataFile,
                                     file.profiles = import.env$signalFile, dir.images = import.env$imageDir, dir.save = import.env$Dir, 
                                     file.RDS = import.env$rdsFile, sepFeat = sepValues[tclvalue(sepSelectFeat)], decFeat = tclvalue(decSelectFeat), 
                                     naFeat=tclvalue(misSelectFeat), sepSig = sepValues[tclvalue(sepSelectSig)], decSig= tclvalue(decSelectSig),
                                     naSig=tclvalue(misSelectSig), RclusTool.env=RclusTool.env) 
            -> RclusTool.env$data.sample)
            
            # Display informations in console
            if (import.env$featuresFile==""){
            	filename=import.env$rdsFile
            	import.env$signalFile <-  RclusTool.env$data.sample$files$profiles
                import.env$refreshSignalsName()
                import.env$metadataFile <-  RclusTool.env$data.sample$files$meta
                import.env$refreshMetadataFileName()
                import.env$imageDir <-  RclusTool.env$data.sample$files$images
    			import.env$refreshImagesName()
            } else {
            	filename=RclusTool.env$data.sample$files$features
            }
            if (!is.null(RclusTool.env$data.sample)) {
            	messageConsole(paste("----- Features importation -----\n",
                                           	   "Filename:  ", basename(filename), "\n",
                                           	   "Number of observations:  ", RclusTool.env$data.sample$size, "\n",
                                           	   "Number of features:  ", length(RclusTool.env$data.sample$features$initial$x), "\n\n", sep = ""),
                               RclusTool.env=RclusTool.env)
            	if (nchar(import.env$metadataFile)) {
            		messageConsole(paste(paste(RclusTool.env$data.sample$metadata$x, collapse = "\n", sep = ""), "\n\n", sep = ""), 
                                   RclusTool.env=RclusTool.env)
                	messageConsole(paste("----- MetaData importation -----\n",
                                              	 "Filename:  ", basename(RclusTool.env$data.sample$files$meta), "\n", sep = ""), 
                                   RclusTool.env=RclusTool.env)
            	}
            	if (nchar(import.env$signalFile))
               	    messageConsole(paste("----- Signals importation -----\n",
                        	                       "Filename:  ", basename(RclusTool.env$data.sample$files$profiles), "\n",
                                   	               "Number of observations:  ", length(RclusTool.env$data.sample$profiles), "\n\n", sep = ""), 
                                      RclusTool.env=RclusTool.env)
            	if (nchar(import.env$imageDir))
                	messageConsole(paste("----- Images importation -----\n",
                                               	   "Folder:  ", basename(RclusTool.env$data.sample$files$images), "\n",
                                                   "Number of observations:  ", length(!is.na(RclusTool.env$data.sample$images)), "\n\n", sep = ""), 
                                   RclusTool.env=RclusTool.env)
                                                   
                if (nchar(import.env$Dir))
                	messageConsole(paste("----- Working directory -----\n",
                                               	   "Folder:  ", basename(RclusTool.env$data.sample$files$dir), "\n\n"), RclusTool.env=RclusTool.env)
                                                   
            	if (!nchar(import.env$rdsFile))
                	messageConsole(paste("----- RDS file -----\n",
                                               		"Creation of a RDS object\n\n", sep = ""), RclusTool.env=RclusTool.env)
                                               		

            # Active the 'Preprocessing' tab
            initPreprocessTab(RclusTool.env = RclusTool.env, reset=TRUE)
                          
  			tk2delete.notetab(win2.nb)

            abdPlotTabsGUI(RclusTool.env)

            RclusTool.env$gui$win1$env$authorization$prepro <- TRUE
            RclusTool.env$gui$win1$env$authorization$classif <- FALSE
            } else {
                message("Error: Missing file, only one column or encoding problem")
            }
            tkconfigure(win1.nb$env$import, cursor = "left_ptr")
        }
    }
  
 	## Build the 'Working Directory' frame
 	WdFrametext <- makeTitle("WORKING DIRECTORY")
    WdFrame <- tkwidget(win1.nb$env$import, "labelframe", text = WdFrametext, font = fontTitleFrame, padx = padx.m, pady = pady, relief = "flat")
    tkEmptyLine(win1.nb$env$import, row=7)
    tkgrid(WdFrame, columnspan = 3, row = 8, sticky = "we", pady=pady)
     
    # Select the directory
    Dir <- tktext(WdFrame, bg="white", font="courier", width=7*RclusTool.env$param$visu$size, height=2, font = fontFrame, state="disabled")

    import.env$refreshDirectoryName <- function()
    {
 		tkconfigure(Dir, state="normal") 
        tkdelete(Dir, "1.0", "end")
        if (dir.exists(import.env$Dir)) {
            tkinsert(Dir,"end", import.env$Dir)
        }
        tkconfigure(Dir, state="disabled") 
    }

    DirButton <- tk2button(WdFrame, text = "DIRECTORY", image = "folder", compound = "left", width = 20, command = function() {
                                 Dir <- tk_choose.dir(default = import.env$DirDefault, caption = "Select folder.")
                                 if (is.na(Dir)) {
                                     tkmessageBox(message = "No folder selected!")
                                 } else {
                                     import.env$Dir <- Dir
                                     import.env$refreshDirectoryName()
                                     import.env$DirDefault <- import.env$Dir
                                 }
                            })

    tkgrid(DirButton, row = 2, column = 1, padx = padx.m, sticky = "w")
    tkgrid(Dir, row = 2, column = 2)
 
     
 ## Build the 'Advices Frame' frame
    AdviceFrametext <- makeTitle("ADVICES")
    AdviceFrame <- tkwidget(win1.nb$env$import, "labelframe", text = AdviceFrametext, font = fontTitleFrame, padx = padx.m, pady = pady, relief = "flat")
    tkEmptyLine(win1.nb$env$import, row=9)
    tkgrid(AdviceFrame, columnspan = 3, row = 10, sticky = "we", pady = pady)
    
 # What kind of CSV file for features
    FeaturesAdvice <- tkwidget(AdviceFrame, "labelframe", 
                            text = "How to format features data",
                            padx = padx, pady = pady, relief = "groove")
    FeaturesAdviceText <- tk2label(FeaturesAdvice, text = "Data must be in a .csv file\nObservations in rows\nFeatures in columns\nMissing value must be 'empty'", width = 30)                   
    tkconfigure(FeaturesAdvice,font = fontFrame)
    tkconfigure(FeaturesAdviceText,font = fontFrame)
    tkgrid(FeaturesAdvice, columnspan = 1, column = 1, row = 1)
    tkgrid(FeaturesAdviceText)
    
  # What kind of CSV file for signal
    SignalsAdvice <- tkwidget(AdviceFrame, "labelframe", 
                            text = "How to format signal data", 
                            padx = padx, pady = pady, relief = "groove")
    SignalsAdviceText <- tk2label(SignalsAdvice, text = "Data must be in a .csv file\nSignals in columns\nA same ID for all signal values\nMissing value must be 'empty'", width = 30)
    tkconfigure(SignalsAdvice,font = fontFrame)
    tkconfigure(SignalsAdviceText,font = fontFrame)    
    tkgrid(SignalsAdvice, columnspan = 1, column = 2, row = 1)
    tkgrid(SignalsAdviceText)

    # What is an RDS
    RDSAdvice <- tkwidget(AdviceFrame, "labelframe", 
                            text = "What is a RDS file ?",
                            padx = padx, pady = pady, relief = "groove")
    RDSAdviceText <- tk2label(RDSAdvice, text = "After a first use of yours files\n an Rclustool RDS file is saved.\nThis file contains all the data\n you used and it's faster to load", width = 30)
    tkconfigure(RDSAdvice,font = fontFrame)
    tkconfigure(RDSAdviceText,font = fontFrame)     
    tkgrid(RDSAdvice, columnspan = 1, column = 3, row = 1)
    tkgrid(RDSAdviceText)
    
    # What kind of file for images
    ImagesAdvice <- tkwidget(AdviceFrame, "labelframe", 
                            text = "Important : how to format images data", 
                            padx = padx, pady = pady, relief = "groove")
    ImagesAdviceText <- tk2label(ImagesAdvice, text = "\nJPEG or PNG images\nObservation's ID for filename\n", width = 30)
    tkconfigure(ImagesAdvice,font = fontFrame)
    tkconfigure(ImagesAdviceText,font = fontFrame)
    tkgrid(ImagesAdvice, columnspan = 1, column = 1, row = 2)
    tkgrid(ImagesAdviceText)

    # What kind of TXT file for metadata
    MetaAdvice <- tkwidget(AdviceFrame, "labelframe", 
                            text = "How to format metadata", 
                            padx = padx, pady = pady, relief = "groove")
    MetaAdviceText <- tk2label(MetaAdvice, text = "\nData must be in a .txt file\n'Metadata name: value'\n", width = 30)
    tkconfigure(MetaAdvice,font = fontFrame)
    tkconfigure(MetaAdviceText,font = fontFrame)    
    tkgrid(MetaAdvice, columnspan = 1, column = 2, row = 2)
    tkgrid(MetaAdviceText)
    
    tk.compute.but <- tk2button(win1.nb$env$import, text="IMPORT", image = "data", compound = "left", width = 15, command=import.env$OnCompute)

    # Reset import Tab
    onReset <- function() {
        initImportTab(RclusTool.env = RclusTool.env, reset=TRUE)
        RclusTool.env$gui$win1$env$authorization$prepro <- FALSE
        RclusTool.env$gui$win1$env$authorization$classif <- FALSE
        # Active the 'Preprocessing' tab
        initPreprocessTab(RclusTool.env = RclusTool.env, reset=TRUE)
    }
    butReset <- tk2button(win1.nb$env$import, text = "RESET", image = "reset", compound = "left", width = 15, command = onReset)

    tkEmptyLine(win1.nb$env$import, row=11)
    tkgrid(tk.compute.but, row = 12, column = 0, pady = pady)
    tkgrid(butReset, row = 12, column = 2)

}

#' function to preview csv file
#' @title Preview CSV file
#' @description This function generates a window printing first rows of the csv file and the first rows of the data.frame obtained.
#' @param file : csv filename.
#' @param nrows : number of rows to be read.
#' @param RclusTool.env environment in which data and intermediate results are stored.
#' @param ... : other parameters of read.csv function.
#' @return None
#' @import tcltk tcltk2 
#' @importFrom utils read.csv
#' @importFrom knitr kable
#' @keywords internal
#' 
previewCSVfile <- function(file, sep, dec, na.strings, RclusTool.env, nrows=3, ...)
{
    if (!file.exists(file)) 
        return()

    df.csv <- tryCatch(
        {utils::read.csv(file=file, sep=sep, dec=dec, na.strings=na.strings, nrows=nrows, ...)},
        error=function(cond) {return(cond)},
        warning=function(cond) {return(cond)})

    raw <- readLines(con=file, n=nrows+1)
    raw <- paste0(raw, collapse="\n")

    tkmb <- tktoplevel()
    tktitle(tkmb) <- paste("First", nrows, "rows of file", basename(file))
    scrx <- tk2scrollbar(tkmb, orientation = "horizontal", command = function(...) tkxview(console, ...))
    scry <- tk2scrollbar(tkmb, orientation = "vertical", command = function(...) tkyview(console, ...))
    console <- tk2text(tkmb, width = RclusTool.env$param$visu$console , height = 12, font = "courier", wrap = "none", 
                       xscrollcommand = function(...) tkset(scrx, ...), 
                       yscrollcommand = function(...) tkset(scry, ...))
    tkgrid(console, scry, sticky = "nsew", pady = c(0,0))
    tkgrid.rowconfigure(tkmb, console, weight = 1)
    tkgrid.columnconfigure(tkmb, console, weight = 1)
    tkgrid(scrx, sticky = "ew")

    if (!is.data.frame(df.csv)) {
        import = paste("Extraction failed:", df.csv)
    } else if (ncol(df.csv)==1) {
        import = "Data extraction failed: only one feature was found (please rectify column separator)."
    } else {
        tc <- textConnection("str","w")
        sink(tc)
        #print(df.csv)
        import = knitr::kable(df.csv)
        sink()
        close(tc)
        #import <- substr(str, 3, nchar(str[1]))
        #import <- paste0("| ", import, "|", collapse="\n")
        import <- paste0(import, collapse="\n")
    }
    tkinsert(console, "0.0", import)
    tkinsert(console, "0.0", "\n")
    #tkinsert(console, "0.0", " ------------------------------------------------------------------------\n")
    tkinsert(console, "0.0", " ------ WARNING : values considered as numerical are RIGHT-ALIGNED ------\n")
    tkinsert(console, "0.0", paste(" ----------------------- EXTRACTED",nrows, "FIRST LINES ------------------------\n"))
    #tkinsert(console, "0.0", " ------------------------------------------------------------------------\n")
    tkinsert(console, "0.0", "\n")
    tkinsert(console, "0.0", "\n")
    tkinsert(console, "0.0", raw)
    tkinsert(console, "0.0", "\n")
    #tkinsert(console, "0.0", " ------------------------------------------------------------------------\n")
    tkinsert(console, "0.0", paste(" ----------------------- ORIGINAL",nrows, "FIRST LINES ------------------------\n"))
    #tkinsert(console, "0.0", " ------------------------------------------------------------------------\n")
}



#' function to initialize (and to create) the 'importTab'
#' @title import tab 
#' @description This function generates the import tab of the \code{\link{RclusToolGUI}}, in which the user can import files.
#' @param RclusTool.env : environment in which data and intermediate results are stored.
#' @param reset : if TRUE the whole tab is reset, with default options
#' @return None
#' @import tcltk tcltk2
#' @keywords internal
#' 
initImportTab <- function(RclusTool.env, reset=FALSE)
{
    if (is.null(RclusTool.env$gui$tabs.env$import) || !length(RclusTool.env$gui$tabs.env$import))
    {
        RclusTool.env$gui$tabs.env$import <- new.env()
        buildImportTab(RclusTool.env)
        reset <- TRUE
    }

    import.env <- RclusTool.env$gui$tabs.env$import

    if (reset)
    {
        import.env$featuresFile <- ""
        import.env$rdsFile <- ""
        import.env$metadataFile <- ""
        import.env$signalFile <- ""
        import.env$imageDir <- ""
        import.env$imageDirDefault <- getwd()
        import.env$DirDefault <- getwd()
        import.env$Dir <- ""
        if (!is.null(RclusTool.env$data.sample$files$images) && dir.exists(RclusTool.env$data.sample$files$images))
            import.env$imageDirDefault <- RclusTool.env$data.sample$files$images
    }

    ########## test only ##########
    if (RclusTool.env$gui$debug.mode == TRUE) {
        rep <- system.file("extdata", package="RclusTool")
        import.env$featuresFile <- file.path(rep, "sample_example_features.csv")
        import.env$metadataFile <- file.path(rep, "sample_example_info.txt")
        import.env$signalFile <- file.path(rep, "sample_example_pulses.csv")
        import.env$imageDir <- file.path(rep, "img_example")
        import.env$OnCompute()
    }
    ######## end test only ########

    # refresh file names
    import.env$refreshFeaturesName()
    import.env$refreshSignalsName()
    import.env$refreshImagesName()
    import.env$refreshMetadataFileName()
    import.env$refreshDirectoryName()
}


