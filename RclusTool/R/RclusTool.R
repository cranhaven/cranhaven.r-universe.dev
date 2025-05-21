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

#' function to display the first window of the RclusTool interface (username and user type selection)
#' @title Username and user type selection
#' @description Generate a first window to enter the username and to select the user type ('standard' or 'expert').
#' @param RclusTool.env environment in which data and results will be stored. If NULL, a local environment will be created.
#' @param debug boolean: if TRUE, the debug mode is activated.
#' @importFrom grDevices graphics.off
#' @import tcltk tcltk2
#' @return Nothing, just open the graphical user interface.
#' @export 
#' @examples 
#' RclusToolGUI()
#'
RclusToolGUI <- function(RclusTool.env = new.env(), debug=FALSE) {

    if (!length(RclusTool.env))
        initParameters(RclusTool.env)
    
    #assign("RclusTool.env", RclusTool.env.init, globalenv())
    
    RclusTool.env$gui$debug.mode <- debug

    grDevices::graphics.off()

    if (!is.null(RclusTool.env$gui$operating.system)&&(RclusTool.env$gui$operating.system=="windows")) {
        RclusTool.env$param$visu$scale.graphics <- 1.4
    } else if (!is.null(RclusTool.env$gui$operating.system)&&(RclusTool.env$gui$operating.system=="apple")) {
        RclusTool.env$param$visu$scale.graphics <- 0.8
    }

    # User selection window
    Sys.sleep(0.1)
    fontTitle <- tkfont.create(family = "Arial", size = 12, weight = "bold")
    Sys.sleep(0.1)
    tk2theme("alt")
    Sys.sleep(0.1)
    userWindow <- tktoplevel()
    Sys.sleep(0.1)
    tktitle(userWindow) <- "User selection"
    Sys.sleep(0.1)
    userBlock <- tkwidget(userWindow, "labelframe", borderwidth = 0)
    Sys.sleep(0.1)
    tkgrid(userBlock)
    Sys.sleep(0.1)
    tkgrid(tklabel(userBlock, text = "RclusTool-GUI", font = fontTitle), columnspan = 2, pady = c(5,5))
    Sys.sleep(0.1)
    name <- tclVar("username")
    Sys.sleep(0.1)
    entName <- tk2entry(userBlock, width = "25", textvariable = name)
    Sys.sleep(0.1)
    tkgrid(entName, pady = c(10,10))
    Sys.sleep(0.1)

	dimensions<-as.character(tkwm.maxsize(userWindow))
	RclusTool.env$param$visu$screenlength <- as.integer(dimensions[1])
	RclusTool.env$param$visu$screenheight <- as.integer(dimensions[2])
	RclusTool.env$param$visu$style <- tk2font.get("TkDefaultFont")
	
    setSizes <- function(screenlength=NULL, screenheight=NULL)
    {
        if (is.null(screenlength))
            screenlength <- RclusTool.env$param$visu$screenlength
        if (is.null(screenheight))
            screenheight <- RclusTool.env$param$visu$screenheight

        f <- min(screenlength/1920.0, screenheight/1080.0)
   		RclusTool.env$param$visu$size <- floor(RclusTool.env$param$visu$size*f)
        RclusTool.env$param$visu$titlesize <- floor(RclusTool.env$param$visu$titlesize*f)
		RclusTool.env$param$visu$style$size <- floor(RclusTool.env$param$visu$style$size*f)
		RclusTool.env$param$visu$hscale <- RclusTool.env$param$visu$hscale*f
		RclusTool.env$param$visu$console <- floor(RclusTool.env$param$visu$console*f)
    }
    setSizes()

    standardGUIgo <- function()
    {
        RclusTool.env$gui$user.name <- tclvalue(name)
        RclusTool.env$gui$user.type <- "standard"
        tkdestroy(userWindow)  
        MainWindow(RclusTool.env = RclusTool.env)
    }

    expertGUIgo <- function()
    {
        RclusTool.env$gui$user.name <- tclvalue(name)
        RclusTool.env$gui$user.type <- "expert"
        tkdestroy(userWindow) 
        MainWindow(RclusTool.env = RclusTool.env)
    }

    # Button for 'standard' use
    standardButton <- tkbutton(userBlock, text = "Standard user\n(recommended)", width = 20, command = function() {
                                   RclusTool.env$gui$user.name <- tclvalue(name)
                                   RclusTool.env$gui$user.type <- "standard"
                                   tkdestroy(userWindow)  
                                   MainWindow(RclusTool.env = RclusTool.env)
})
    tkgrid(standardButton, padx = c(100,100), pady = c(5,5))

    # Button for 'expert' use
    expertButton <- tkbutton(userBlock, text = "Expert user", width = 20, command = function() {
                                 RclusTool.env$gui$user.name <- tclvalue(name)
                                 RclusTool.env$gui$user.type <- "expert"
                                 tkdestroy(userWindow) 
                                 MainWindow(RclusTool.env = RclusTool.env)
})
    tkgrid(expertButton, pady = c(0,10))

	



	ScreenFrame <- tkwidget(userBlock,"labelframe", text = "Screen Resolution")
	tkgrid(ScreenFrame)

    # Button for default size screen
    DSButton <- tkbutton(ScreenFrame, text = "Auto", width = 2, command = function() {
                                 setSizes()
                                 tkconfigure(DSButton, borderwidth= 2, state="disabled")
                                 tkconfigure(SSButton, borderwidth= 2, state="normal")
                                 tkconfigure(MSButton, borderwidth= 2, state="normal")
                                 tkconfigure(LSButton, borderwidth= 2, state="normal")
})

	tkgrid(DSButton, row=5, column=0)

    # Button for Small screen
    SSButton <- tkbutton(ScreenFrame, text = "S", width = 2, command = function() {
                                 setSizes(1360)
                                 tkconfigure(DSButton, borderwidth= 2, state="normal")
                                 tkconfigure(SSButton, borderwidth= 2, state="disabled")
                                 tkconfigure(MSButton, borderwidth= 2, state="normal")
                                 tkconfigure(LSButton, borderwidth= 2, state="normal")
})

	tkgrid(SSButton, row=5, column=1)

    # Button for Medium screen
    MSButton <- tkbutton(ScreenFrame, text = "M", width = 2, command = function() {
                                 setSizes(1600)
                                 tkconfigure(DSButton, borderwidth= 2, state="normal")
                                 tkconfigure(SSButton, borderwidth= 2, state="normal")
                                 tkconfigure(MSButton, borderwidth= 2, state="disabled")
                                 tkconfigure(LSButton, borderwidth= 2, state="normal")
})

	tkgrid(MSButton, row=5, column=2)
	
    # Button for Large screen
    LSButton <- tkbutton(ScreenFrame, text = "L", width = 2, command = function() {
                                 setSizes(1920)
                                 tkconfigure(DSButton, borderwidth= 2, state="normal")
                                 tkconfigure(SSButton, borderwidth= 2, state="normal")
                                 tkconfigure(MSButton, borderwidth= 2, state="normal")
                                 tkconfigure(LSButton, borderwidth= 2, state="disabled")
})

	tkgrid(LSButton, row=5, column=3)
	
	
    tkconfigure(DSButton, borderwidth= 2, state = "disabled")
	
	
    ############### test only #############
    if (RclusTool.env$gui$debug.mode == TRUE)
        expertGUIgo()
    ############### end test only #############
}

#' function to display the graphical user interface to classify dataset.
#' @title Main window
#' @description Generate an user-friendly interface to classify data in a unsupervised, semi-supervised or supervised way.
#' @param RclusTool.env environment in which data and results will be stored. If NULL, a local environment will be created.
#' @return None
#' @import tcltk tcltk2 
#' @importFrom tkrplot tkrplot
#' @importFrom graphics plot par
#' @keywords internal

MainWindow <- function(RclusTool.env) {

    fontImportIntro <- tkfont.create(family = "Arial", size = 16, slant = "italic")

    # Main window building
    mainWindow <- tktoplevel()
    tktitle(mainWindow) <- "R-GUI classification" 
    tk2font.set("TkDefaultFont",RclusTool.env$param$visu$style)	 
    # messageBox for confirmation of interface closing
    tkwm.protocol(mainWindow, "WM_DELETE_WINDOW", function() {
                      response <- tkmessageBox(title = "Confirm close", icon = "question", 
                                               message = "Are you sure you want to close the interface?",
                                               type = "yesno", parent = mainWindow)
                      if (as.character(response) == "yes") {
                          if (!is.null(RclusTool.env$data.sample$files$results$dir)) {
                              LogFile.txt <- paste("log ", RclusTool.env$gui$user.name, 
                                                             format(Sys.time(),'_%Y%m%d_%Hh%Mm%Ss'), ".txt", sep="")
                              saveLogFile(LogFile.txt, tclvalue(tkget(console,"0.0","end")), RclusTool.env$data.sample$files$results$dir)
                          }
                          tkdestroy(mainWindow)
                      }
})

    # Logos display
    .logoFrame(mainWindow)

    # Message Frame  
    msgFrame <- tkwidget(mainWindow, "labelframe", borderwidth = 0)
    titleMsgFrameFrame <- tkwidget(msgFrame, "labelframe", borderwidth = 0)
    tkgrid(msgFrame, column = 2, row = 1)
    tkgrid(titleMsgFrameFrame, sticky = "w")
    tkgrid(tklabel(titleMsgFrameFrame, text = "Messages :"))
	fontFrame <- tkfont.create(family = "Arial", weight = "bold", size = RclusTool.env$param$visu$size)
	
    scrx <- tk2scrollbar(msgFrame, orientation = "horizontal", command = function(...) tkxview(console, ...))
    scry <- tk2scrollbar(msgFrame, orientation = "vertical", command = function(...) tkyview(console, ...))
    console <- tk2text(msgFrame, width = RclusTool.env$param$visu$console , height = 12, font = fontFrame, wrap = "none", 
                       xscrollcommand = function(...) tkset(scrx, ...), 
                       yscrollcommand = function(...) tkset(scry, ...))
    tkgrid(console, scry, sticky = "nsew", pady = c(0,0))
    tkgrid.rowconfigure(msgFrame, console, weight = 1)
    tkgrid.columnconfigure(msgFrame, console, weight = 1)
    tkgrid(scrx, sticky = "ew")

    # Insert operator name and user type
    messageConsole(paste("Date: ", format(Sys.time(),'%Y-%m-%d %Hh%Mm'), "\n",
                                   "Operator:  ", RclusTool.env$gui$user.name, "\n",
                                   "User type:  ", RclusTool.env$gui$user.type, "\n\n", sep = ""), RclusTool.env=RclusTool.env)

    # Graphic Frame
    graphicFrame <- tkwidget(mainWindow, "labelframe", borderwidth = 0)
    tkgrid(graphicFrame, column = 2, row = 2, rowspan = 2, sticky = "w")
    plotIni <- tkrplot(graphicFrame, hscale = RclusTool.env$param$visu$hscale , function() {
    			           opar <- graphics::par(no.readonly=TRUE)
			   	   		   on.exit(graphics::par(opar))
                           graphics::par(bg = "#D9D9D9")
                           graphics::plot(1, col="#D9D9D9", axes=FALSE, xlab=NA, ylab=NA)
                       })
    tkgrid(plotIni, row = 0, column = 1, sticky = "w")

    # Tabs
    RclusTool.env$gui$win1 <- tkwidget(mainWindow, "labelframe", borderwidth = 0)
    tkgrid(RclusTool.env$gui$win1, column = 1, row = 1, rowspan = 2, sticky = "nw")

    win1.env <- RclusTool.env$gui$win1$env
    win1.env$nb <- tk2notebook(RclusTool.env$gui$win1, tabs = c("   Importation   ", "   Preprocessing   ", 
                                              "   Unsupervised   ", "   Semi-supervised   ", 
                                              "   Supervised   ", " Batch process "))
    tkpack(win1.env$nb, fill = "both", expand = TRUE)

    nb1 <- win1.env$nb

    tk2notetab.RclusTool(nb1, "   Importation   ", "import")
    tk2notetab.RclusTool(nb1, "   Preprocessing   ", "preprocess")
    tk2notetab.RclusTool(nb1, "   Unsupervised   ", "unsup")
    tk2notetab.RclusTool(nb1, "   Semi-supervised   ", "semisup")
    tk2notetab.RclusTool(nb1, "   Supervised   ", "sup")
    tk2notetab.RclusTool(nb1, " Batch process ", "batch")
    win1.env$authorization$prepro <- FALSE
    win1.env$authorization$classif <- FALSE
    
    RclusTool.env$gui$win2 <- tkwidget(graphicFrame, "labelframe", borderwidth = 0)
    tkgrid(RclusTool.env$gui$win2, column = 1, row = 0, rowspan = 2, sticky = "nw")

    win2.env <- RclusTool.env$gui$win2$env
    win2.env$nb <- tk2notebook(RclusTool.env$gui$win2, tabs = c())
	tkpack(win2.env$nb, fill = "both", expand = TRUE)     	         	     
	      	                			  		
    classif_authorization <- function () {
    	if (win1.env$authorization$prepro == FALSE){
    		return(tk2notetab.select(win1.env$nb, "   Importation   "))
    	}
    	if (win1.env$authorization$classif == FALSE){
    		return(tk2notetab.select(win1.env$nb, "   Preprocessing   "))
    	}
    }
    
    tkbind(nb1$env$preprocess, "<Enter>", classif_authorization)
    tkbind(nb1$env$unsup, "<Enter>", classif_authorization)
    tkbind(nb1$env$semisup, "<Enter>", classif_authorization)
    tkbind(nb1$env$sup, "<Enter>", classif_authorization)

    RclusTool.env$gui$mainWindow <- mainWindow
    RclusTool.env$gui$console <- console
    RclusTool.env$gui$graphicFrame <- graphicFrame

    initImportTab(RclusTool.env = RclusTool.env)
    initBatchTab(RclusTool.env = RclusTool.env, reset=TRUE)
    
}


#' function to create and position the logo frame in the main window
#' @title Logo frame in the graphical user interface
#' @description Create and position the logo frame and images in the graphical user interface.
#' @param window window in which the logo frame is positioned.
#' @return None
#' @keywords internal 
#' 
.logoFrame <- function (window) {
    # Images for folders, files, etc.
    tcl("image", "create", "photo", "txtFile", file = system.file("images", "txt.gif", package="RclusTool"))
    tcl("image", "create", "photo", "csvFile", file = system.file("images", "csv.gif", package="RclusTool"))
    tcl("image", "create", "photo", "csvRDSFile", file = system.file("images", "csv-rds.gif", package="RclusTool"))
    tcl("image", "create", "photo", "folder", file = system.file("images", "folder.gif", package="RclusTool"))
    tcl("image", "create", "photo", "data", file = system.file("images", "data.gif", package="RclusTool"))
    tcl("image", "create", "photo", "check", file = system.file("images", "check.png", package="RclusTool"))
    tcl("image", "create", "photo", "reset", file = system.file("images", "reset.png", package="RclusTool"))
    tcl("image", "create", "photo", "visualize", file = system.file("images", "visualize.png", package="RclusTool"))

    # Images for logo frame
    tcl("image", "create", "photo", "ulcoLogo", file = system.file("images", "ulcoLogo.gif", package="RclusTool"))
    tcl("image", "create", "photo", "lisicLogo", file = system.file("images", "lisicLogo.gif", package="RclusTool"))
    tcl("image", "create", "photo", "cnrsLogo", file = system.file("images", "cnrsLogo.gif", package="RclusTool"))
    tcl("image", "create", "photo", "logLogo", file = system.file("images", "logLogo.gif", package="RclusTool"))
    tcl("image", "create", "photo", "DymaphyLogo", file = system.file("images", "DymaphyLogo.gif", package="RclusTool"))
    tcl("image", "create", "photo", "UELogo", file = system.file("images", "UELogo.gif", package="RclusTool"))
    tcl("image", "create", "photo", "InterregLogo", file = system.file("images", "InterregLogo.gif", package="RclusTool"))
    tcl("image", "create", "photo", "MarcoLogo", file = system.file("images", "MarcoLogo.gif", package="RclusTool"))
    tcl("image", "create", "photo", "regionLogo", file = system.file("images", "regionLogo.gif", package="RclusTool"))
    tcl("image", "create", "photo", "JericonextLogo", file = system.file("images", "JericonextLogo.gif", package="RclusTool"))

    # University and lab frame
    logoUnivFrame <- tkwidget(window, "labelframe", borderwidth = 0)
    tkgrid(logoUnivFrame, column = 1, row = 3, sticky = "w")
    tkgrid(ttklabel(logoUnivFrame, image = "ulcoLogo", compound = "image"), row = 0, column = 0)
    tkgrid(ttklabel(logoUnivFrame, image = "lisicLogo", compound = "image"), row = 0, column = 1)
    tkgrid(ttklabel(logoUnivFrame, image = "cnrsLogo", compound = "image"), row = 0, column = 2)
    tkgrid(ttklabel(logoUnivFrame, image = "logLogo", compound = "image"), row = 0, column = 3)

    # Project frame
    logoProjectFrame <- tkwidget(window, "labelframe", borderwidth = 0)
    tkgrid(logoProjectFrame, column = 1, row = 3, sticky = "e")
    tkgrid(ttklabel(logoProjectFrame, image = "DymaphyLogo", compound = "image"), row = 0, column = 3)
    tkgrid(ttklabel(logoProjectFrame, image = "UELogo", compound = "image"), row = 0, column = 4)
    tkgrid(ttklabel(logoProjectFrame, image = "InterregLogo", compound = "image"), row = 0, column = 5)
    tkgrid(ttklabel(logoProjectFrame, image = "MarcoLogo", compound = "image"), row = 0, column = 6)
    tkgrid(ttklabel(logoProjectFrame, image = "regionLogo", compound = "image"), row = 0, column = 7)
    tkgrid(ttklabel(logoProjectFrame, image = "JericonextLogo", compound = "image"), row = 0, column = 8)
}
