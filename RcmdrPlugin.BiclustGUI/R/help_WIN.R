# Project: Master Thesis
# 
# Author: Ewoud
###############################################################################


biclustGUI_help <- function(){
	print(help(RcmdrPlugin.BiclustGUI))
}

openGUIScripts <- function(){ # Possible only works for windows atm
	dir <- file.path(system.file("scripts",package="RcmdrPlugin.BiclustGUI"))
	browseURL(dir)

}

openGUIVignette <- function(){ # ONLY works for windows, can use openPDF from "Biobase"
# OLD CODE
#	dir <- file.path(system.file("doc",package="RcmdrPlugin.BiclustGUI"),"vignette.pdf")
#	shell.exec(dir) #browseURL would also work, since it uses shell.exec when browseURL has no browser as input
#	
	print(vignette("GuideBiclustGUI",package="RcmdrPlugin.BiclustGUI"))
}

#placeholder <- function(){}