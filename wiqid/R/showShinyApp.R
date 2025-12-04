# Run a Shiny App from a Package

# Adapted from code by Jason Bryer (jason@@bryer.org) on Github at
#  https://github.com/jbryer/IS606
# This version will only run apps in the wiqid package.

showShinyApp <- function(topic) {
    
	shinyPath <- file.path(path.package("wiqid"), 'shiny')
  apps <- list.dirs(shinyPath, recursive=FALSE, full.names=FALSE)
  if(missing(topic)) {
    cat("The following topics are available:\n")
    return(apps)
  }
  
  if(!requireNamespace("shiny", quietly=TRUE))
    stop("You need to install the 'shiny' package to run this function.")

  which <- pmatch(topic, apps)
  if(is.na(which))
    stop("No app for this topic available.")

  appPath <- file.path(path.package("wiqid"), 'shiny', apps[which])
  cat("--- R will freeze while the app is running; press Esc when done. ---\n") ; flush.console()
  try(shiny::runApp(appPath))
}
