#' @export
CADStat.help <- function(topic=NULL,doc="CADStat.JGR.html",doc.path=file.path("CADStat","doc"),home=.Library)
{
  ## CADStat.help will open an xml, html, xhtml, sxw, or pdf in a browser
  ## e.g., CADStat.help("speciestolerancevalues")
  
  ## If topic is not null, create a link.
  
  if(!is.null(topic)){
    docFile=paste(topic,".html",sep="")
  } else {
    docFile=doc
  }
  
  ## In some installations of R, mainly ones installed on a private computer or laptop,
  ## multiple library paths are created. This causes CADStat to incorrectly link help pages when
  ## .Library is used to refer to the directory where CADStat is supposed to be installed.
  ##
  ## This section of code checks fallback locations for the appropriate help file before
  ## informing the user that it can't find a help page.

  ## Checks if help page can be found in the specified location.
  basePath = file.path(home[1],doc.path, docFile)
  
  if (!file.exists(basePath)){
    cat("Help page not found in '",basePath,"', looking in other library paths...\n",sep="")

    ## If not, search in fallback locations.
    for(path in .libPaths()){
      ## Quit looking on the first successful path.
      if (file.exists(file.path(path,doc.path,docFile))){
        basePath = file.path(path, doc.path, docFile)
        cat("\tFound help file in ",basePath)
        break;
      }
    }
  }
  ## Set the help URL
  helpURL <- paste("file:///",basePath,sep="")  
  
  ## browseHelp(helpURL)

  ## If the path still refers to a non-existant file, warn the user.
  if (!file.exists(basePath)){
  cat("\n")
  cat("Could not find help page '",doc,"'.",sep="")
  
  ## Otherwise, open the location as a web page in the browser.
  } else{
    ## Note: this trick only works in Windows, where a default browser is set.
    ## In other operating systems, such as Linux or OSX, it will be necessary to
    ## specify a browser or use some other tool to locate an appropriate browser.
    browseURL(helpURL,browser=NULL)
  } 
  invisible("")
}

## Deprecated function for opening a help page. Evidently, CADStat originally shipped with k-meleon as a browser.
browseHelp <- function(url="about:blank")
{
  win.browser.path <- file.path(.Library, "CADStat","doc","browser","k-meleon.exe")
  
  if (.Platform$OS.type == 'windows' && file.exists(win.browser.path))
  {
    url <- chartr("/", "\\", url)

    win.browser.path <- file.path(.Library, "CADStat","doc","browser","k-meleon.exe")
    cmd <- paste("\"",win.browser.path,"\" \"",url,"\"",sep="")
    
    system(cmd, wait = FALSE)
  }
  
  else
  {
    browseURL(url)
  }
}

