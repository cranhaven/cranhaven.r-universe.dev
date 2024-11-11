\name{tk.utils}
\alias{tk.file}
\alias{tk.file}
\alias{tk.files}
\alias{tk.folder}
\alias{handle}

\title{
  Tk interface utilities
}

\description{
  These functions are components used to build interactive interfaces.
  
  \code{tk.file} is a wrapper to tcltk functions \code{tkgetOpenFile} and \code{tkgetOpenFile} with several enhancements.
  
  \code{tk.files} proposes to build and order a file list from multiple calls to \code{tk.file}.
  
  \code{tk.folder} is a wrapper to tcltk function \code{tkchooseDirectory} with small enhancements.
  
  \code{handle} is a wrapper to \code{withRestarts}, which allows to catch errors, warnings and messages while executing an R expression and handle them with custom functions.
}

\usage{
  tk.file(title = "Choose a file", typeNames = "All files", typeExt = "*",
    multiple = FALSE, mandatory = TRUE, type = c("open", "save"), initialdir = NULL,
    parent = NULL)
  tk.files(preselection = character(0), multiple = TRUE, parent = NULL, ...)
  tk.folder(title = "Choose a directory", mustexist = TRUE, mandatory = TRUE)
  handle(expr, messageHandler, warningHandler, errorHandler)
}

\arguments{
  \item{title}{
    Single character value, the displayed name of the summoned window.
  }
  \item{typeNames}{
    Character vector defining the displayed names of the filtered file extensions. Parallel to \code{typeExt}.
  }
  \item{typeExt}{
    Character vector defining the filtered file extensions (use "*" as wildcard). Parallel to \code{typeNames}.
  }
  \item{multiple}{
    Single logical value, whether to allow multiple file selection or not.
  }
  \item{mandatory}{
    Single logical value, whether to throw an error if no file is selected or not.
  }
  \item{type}{
    Single character value defining the label of the button.
  }
  \item{initialdir}{
    Single character value, the initially selected directory when the window is summoned. \code{NULL} corresponds to the last directory browsed via one of these functions.
  }
  \item{parent}{
    A Tcl-Tk object to consider as the parent of the new frame.
  }
  \item{preselection}{
    Character vector, the files already selected when the window is summoned.
  }
  \item{\dots}{
    Further arguments to be passed to \code{tk.file}.
  }
  \item{mustexist}{
    Single logical value, whether to throw an error if a non-existing directory is selected or not.
  }
  \item{expr}{
    An R expression in which errors, warnings and messages are to be caught. Use \code{\{\}} to monitor a multi-line R code.
  }
  \item{messageHandler}{
    A function taking as single argument the \link[=conditions]{condition} object caught. If missing, \link[=message]{messages} will pass through.
  }
  \item{warningHandler}{
    A function taking as single argument the \link[=conditions]{condition} object caught. If missing, \link[=warning]{warnings} will pass through.
  }
  \item{errorHandler}{
    A function taking as single argument the \link[=conditions]{condition} object caught. If missing, \link[=stop]{errors} will pass through.
  }
}

\value{
  \code{tk.file}, \code{tk.files} and \code{tk.folder} return the selection as a character vector, possibly empty.
  
  \code{handle} returns nothing.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{tk.browse}}, \code{\link{tk.convert}}, \code{\link{tk.tracks}}
}

