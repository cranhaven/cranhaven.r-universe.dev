\name{1.2.export.data}
\alias{1.2.export.data}
\alias{export.data}
\alias{exportPEMS}
\alias{exportPEMS2TAB}
\alias{exportPEMS2CSV}


\title{
  exporting PEMS data
}

\description{
  Some functions for exporting data from R and pems.utils.
}

\usage{

exportPEMS(pems, file = "tempfile", file.writer = write.table, 
           sep = "\t", ...)

exportPEMS2TAB(pems, file = "tempfile", file.writer = write.table, 
           sep = "\t", ...) 

exportPEMS2TAB(pems, file = "tempfile", file.writer = write.table, 
           sep = "\t", ...) 

#exportPEMS2Excel 
#currently disabled

}



\arguments{

  \item{pems}{
  (A required object) The object to export from R, typically a \code{data.frame} or 
  \code{pems} object.   
} 

  \item{file}{
  (Character) The name of the file to create when exporting data. This can be 
  \code{'clipboard'}, to export to the clipboard assuming the clipboard buffers are not 
  exceeded.  
} 

  \item{file.writer, sep}{
  (Various arguments) \code{file.writer} is the R function used to create the export file. 
  \code{sep} is the separator argument passed to \code{file.writer}.     
}

  \item{\dots}{
  (Optional) Other arguments, handling varies. For \code{exportPEMS2...} functions, these typically 
  passed to \code{exportPEMS} or from there to the assigned \code{file.writer}. 
}
    
}


\details{

  By default, \code{exportPEMS2TAB} and \code{exportPEMS2CSV} export the data component of a supplied 
  \code{pems} object, to tab-delimited \code{.txt} and comma-delimited \code{.csv} files, 
  respectively. \code{file} sets the file name (default \code{tempfile}).  

  These are typically used in form: 

  \code{exportPEMS2...(pems, file, ...)}

  By default, they make the following associated modifications:

  If file extensions are not included in \code{file}, they add \code{.txt} and \code{.csv} 
  extensions to tab-delimited and comma-delimited files, respectively. The argument 
  \code{tidy.file=FALSE} can be used to disable this modification.

  Time stamps, if identified, are exported in "DD/MM/YYYY HH:MM:SS.x" format. Handling can be 
  altered using \code{time.stamp}, \code{time.format} and \code{tz} arguments like 
  \code{\link{import2PEMS}} or disabled using \code{tidy.time.stamp=FALSE}. 

  Data-series units can also be added to exported file column names in form \code{name(units)} 
  by adding the argument \code{units="add.to.names"}.     
}


\value{

  \code{exportPEMS2...()} functions generate export file from pems data. 
}

\references{

  References in preparation.
}
\author{

  Karl Ropkins
}
\note{

  \code{exportPEMS2Excel} is curently disabled.

  These are very crude functions in the most part because they are rarely used. Suggestions for helpful 
  improvements would be very welcome.
 
}

\section{Warning}{

  Currently, \code{exportPEMS...} functions overwrite without warnings. 

}

\seealso{

  See \code{\link{import2PEMS}}, etc. for importing data into \code{pems.utils}. 
}

\examples{

###########
##example 1 
###########

#making a comma-delimited copy of pems.1

\dontrun{
exportPEMS2CSV(pems.1, "pems.example")
dir()
}

}

\keyword{ methods }
