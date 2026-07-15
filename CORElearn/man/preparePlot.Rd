\name{preparePlot}
\alias{preparePlot}
\alias{preparePlot.Core}
\title{ Prepare graphics device }
\description{
 Based on provided \code{fileName} opens and sets appropriate graphical device: pdf, postscript, 
 interactive graphical window, or (only on windows) windows metafile,.
 } 
\usage{
preparePlot(fileName="Rplot",...)
 }

\arguments{
  \item{fileName}{ Name of the file to store the output to. }
  \item{...}{ Further parameters passed to device. }
  }
\details{
   The function opens the graphical output device based on \code{fileName} extension. 
   The extensions .pdf, .ps, .jpg, .bmp, .tif, .png, .tiff or none select  \code{\link{pdf}}, \code{\link{postscript}},
   \code{\link{jpeg}}, \code{\link{bmp}}, \code{\link{tiff}},\code{\link{png}}, \code{\link{bitmap}}
    or a default (interactive) graphical device.
   
   On Windows also .emf extension is supported which opens \code{win.metafile}    
    and creates vector graphics in windows enhanced metafile format.      
   
   The extension .tiff opens \code{\link{bitmap}} device which produces bitmap via \code{\link{postscript}} device.
   Therefore it requires Ghostscript to be installed and on the executable path.
   
    Some sensible default values are passed to created devices, but further options can be
   passed via \code{...}.  
}
\value{
A plot device is opened and nothing is returned to the R interpreter.
}

\author{ Marko Robnik-Sikonja }


\seealso{
\code{\link{CORElearn}},
\code{\link{plot.ordEval}},
\code{\link{pdf}}, 
\code{\link{postscript}},
\code{\link{jpeg}}, 
\code{\link{bmp}}, 
\code{\link{tiff}},
\code{\link{png}},
\code{\link{Devices}}    
}

\examples{
   # prepare a data set
    dat <- ordDataGen(200)
    # evaluate ordered features with ordEval
    oe <- ordEval(class ~ ., dat, ordEvalNoRandomNormalizers=200)
    
    # the folowing line if uncommented will create a separate 
    # postscript file with given filename for each attribute   
    # preparePlot("myGraph\%03d.ps") 
    
    plot(oe)
    dev.off()
}

