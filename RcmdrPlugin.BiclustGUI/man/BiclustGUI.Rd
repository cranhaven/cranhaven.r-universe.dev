\docType{package}
\name{RcmdrPlugin.BiclustGUI-package}
\alias{RcmdrPlugin.BiclustGUI-package}
\alias{RcmdrPlugin.BiclustGUI}
\title{The BiclustGUI package}
\description{
\code{RcmdrPlugin.BiclustGUI} is a plug-in package for R Commander (\code{Rcmdr}). 
The package is a \emph{Graphical User Interface} (GUI) in which several 
\emph{biclustering} methods can be executed, followed by diagnostics and 
plots of the results. Further, the GUI also has the possibility to connect the methods to more general diagnostic packages for biclustering.
Biclustering methods from  \pkg{biclust}, \pkg{fabia}, \pkg{iBBiG}, \pkg{isa2}, \pkg{BiBitR} \pkg{rqubic} and \pkg{BicARE} are implemented. Additionally, \pkg{superbiclust} and \pkg{BcDiag} are also implemented to be able to further investigate results.
The GUI also provides a couple of extra utilities to export, save, search through and plot the results.

\code{RcmdrPlugin.BiclustGUI} also provides a very specific framework for biclustering in which new methods, diagnostics and plots can be added. 
Scripts were prepared so that R-package developers can freely design their own dialogs in the GUI which can then be added by the maintainer of \code{RcmdrPlugin.BiclustGUI}. 
These scripts do not required any knowledge of \pkg{tcltk} and \pkg{Rcmdr} and are easy to fill in.

}

\details{
More detailed information about all the implementations and scripting of \code{RcmdrPlugin.BiclustGUI} can be found in the vignette. (See the doc folder of package)
}
\author{Ewoud De Troyer \email{ewoud.detroyer@uhasselt.be}}
\references{
Kasim, A., Shkedy, Z., Kaiser, S., Hochreiter, S. and Talloen W. (2016), ``Applied Biclustering Methods for Big and High Dimensional Data Using R'', \emph{CRC}
 }
\seealso{
  The Bicluster algorithms in the packages: \code{\link{biclust}}, \code{\link{fabia}}, \code{\link{iBBiG}}, \code{rqubic} and \code{\link{BicARE}}.
  
  Additional Plotting & Diagnostics packages: \code{\link{superbiclust}} and \code{\link{BcDiag-package}}
}


% RD's to do:
% One for all internals
% One for ALL the functions in the scripts-using
% A couple of specific ones for some GUI functions (or ALL the GUI ones in one )
% page for all the windows
