% varrank.Rd ---
% Author           : Gilles Kratzer
% Created on :       08.02.2018
% Last modification :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{plot.varrank}
\alias{plot.varrank}
\title{Visualization of varrank output}

\usage{
\method{plot}{varrank}(x,
                       ## block separation
                       colsep = TRUE,
                       rowsep = TRUE,
                       sepcol ="white",
                       sepwidth=c(0.005,0.005),

                       ## cell labeling
                       cellnote = TRUE,
                       notecex = 1.5,
                       notecol = "black",
                       digitcell = 3,

                       ## Row/Column Labelling
                       margins = c(6, 6, 4, 2),
                       labelscex = 1.2,

                       ## color key + density info
                       colkey = NULL,
                       densadj = 0.25,
                       textlines = 2,

                       ## plot labels
                       main = NULL,
                       maincex = 1,
                       \dots)}

\arguments{
  \item{x}{an object of class \code{varrank}.}
  \item{colsep}{(optional) a logical parameter to indicate if columns should be separated from others by narrow space of color. The default is \code{TRUE}.}
  \item{rowsep}{(optional) a logical parameter to indicate if rows should be separated from others by narrow space of color. The default is \code{TRUE}.}
  \item{sepcol}{(optional) the color to use to separate rows or columns. The default is \code{white}.}
  \item{sepwidth}{(optional) Vector of length 2 giving the width
    (colsep) or height (rowsep) the separator box drawn by colsep and
    rowsep as a function of the width (colsep) or height (rowsep) of a
    cell. The defaults is \code{c(0.005, 0.005)}.}

  % cell labeling
  \item{cellnote}{(optional) a logical parameter to indicate if the scores should be displayed in cells.}
  \item{notecex}{(optional) numeric scaling factor for \code{scores}. The default is 1.5.}
  \item{notecol}{(optional) character string specifying the color for
    \code{cellnote} text.  Defaults to "black".}
    \item{digitcell}{(optional) integer that indicate how many digits of the \code{scores} should be displayed. The default is 3.}
    % Row/Column Labeling
\item{labelscex}{the magnification factor to be used for x and y labels relative to the current setting of cex. The default is 1.2.}
\item{margins}{numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(6, 6, 4, 2).}

% color key + density info
\item{colkey}{specification for the color scheme to be used. The default is a rainbow color scheme.}
\item{densadj}{numeric scaling value for tuning the kernel width when
    a density plot is drawn on the color key.  (See the \code{adjust}
    parameter for the \code{density} function for details.)  Defaults is
    0.25.}

  \item{textlines}{number of lines to display \code{relevance/redundance} in the key. Default is 2.}

%plot labels
\item{main}{an overall title for the plot. Default is none.}
\item{maincex}{main magnification to be used for the plot. Default is 1.}
\item{\dots}{additional arguments passed to image.}
  }

\description{
\code{plot} method for \code{varrank} objects with multiple options.
}

\details{
This \code{plot} method for \code{varrank} objects provides an
  extensible framework for the visualization \code{varrank} output analysis. The
  user is allowed to specify block separations, display of scores and the color scheme to be used. The other parameters give a full control on the output. The final rendering depends on the \code{algorithm} used. For a `forward` search the key density is on the upper right corner and for a `backward`
 search the key density is in the bottom left corner.
  The default color scheme is continuous heat color from blue to red. A
  popular alternative for creating color palettes is \pkg{RColorBrewer},
  \url{https://cran.r-project.org/package=RColorBrewer}.

  This \code{plot} method is very similar to the \code{heatmap.2} function from \pkg{gplots}, \url{https://cran.r-project.org/package=gplots}.
}

\author{Gilles Kratzer}

\examples{
if (requireNamespace("mlbench", quietly = TRUE)) {
library(mlbench)
data(PimaIndiansDiabetes)

##forward search for all variables
out <- varrank(data.df = PimaIndiansDiabetes,
  method = "estevez",
  variable.important = "diabetes",
  discretization.method = "sturges",
  algorithm = "forward",scheme="mid")

##default output
plot(x = out)

##typical plot for high dimensional datasets
plot(x = out, colsep = FALSE, rowsep = FALSE, cellnote = FALSE)
}
}
