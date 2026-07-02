\name{plot.map}
\alias{plot.map}
\title{
Plot cartographical maps
}
\description{
A function to plot cartographical maps of the Roman world and Mediterranean region.
}
\usage{
\method{plot}{map}(x = NULL, type = c("plain", "rp", "si", "tetra", "med"), settl, roads, shipr, 
         main, cap, date, name, fsize, fcol, fsize2, fcol2, xd, yd, new, \dots)
%plot.map(x = NULL, type = c("plain", "rp", "si", "tetra", "med"), settl, roads, shipr, 
%         main, cap, date, name, fsize, fcol, fsize2, fcol2, xd, yd, new, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 acronym of ancient Roman province or Italian region (see "\code{\link{rp}}")
}
  \item{type}{
      Type of cartographical map: 
        
        \code{plain} most of Europe and land around the Mediterranean
        
        \code{rp} ancient Roman provinces
        
        \code{si} Senatorial-Imperial provinces
        
        \code{tetra} First Tetrarchy
        
        \code{med} Mediterranean region
}
  \item{settl}{
  display settlements? (optional and logical, for cartographical map)
}
  \item{roads}{
  display terrestrial routes? (optional and logical, for cartographical map)
}
  \item{shipr}{
  display shipping routes? (optional and logical, for cartographical map)
}
  \item{main}{
  plot's main title (optional)
}
  \item{cap}{
 display caption? (optional and logical, for provinces)
}
  \item{date}{
 display date? (optional and logical, for provinces)
}
  \item{name}{
 display map title name? (optional and logical, for provinces)
}
  \item{fsize}{
 font size in main title (optional)
}
  \item{fcol}{
 font color in main title (optional)
}
  \item{fsize2}{
 font size in date (optional)
}
  \item{fcol2}{
 font color in date (optional)
}
  \item{xd}{
 \emph{x} positioning for the date (optional)
}
  \item{yd}{
 \emph{y} positioning for the date (optional)
}
  \item{new}{
 whether the plotted map has superimposed graphics (optional)
}
  \item{\dots}{
  additional optional parameters
}
}
\details{
This plot function is for creating cartographical maps of ancient provinces and Italian regions of the Roman Empire around the year AD 117. 
The input data \code{x} can be a character vector, but this is intended for a recording output. 
By default, the argument's \code{name} and \code{cap} are set to \code{TRUE} while the \code{date} is set to \code{FALSE}; 
however, the argument \code{main} prevails over \code{name}.

The \code{type} argument allows plotting cartographical maps related to the Roman Empire and the Mediterranean basin 
as specified in \code{rpmp} and \code{\link{rpmcd}} datasets. 
In the cartographical maps, settlements are displayed as circles while squares are for military forts, while terrestrial and maritime routes are given as solid paths with different colours. 
Shapes of places and routes are specified in the \code{retn} dataset.


Dataset \code{retn} is a list of lists with specifications to plot different cartographical maps of the Roman Empire and 
the Mediterranean with transport network including settlements, roads, and shipping routes.
This list of lists object has the shape data in different slots for 4 cartographical maps of the Roman Empire 
with names \code{rcoast} for a plain map, \code{rpcoast} for a map with provinces, \code{rpsi} for a map 
with senatorial and imperial provinces, and \code{rptetra} for a tetrarchy map. 
These options for cartographical maps in the Mediterranean are for both the classical and the late antiquity periods. 
Three components in \code{retn} dataset have coordinates for settlements \code{nds}, roads \code{rds}, and shipping 
routes \code{srs} for these maps. 
In addition, the dataset has a cartographical map of the Mediterranean in \code{med} where settlements 
and transport network is yet to complete.


Dataset \code{rpmp} is a list with specifications to plot cartographical maps of ancient Roman provinces and Italian regions. 
This list of lists object has 59 Roman provinces and Italian regions in year 117AD, and where \code{names(rpmp)} gives the 
province acronyms according to \code{rp} dataset. 
Each province in \code{rpmp} has a two-length list with the province name and the shape data for a cartographical map in 
different slots. 
}
\value{
A plot of a cartographical map for the Roman world with a title name, and a caption with an approximate province establishment date.
}
\references{
https://github.com/sdam-au/sdam/tree/master/data

https://github.com/mplex/cedhar/tree/master/pkg/sdam/data
}
\author{
Antonio Rivero Ostoic
}
\note{
Positions for caption and date are for a PDF output and the rendering may vary for browser displays. 
}

\section{Warning }{
\code{rpmp} and \code{retn} are built-in datasets in the development and legacy version of the package but, 
because of its size, are not part of the CRAN distribution, which means that they are downloaded from another repository. 
}

\seealso{
\code{rpmp}, \code{\link{rpmcd}}, \code{retn}, \code{\link{get.edh}}
}
\examples{
# Roman Empire transport network
plot.map(roads=TRUE, shipr=TRUE)

\donttest{
# Roman province of Aegyptus
plot.map(x="Aeg")}
}
\keyword{ graphs }
\keyword{ utilities }
