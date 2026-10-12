\name{key.sector}
\alias{key.sector}
\title{Impact Analysis via Backward and Forward Linkages}
\description{Uses backward and forward \code{\link{linkages}} to identify key sectors in the system. Can calculate total and direct linkages. If the data is multiregional, intraregional and interregional linkages can be calculated. Can also be used on a specified subset of all regions.}
\usage{
key.sector(io, ES = NULL, crit = 1, regions = "all", sectors = "all", 
           type = c("direct"), intra.inter = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{io}{An object of class \code{InputOutput} calculated from \code{\link{as.inputoutput}}.}
  \item{ES}{An object of class \code{EasySelect} from \code{\link{easy.select}}}
  \item{crit}{Integer. The value to compare linkages above or below to classify sectors. Default is 1.}
  \item{regions}{Character or Integer. Specific regions to be used. Can either be a character that exactly matches the name of the region in \code{RS_label} or the number of the region in the order it appears in \code{RS_label}.}
  \item{sectors}{Character or Integer. Specific sectors to be used. Can either be a character that exactly matches the name of the sector in \code{RS_label} or the number of the sector in the order it \code{RS_label}.}
  \item{type}{Character. Identifying the type of backward and forward linkages to be calculated. Options are \code{"total"} and \code{"direct"}.}
  \item{intra.inter}{Logical. Only applies to multiregional systems. Determines whether or not to calculate intraregional and interregional backward and forward linkages in addition to aggregate linkages.}
}

\details{Uses the (various) specified backward and forward \code{\link{linkages}} to calculate a key to identify dependence using the specified critical value.

\code{I}   BL < crit, FL < crit - Generally independent

\code{II}  BL < crit, FL > crit - Dependent on interindustry demand

\code{III} BL > crit, FL > crit - Generally dependent

\code{IV}  BL > crit, FL < crit - Dependent on interindustry supply
}


\value{If there is only one region, key sector binds to the output from \code{\link{linkages}} to make a table. Otherwise, it produces a list of key sector codes for each country using the names of regions provided. See Examples for more details.}

\references{Blair, P.D. and Miller, R.E. (2009). "Input-Output Analysis: Foundations and Extensions". Cambridge University Press

Nazara, Suahasil & Guo, Dong & Hewings, Geoffrey J.D., & Dridi, Chokri, 2003. "PyIO. Input-Output Analysis with Python". REAL Discussion Paper 03-t-23. University of Illinois at Urbana-Champaign. (\url{http://www.real.illinois.edu/d-paper/03/03-t-23.pdf})}
\author{
John J. P. Wade, Ignacio Sarmiento-Barbieri
}

\seealso{
\code{\link{linkages}}, \code{\link{as.inputoutput}}
}
\examples{
data(toy.IO)
class(toy.IO)
key1 <- key.sector(toy.IO)
key1$Narnia

# A more detailed example
# Using critical value of 2 because this is randomly generated data and better 
# illustrates functionality
key2 <- key.sector(toy.IO, intra.inter = TRUE, type = c("direct"), crit = 2)
key2

key3 <- key.sector(toy.IO, regions = c(1:2), sectors = c(1:3,5))
key3
}

