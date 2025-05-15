\name{kohonen.SDA}
\alias{kohonen.SDA}
\title{Kohonen's self-organizing maps for symbolic interval-valued data}
\description{Kohonen's self-organizing maps for a set of symbolic objects described by interval-valued variables}
\usage{
kohonen.SDA(data, rlen=100, alpha=c(0.05,0.01))
}
\arguments{
\item{data}{symbolic data table in simple form (see \code{\link{SO2Simple}})}
\item{rlen}{number of iterations (the number of times the complete data set will be presented to the network)}
\item{alpha}{learning rate, determining the size of the adjustments during training. Default is to decline linearly from 0.05 to 0.01 over rlen updates}
}
\value{
\item{clas}{vector of mini-class belonginers in a test set}
\item{prot}{prototypes}
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl}, Justyna Wilk \email{justyna.wilk@ue.wroc.pl}
Department of Econometrics and Computer Science, Wroclaw University of Economics, Poland \url{http://keii.ue.wroc.pl/symbolicDA/}
}
\references{
Kohonen, T. (1995), \emph{Self-Organizing Maps}, Springer, Berlin-Heidelberg.

Bock, H.H. (2001), \emph{Clustering Algorithms and Kohonen Maps for Symbolic Data}, International Conference on New Trends in Computational Statistics with Biomedical Applications, ICNCB Proceedings, Osaka, pp. 203-215.

Bock, H.H., Diday, E. (eds.) (2000), \emph{Analysis of Symbolic Data. Explanatory Methods for Extracting Statistical Information from Complex Data}, Springer-Verlag, Berlin.

Diday, E., Noirhomme-Fraiture, M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester, pp. 373-392.
}
\details{
See file \url{../doc/kohonenSDA_details.pdf} for further details
}
\seealso{
\code{SO2Simple}; \code{som} in \code{kohonen} library
}
\examples{
# Example will be available in next version of package, thank You for your patience :-)
}
\keyword{cluster}
