\name{interface}
\alias{interface.scaleboot}
\alias{read.mt}
\alias{read.ass}
\alias{read.cnt}
\alias{sbfit.pvclust}
\alias{sbpvclust}
\title{Interface to External Packages}
\description{
  Interface for other packages such as
  CONSEL (phylogenetic inference), and pvclust (hierarchical clustering)
}
\usage{
read.mt(file,tlab="t")

read.ass(file,identity=TRUE,tlab="t",elab="e") 

read.cnt(file)


\method{sbfit}{pvclust}(x,...)

sbpvclust(x,mbs,k=3,select="average",...)
}
\arguments{
  \item{file}{character of a file name to be read.}
  \item{identity}{logical. Should the identity association be included?}
  \item{tlab}{character for basename of tree labels.}
  \item{elab}{character for basename of edge labels.}
  \item{x}{an object of class \code{"pvclust"}.}
  \item{mbs}{an object of class \code{"scalebootv"}.}
  \item{k}{numeric of \eqn{k} for a AU p-value.}
  \item{select}{character of model name (such as "poly.3") or one of
"average" and "best".}
  \item{...}{further arguments passed to or from other methods.}
}
\details{
   CONSEL is a program package consisting of small programs written in
   the C language for assessing the confidence of phylogenetic tree selection.
   Some functions for interfacing with CONSEL are:
   \code{read.mt},   \code{read.ass},  and \code{read.cnt}
   for reading, respectively, \code{mt}, \code{ass}, and \code{cnt}
   format. Once \code{mt} file is read, we can calculate improved
   versions of approximately unbiased p-values by \code{relltest}in
   \code{scaleboot} instead of CONSEL.
   
   pvclust is a R package for hierarchical clustering with p-values.
   Functions for interface to pvclust are: \code{sbfit} method for an
   object of class \code{"pvclust"} to convert it to \code{"scalebootv"} class,
   and \code{sbpvclust} for writing back the result to a
   \code{"pvclust"} object with additional class \code{"sbclust"}.
   \code{plot} method for class \code{"sbclust"} overwrites that for \code{"pvclust"}.
 }
\value{
  \code{read.mt} returns a matrix of dimension sequence-length by tree-number.
  If \code{identity=FALSE}, then \code{read.ass} returns a list containing components
  \code{x} for edge->tree associations and \code{y} for tree\eqn{\to}edge associations.
  If \code{identity=TRUE}, \code{read.ass} returns a list vector of edge\eqn{\to}tree
  associations, where the identity associations for tree\eqn{\to}tree are included.
  \code{read.cnt} returns a list containing components \code{bps},
  \code{nb}, and \code{sa} to be used for \code{sbfit}. The list also contains
  components \code{cnt}, \code{id}, and \code{val}.

  \code{sbfit.pvclust} returns an object of class
  \code{"scalebootv"}. \code{sbpvclust} returns an object of class
  \code{"sbclust"} added to the class \code{"pvclust"}.
}
\references{
Shimodaira, H. and Hasegawa, M. (2001). CONSEL: for
assessing the confidence of phylogenetic tree selection,
\emph{Bioinformatics}, 17, 1246-1247 (software is available from
\url{http://stat.sys.i.kyoto-u.ac.jp/prog/consel/}).

Suzuki, R. and Shimodaira, H. (2006).
pvclust: An R package for hierarchical clustering with p-values,
\emph{Bioinformatics}, 22, 1540-1542 (software is available from CRAN or
\url{http://stat.sys.i.kyoto-u.ac.jp/prog/pvclust/}).
}
\author{Hidetoshi Shimodaira}
\seealso{\code{\link{lung73}}, \code{\link{mam15}}, \code{\link{relltest}}}
\examples{
## replace au/bp entries in pvclust object
## see help(lung73) for details
library(pvclust)
data(lung73)
plot(lung73.pvclust) # draw dendrogram of pvlcust object
pvrect(lung73.pvclust)
lung73.new <- sbpvclust(lung73.pvclust,lung73.sb) # au <- k.3
plot(lung73.new) # redraw dendrogram with the new au/bp values
pvrect(lung73.new)

\dontrun{
## reading CONSEL files
## sample files are found in mam15 subdirectory
## see help(mam15) for details
mam15.mt <- read.mt("mam15.mt")
mam15.ass <- read.ass("mam15.ass")
mam15.cnt <- read.cnt("mam15.cnt")
}

}
\keyword{ file }

