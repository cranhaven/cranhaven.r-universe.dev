\name{getInteUMGraph}
\alias{getInteUMGraph}
\title{Get the reconstructed condition-specific lncRNA-regulated signal pathways}
\description{
  Get the reconstructed condition-specific LncRNA-regulated signal pathways embedded by lncRNAs of competitive regulation through linking to their regulated-mRNAs.
}
\usage{
getInteUMGraph(LncGenePairs)
}
\arguments{
  \item{LncGenePairs}{A data frame. It contains two columns, the first is lncRNA names and the second is mRNA names.}
}
\details{
The argument "LncGenePairs" represents competitively regulated lncRNA-mRNA interactions, which can be returned from the \code{getLncGenePairs}.
 }
\value{
A graph list. 
}
\author{ Xinrui Shi, Chunquan Li and Xia Li}
\seealso{\code{\link{getLncGenePairs}}}
\examples{
\dontrun{

### Integrate lncRNAs of competitive regulation into KEGG pathway graphs ###
  LncGenePairs<-GetExampleData(exampleData="LncGenePairs")
  inteUMGraph<-getInteUMGraph(LncGenePairs)
 }
}
\keyword{ file }