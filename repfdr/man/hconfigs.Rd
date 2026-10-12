\name{hconfigs}
\alias{hconfigs}
\title{
Enumeration of all possible vectors of association status.
}
\description{
The function generates a matrix with all possible vectors of association status (in rows), given the number of studies and number of possible association status states in each study (2 or 3).
}
\usage{
hconfigs(n.studies, n.association.status = 3, studies.names = NULL)}
\arguments{
   \item{n.studies}{Number of studies in the analysis.
}
  \item{n.association.status}{either 2 for no-association\\association or 3 for no-association\\negative-association\\positive-association.
}
  \item{studies.names}{
Optional study names to display.
}
}
\details{
 This  matrix should be used when selecting the rows indices for the association status vectors that are in the non-null set, specified by the used in \code{non.null.rows} in the function \code{\link{repfdr}}.
}
\value{Matrix with rows indicating all the possible vectors of association status.
 

}
\seealso{
\code{\link{repfdr}}
}
\examples{

(H <- hconfigs(n.studies = 3))
# in replication analysis the non-null vectors are:
H[apply(H,1,function(y){ sum(y==1)>1 | sum(y==-1)>1 }),]
# in meta-analysis there is only one null vector (c(0,0,0)):
H[rowSums(abs(H))!=0,]

hconfigs(n.studies = 3, n.association.status= 2)
}