\name{optimalfrequency}
\alias{optimalfrequency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Evaluate potential haplotypes.
}
\description{
Calculate the difference of a known haplotype and the resulting unordered pair of snip pairs. 
}
\usage{
optimalfrequency(mm, mmorg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mm}{
   This is a contigency table of two haplotype snips.
   2x2 matrix or ndata.frame 
}
  \item{mmorg}{
   This is a contigency table of two diplotype snips pairs.
}
}
\details{
The average squared distance to the expected result 3x3 table is used as a T statistic. The p value not to be zero is calculated.
The higher the p value the more exact is the haplotype. 
}
\value{
A list of values is returned.
\item{result\$LK}{ Linkage disequilibrium }
\item{result\$Testvalue}{ The squared sitance multiplied by the number of entries in 3x3 matrix mmorg }
\item{result\$prSimilarByChange}{ The probability not o be equal to zero by change. }
}
\references{
Stahel: Statistik fuer Naturwissenschaftler und Medizinier, pp. 107-120. 
}
\author{
Jan wolfertz.
}

\seealso{
findoptimal
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{haplotype}
\keyword{'model-fit'}
