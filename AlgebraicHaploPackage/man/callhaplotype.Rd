\name{callhaplotype}
\alias{callhaplotype}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calcultes the cotigency table of the haplotypes
}
\description{
It starts with a contigency table of pairs of haplotypes and ends up with the haplotypes one. The heterocygote cases are the middle of the column and rows.
}
\usage{
callhaplotype(dd)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dd}{
  This is a  contingency table. Rows and columns are in the order are AA, AB,BB. 
}
}
\details{
A 2x2  contingency table of haplotypes is calculated. The most likely solution had been choosen.
}
\value{
The haplotype contingency table is returned. All entries are numeric.
}
\references{
David Clayton.      
"An r    package for analysis of whole-genome association studies."
Human Heredity, 64(1):45 - 51, 2007.     
doi: doi:10.1001/archgenpsychiatry.2010.25. 
URL
http://archpsyc.jamanetwork.com/article.aspx?articleid=210679.
Jan Wolfertz(in press.):""
}
\author{
Jan Wolfertz
}
\note{
The differences are the coice of the solution of the cubic equations. About 4 percent differences and about 7 assuming 1 per thousand.
For data export or import you can use a different package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
print("###########################")
dd2=matrix(c(4,0,0,0,30,0,0,0,23),ncol=3,byrow=TRUE)
callhaplotype(dd2)
callhaplotype(dd2)/(2*57)

### The second example
print("##############################")
print("The second example: \n")
dd=matrix(c(1212, 2, 0, 679, 0,0,75,0,0), byrow=TRUE, nrow=3)
colnames(dd)=c("CC","CT","TT")
rownames(dd)=c("CC","CT","TT")
callhaplotype(dd)
print("##############################")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{haplotyping}
\keyword{cubic}
\keyword{haplotypeit}
