\name{haplotypeit}
\alias{haplotypeit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Haplotype a 3 x 3 counting matrix. 
}
\description{
This functions recalculates the potential 2x2 haplotype matrics.
It gets a 3x3 matrixs and returns a list A,B,C,D of vectors.
A[1],B[1],C[1],D[1] is the first solution of the matrix. There are always three solutions.
}
\usage{
haplotypeit(a, b, c, d, e, f, g, h, i)
}
\arguments{
  \item{a}{
   Number of counts of matching snip pairs.
}
  \item{b}{
   Number of counts of matching snip pairs..
}
  \item{c}{
  Number of counts of matching snip pairs.
}
  \item{d}{
  Number of counts of matching snip pairs.
}
  \item{e}{
  Number of counts of matching snip pairs. 
}
  \item{f}{
  Number of counts of matching snip pairs.
}
  \item{g}{
  Number of counts of matching snip pairs.
}
  \item{h}{
  Number of counts of matching snip pairs.
}
  \item{i}{
  Number of counts of matching snip pairs.
}
}
\details{
The software automatically resolves the cases e=0 by circumventing the cubic equation. If the degree of the equation is lower additional copies of some solution will be made to produce the outputformat.
The output format is a list of four vectors of copefficients. Each vector contains three complex numbers.
}
\value{
output$A is a vector of length 3.
output$B,Output$C, output$D is eighter of length 3.
One potential solution is A[1],B[1],C[1],D[1].
}
\references{
This methods refers to an article:
David Clayton. An r    package for analysis of whole-
genome association studies. Human Heredity, 
64(1):45 - 51, 2007.     doi: doi:10.1001/archgenpsychiatry.2010.25. URL
http://archpsyc.jamanetwork.com/article.aspx?articleid=210679.
}
\author{
Jan wolfertz
}
\seealso{
callhaplotype
}
\examples{
haplotypeit(4,0,0,0,30,0,0,0,23)
print("##############################")
print("This tests the cubic routine")
haplotypeit(4,0,0,0,30,0,0,0,23)
### Formated of 4 digits
print("Formated of 4 digits")
round(as.numeric(Re(haplotypeit(4,0,0,0,30,0,0,0,23)$A)),digit=4)
round(as.numeric(Re(haplotypeit(4,0,0,0,30,0,0,0,23)$B)),digit=4)
round(as.numeric(Re(haplotypeit(4,0,0,0,30,0,0,0,23)$C)),digit=4)
round(as.numeric(Re(haplotypeit(4,0,0,0,30,0,0,0,23)$D)),digit=4)
### 
}
\keyword{haplotyping}
