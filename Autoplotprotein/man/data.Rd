\name{data}
\alias{data}
\title{
Save the information}
\description{
Keep all the information of the painted protein in a file}
\usage{
data()
}
\details{
Save information, including protein mutation point information, domain information, option information, enlargement information, protein information, length information and site information}
\value{
Data of various kinds of information
}
\references{
https://cran.r-project.org/doc/manuals/R-exts.html
}
\author{
Xiaoyu Zhang
}
\seealso{
code{\link{help}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function () 
{
    library("ade4")
    library("seqinr")
    library("plotrix")
    protein = read.table("Protein.txt", sep = "\t", stringsAsFactors = F)
    domain = read.table("Domain.txt", sep = "\t", stringsAsFactors = F)
    length = read.table("Length.txt", sep = "\t", stringsAsFactors = F)
    site = read.table("Site.txt", sep = "\t", stringsAsFactors = F)
    muta = read.table("Mutagenesis.txt", sep = "\t", stringsAsFactors = F)
    option = read.table("Option.txt", sep = "\t", stringsAsFactors = F)
    zoomin = read.table("ZoomIn.txt", sep = "\t", stringsAsFactors = F)
    c <- merge(muta, domain, all = T, sort = FALSE)
    c <- merge(c, option, all = T, sort = FALSE)
    c <- merge(c, zoomin, all = T, sort = FALSE)
    c <- merge(c, protein, all = T, sort = FALSE)
    c <- merge(c, length, all = T, sort = FALSE)
    c <- merge(c, site, all = T, sort = FALSE)
    write.table(c, file = "data.txt", sep = "\t", quote = FALSE, 
        row.names = F, col.names = F)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{data}

