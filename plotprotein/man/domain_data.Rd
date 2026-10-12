\name{domain_data}
\alias{domain_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
downloading protein length
}
\description{
Load the start and end positions of the domain
}
\usage{
domain_data()
}
%- maybe also 'usage' for other objects documented here.
\details{
The tool ennable visualization of amino acid changes at the protein level,The scale of a protein domain and the position of a functional motif/site will be precisely defined. The features available include domains
}
\value{

The start and end positions of the domain
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
    library(XML)
    library(plyr)
    protein = read.table("Protein.txt", sep = "\t", stringsAsFactors = F)
    name = protein[2]
    url_p = "http://www.uniprot.org/uniprot/"
    url_s = "#showFeatures"
    url_w = paste(url_p, name, url_s, sep = "")
    url = url_w
    doc <- htmlParse(url)
    position_d = xpathSApply (doc, "//table[@id= 'domainsAnno_section'] 
  /tr/td/  a[@class = 'position tooltipped']",  
        xmlValue)
    name_d = xpathSApply (doc, "//table[@id= 'domainsAnno_section']/tr/td/span[@property='text']", 
        xmlValue)
    s_d = c()
    for (i in 1:length(position_d)) {
        s_d[i] <- gsub(pattern = "//D", replacement = "x", position_d[i])
    }
    s_d <- strsplit(s_d, "xxx")
    d1_d <- laply(s_d, function(x) x[1])
    d2_d <- laply(s_d, function(x) x[2])
    r1_d = d1_d
    r2_d = d2_d
    r3_d = name_d
    dfrm_d = data.frame(r1_d, r2_d, r3_d)
    write.table(dfrm_d, file = "Domain.txt", sep = "/t", quote = FALSE, 
        row.names = F, col.names = F)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ domain data }
\keyword{ file }
