\name{fibroblast}
\alias{fibroblast}
\docType{data}
\title{
Fibroblast Cell Line dataset
}
\description{
Genomic sequences of 15 fibroblast cell lines. 
}
\usage{data(fibroblast)}
\format{
  A data frame with 2462 observations on the following 11 variables.
  \describe{
    \item{\code{Chromosome}}{a numeric vector to identify the chromosome}
    \item{\code{Genome.Order}}{a numeric vector meaning the genome index}
    \item{\code{gm05296}}{cell line GM05296}
    \item{\code{gm03563}}{cell line GM03563}
    \item{\code{gm01535}}{cell line GM01535}
    \item{\code{gm07081}}{cell line GM07081}
    \item{\code{gm01750}}{cell line GM01750}
    \item{\code{gm03134}}{cell line GM03134}
    \item{\code{gm13330}}{cell line GM13330}
    \item{\code{gm13031}}{cell line GM13031}
    \item{\code{gm01524}}{cell line GM01524}
  }
}
\details{
Data come from a single experiments on 15 fibroblast cell lines with each array
containing over 2000 (mapped) BACs spotted in triplicate. The variable in the 
dataset is the normalized average of the log base 2 test over reference ratio.}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Snijders, A. M., Nowak, N., Segraves, R., Blackwood, S., Brown, N., Conroy, J., Hamilton, G.,
Hindle, A. K., Huey, B., Kimura, K. et al., (2001). Assembly of microarrays for genome-wide measurement
of DNA copy number. Nature Genetics 29, 263-264.
}
\examples{
\dontrun{
data(fibroblast)
#select chromosome 1
z<-na.omit(fibroblast$gm03563[fibroblast$Chromosome==1]) 
o<-jumpoints(z,k=30,output="3")
plot(z)
plot(o,add=TRUE,y=FALSE,col=4)
}
}
\keyword{datasets}
