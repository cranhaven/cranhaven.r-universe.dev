\name{SRH}
\alias{SRH}
\alias{SRH.seq}
\docType{data}
\title{
Longitudinal data on self rated health
}
\description{
Longitudinal data on self rated health from waves 1-11 of the Swiss household panel
}
\usage{data(SRH)
}
\format{
  \code{SRH} is a data frame with 2612 observations on the following 15 variables.
  \describe{
    \item{\code{idpers}}{personal identification number}
    \item{\code{sex}}{a factor with levels \code{man} \code{woman}}
    \item{\code{birthy}}{birth year of the respondent}
    \item{\code{wp09lp1s}}{longitudinal weight}
    \item{\code{p99c01} ... \code{p09c01}}{factors with levels:\cr
\code{ very well; well; so, so (average); not very well; not well at all}
}
}
  \code{SRH.seq} is a \code{TraMineR} sequence object created from the \code{SRH} data frame using the code in \code{example}. States are coded as follows:\cr

\tabular{ll}{
\code{G1} \tab (very well) \cr
\code{G2} \tab (well)\cr
\code{M}  \tab (so, so (average)) \cr
\code{B2} \tab (not very well) \cr
\code{B1} \tab (not well at all) \cr
}

}
\details{
Respondant's self rated health is collected at each yearly wave of the SHP with the following question: \emph{How do you feel right now?}. Possible answers are: very well; well; so, so (average), not very well and not well at all. The sequences are made of an individual's responses over 11 yearly waves of the SHP, starting with wave 1 in 1999. Variable p99c01 contains the self rated health at wave 1, p00c01 contains the self rated health at wave 2, etc... Note that sequences may contain missing values due to wave or item non response. 
}
\source{
Swiss Household Panel: \url{https://forscenter.ch/projects/swiss-household-panel/}
}

\examples{
## Preparing a sequence object with the SRH data set
data(SRH)

## Long state labels
state.list <- levels(SRH$p99c01)

## Sequential color palette
mycol5 <- rev(brewer.pal(5, "RdYlGn"))

## Creating the sequence object
SRH.seq <- seqdef(SRH, 5:15, alphabet=state.list, 
	states=c("G1", "G2", "M", "B2", "B1"), labels=state.list, 
	weights=SRH$wp09lp1s, right=NA, cpal=mycol5)
names(SRH.seq) <- 1999:2009
}
\keyword{datasets}
