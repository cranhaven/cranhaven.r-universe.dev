\name{balance8}

\docType{data}

\alias{balance8}

\title{Repeated measures on the balance scale}

\description{

	Data are from 8 repeated measurements of 25 balance scale items with 1004 
	participants ranging from 6-17 years of age. The data are aggregated over
	5 item types: weight (w), distance (d), conflict weight (cw), conflict 
	distance (cd), and conflict balance (cb). 
	
}

\usage{data(balance8)}
\format{
  A data frame with 8032 observations on the following 23 variables.
  \describe{
    \item{\code{id}}{the participant id number.}
    \item{\code{age}}{the participant age in years.}
    \item{\code{sex}}{a factor with levels \code{m} \code{f}}
    \item{\code{group}}{the participant school group number; groups 3-8 are in primary school; 
	corollary10 and 11 in secondary school.}
    \item{\code{school}}{a factor with levels \code{primary} \code{secondary}}
    \item{\code{time}}{the number of the repeated measurement.}
    \item{\code{wc}}{the number of correct weight items.}
    \item{\code{wi}}{the number of incorrect weight items.}
    \item{\code{ws}}{the total number of weight items.}
    \item{\code{dc}}{the number of correct distance items.}
    \item{\code{di}}{the number of incorrect distance items.}
    \item{\code{ds}}{the total number of distance items.}
    \item{\code{cwc}}{the number of correct conflict-weight items.}
    \item{\code{cwi}}{the number of incorrect conflict-weight items.}
    \item{\code{cws}}{the total number of conflict-weight items.}
    \item{\code{cdc}}{the number of correct conflict-distance items.}
    \item{\code{cdi}}{the number of incorrect conflict-distance items.}
    \item{\code{cds}}{the total number of conflict-distance items.}
    \item{\code{cbc}}{the number of correct conflict-balance items.}
    \item{\code{cbi}}{the number of incorrect conflict-balance items.}
    \item{\code{cbs}}{the total number of conflict-balance items.}
    \item{\code{totalCor}}{the total number of correct  items.}
	\item{\code{totalTrials}}{the total number of items.}
  }
}

\source{
	
	Brenda Jansen, University of Amsterdam. Unpublished data. 
	
}

\examples{
data(balance8)
}

\keyword{datasets}
