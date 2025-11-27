\name{subtree}
\docType{methods}
\alias{subtree}
\alias{subtree,PSTf-method}
\title{ Extract a subtree from a segmented PST }
\description{
 Extract a subtree from a segmented PST
}

\usage{
\S4method{subtree}{PSTf}(object, group=NULL, position=NULL)
}

\arguments{
  \item{object}{
	A segmented probabilistic suffix tree, i.e an object of class \code{"\linkS4class{PSTf}"}) as returned by the \code{\link{pstree}}, \code{\link{prune}} or \code{\link{tune}} function.
}
\item{group}{ 
integer. Segment of the PST
}
\item{position}{ 
Not implemented yet. 
}
}
\details{
See also \cite{Gabadinho 2016}.
}
\examples{
## activity calendar for year 2000
## from the Swiss Household Panel
## see ?actcal
data(actcal)

## selecting individuals aged 20 to 59
actcal <- actcal[actcal$age00>=20 & actcal$age00 <60,]

## defining a sequence object
actcal.lab <- c("> 37 hours", "19-36 hours", "1-18 hours", "no work")
actcal.seq <- seqdef(actcal,13:24,labels=actcal.lab)

## building a PST segmented by age group
gage10 <- cut(actcal$age00, c(20,30,40,50,60), right=FALSE,
	labels=c("20-29","30-39", "40-49", "50-59"))

actcal.pstg <- pstree(actcal.seq, nmin=2, ymin=0.001, group=gage10)

## pruning
C99 <- qchisq(0.99,4-1)/2
actcal.pstg.opt <- prune(actcal.pstg, gain="G2", C=C99)

## extracting PST for age group 20-39 and 30-39
g1.pst <- subtree(actcal.pstg.opt, group=1)
g2.pst <- subtree(actcal.pstg.opt, group=2)

## plotting the two PST
par(mfrow=c(1,2))
plot(g1.pst, withlegend=FALSE, max.level=4, main="20-29")
plot(g2.pst, withlegend=FALSE, max.level=4, main="30-39")

}
\references{
Gabadinho, A. & Ritschard, G. (2016). Analyzing State Sequences with Probabilistic Suffix Trees: The PST R Package. \emph{Journal of Statistical Software}, \bold{72}(3), pp. 1-39.
}
\author{
Alexis Gabadinho
}
\keyword{methods}
\keyword{models}
