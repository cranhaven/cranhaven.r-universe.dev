\name{schiz}
\alias{schiz}
\docType{data}
\title{ Data on Response Times for Scizophrenics and non-Schizophrenics   }
\description{
   Response times (in milliseconds) for 11 non-schizophrenic and 6 schizophrenic
   individuals.  
}
\usage{data(schiz)}
\format{
  The format is:
 num [1:30, 1:17] 312 272 350 286 268 328 298 356 292 308 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr [1:17] "nonsch1" "nonsch2" "nonsch3" "nonsch4" ...

  A numerical matrix with individuals as columns. 
}
\details{
   Psychological theory from the last half century and before suggests a model in 
   which schizophrenics suffer from an attentional deficit on some trials, as well as a 
   general motor reflex retardation.  
}
\examples{
data(schiz)
str(schiz)
# Making figure 18.1 in the book:
opar <- par(no.readonly=TRUE)
par( mar=c(2.0, 1,1,1))
par(mfrow=c(5,4))
for (i in 1:11) hist( log(schiz[,i]), main="", xlab="", ylab="", xlim=c(5.4, 7.5))
par( mfg=c(4, 1))
for (i in 1:6) hist( log(schiz[,11+i]), main="", xlab="", ylab="", xlim=c(5.4, 7.5))
par(opar)
}
\keyword{datasets}
