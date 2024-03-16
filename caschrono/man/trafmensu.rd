\name{trafmensu}
\alias{trafmensu}
\docType{data}
\title{Monthly Air traffic at Toulouse Blagnac Airport for the period 1993-2007}
\description{
The file "/import/trafquoti.txt" contains daily Air traffic at Toulouse Blagnac Airport for the period 1993-2007
}
\format{The series is imported first as a data.frame with function \code{read.table},
aggregated by month and then transformed into a \code{ts} object. Is is then converted in 1000 of people.}
\source{Chambre de Commerce et d Industrie de Toulouse (CCIT)}
\examples{
data(trafmensu)
# The executed code is : 
## Not run: 
bb=read.table(file= system.file("/import/trafquoti.txt",package="caschrono"),
header=FALSE,quote="",sep="", colClasses=c('numeric','character'),
col.names =c('trafic','date'))
mois.an= as.numeric(paste(substr(bb$date,1,4), substr(bb$date,6,7), sep=""))
trafmens=aggregate(bb$traf, list(Mois.An = mois.an), sum)
trafmensu=ts(trafmens$x/1000,start= c(1993,1),frequency= 12)
## End(Not run)
}
\keyword{datasets}
