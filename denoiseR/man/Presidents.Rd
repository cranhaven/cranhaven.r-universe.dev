\name{Presidents}
\alias{Presidents}
\docType{data}
\title{
Contingency table with US Presidents speeches. 
}
\description{
A data set on US presidents inaugural speeches.
}
\usage{data(Presidents)
}

\format{
A data frame with 13 rows and  836 columns. Rows represents the  US presidents
(from 1940 to 2009) and columns words used during their inaugural addresses. This is a contingency table.
}

\source{
http://www.presidency.ucsb.edu and http://www.usa-presidents.info/union/
DtmVic software (Lebart 2015) http://www.dtmvic.com/
}

\examples{
\dontrun{
data(Presidents)
isa.ca <- ISA(Presidents, delta = 0.1, transformation = "CA")
rownames(isa.ca$mu.hat) <- rownames(Presidents)
colnames(isa.ca$mu.hat) <- colnames(Presidents)
res.isa.ca <- CA(as.data.frame(isa.ca$mu.hat), graph = FALSE)
plot(res.isa.ca, title = "Regularized CA", cex = 0.8, selectRow = "contrib 40")
plot(res.isa.ca, title = "Regularized CA", cex = 0.6, invisible = "row" )
}
}
\keyword{datasets}



