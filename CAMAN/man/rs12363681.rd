\name{rs12363681}
\docType{data}
\alias{rs12363681}
\title{Gene calling}
\description{
This data set can be used to perform a cluster analyis of bivariate data.  \cr

This data set contains SNP of 3680 individuala 
}
\usage{data("rs12363681")}
\format{A data frame consisting of 3680 data sets (rows) and 2 attributes (columns)}
\examples{
\dontrun{
# Example
# EM and classification for bivariate data
data(rs12363681)
test <- bivariate.mixalg(obs1=x, obs2=y, type="bi", 
                         lambda1=0, lambda2=0, p=0, 
                         data=rs12363681, startk=20, class="TRUE")
# scatter plot with ellipse
plot(test)
# scatter plot without ellipse
plot(test, ellipse = FALSE)
}
}
\references{
Schlattmann, P.(2009) \emph{Medical Applications of Finite Mixture Models.} Berlin: Springer.
}
\keyword{datasets}
