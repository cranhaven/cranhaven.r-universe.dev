\name{summary.luca}
\alias{summary.luca}
\title{Summarize results of the luca function}
\description{
Summary function for reporting the results of the luca function in a similar  
style to the lm and glm summaries.
}
\usage{\method{summary}{luca}(object, \ldots)}
\arguments{
\item{object}{a list of class \code{luca} output by the \code{\link{luca}} function}
\item{\ldots}{additional arguments to the summary function (currently unused)}
}
\value{
\item{call}{function call}
\item{coefficients}{Table of estimated coefficients, standard errors and Wald tests for each variable}
}
\examples{
data(lucaDat)
pen.model <- formula(d ~ I(allele.count(g, "C")) + 
    a + a2 + I(allele.count(g, "C")):a2)
fitDep <- luca(pen.model = pen.model, gLabel = "g", 
    dat = lucaDat, dep.model = formula(g ~ a))
# Summarize the results:
summary.luca(fitDep) # or just summary(fitDep)
#Returns:
#Call:
#luca(dat = lucaDat, pen.model = pen.model, gLabel = "g", dep.model = 
#formula(g ~ a))
#
#Coefficients:
#                              Estimate Std. Error     zscore     Pr(>|z|)
#I(allele.count(g, "C"))     0.61738385 0.10820323  5.7057800 1.158115e-08
#a                           0.11629696 0.07815014  1.4881222 1.367187e-01
#a2                         -0.03087368 0.10787965 -0.2861863 7.747354e-01
#I(allele.count(g, "C")):a2  0.31879401 0.08236130  3.8706772 1.085334e-04

}
\references{Shin J-H, McNeney B, Graham J (2007).
Case-Control Inference of Interaction between Genetic and Nongenetic
Risk Factors under Assumptions on Their Distribution.
Statistical Applications in Genetics and Molecular Biology 6(1),
Article 13. Available at: http://www.bepress.com/sagmb/vol6/iss1/art13.
}

\seealso{
\code{\link{luca}}
}
\keyword{methods}
