\name{thai_cohort}
\docType{data}
\alias{thai_cohort}
\title{Cohort study in north east Thailand}
\description{
In a cohort study in northeast Thailand the health status of 602 preschool children
was checked every 2 weeks from June 1982 until September 1985 (Schelp et al.
1990). In this time period it was recorded how often the children showed symptoms
of fever, cough, running nose, or these symptoms together. The frequencies of these
illness spells are given in the data set.
}
\usage{data("thai_cohort")}
\examples{
data("thai_cohort")
mix <- mixalg(obs="counts", weights="frequency", family="poisson", data=thai_cohort,
              numiter=18000, acc=0.00001, startk=25)
}
\references{
Schelp, F., P. Vivatanasept, P. Sitaputra, S. Sornmani, P. Pongpaew, N. Vudhivai, S. Egormaiphol, and D. B{\"o}hning.  \dQuote{Relationship of the morbidity of under-fives to anthropometric measurements and community health intervention.} \emph{Trop Med Parasitol}, 1990, 41(2), 121--126.


Schlattmann, P. (2009) \emph{Medical Applications of Finite Mixture Models}. Berlin: Springer.
} 
\keyword{datasets}
