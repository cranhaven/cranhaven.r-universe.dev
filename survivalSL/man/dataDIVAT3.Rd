\name{dataDIVAT3}
\alias{dataDIVAT3}
\docType{data}
\title{
A Sample from the DIVAT Data Bank.
}
\description{
A data frame with 4267 French kidney transplant recipients.
}
\usage{data(dataDIVAT3)}
\format{
  A data frame with 4267 observations for the 8 following variables.
  \describe{
    \item{\code{ageR}}{This numeric vector represents the age of the recipient (in years)}
    \item{\code{sexeR}}{This numeric vector represents the gender of the recipient (1=men, 0=female)}
    \item{\code{year.tx}}{This numeric vector represents the year of the transplantation}
    \item{\code{ante.diab}}{This numeric vector represents the diabetes statute (1=yes, 0=no)}
    \item{\code{pra}}{This numeric vector represents the pre-graft immunization using the panel reactive antibody (1=detectable, 0=undetectable)}
    \item{\code{ageD}}{This numeric vector represents the age of the donor (in years)}
    \item{\code{death.time}}{This numeric vector represents the follow up time in days (until death or censoring)}
    \item{\code{death}}{This numeric vector represents the death indicator at the follow-up end (1=death, 0=alive)}
  }
}

\source{URL: www.divat.fr}

\references{
Le Borgne et al. Standardized and weighted time-dependent ROC curves to evaluate the intrinsic prognostic capacities of a marker by taking into account confounding factors. Manuscript submitted. Stat Methods Med Res. 27(11):3397-3410, 2018. <doi: 10.1177/ 0962280217702416.>
}

\examples{
data(dataDIVAT3)

### a short summary of the recipient age at the transplantation
summary(dataDIVAT3$ageR)

### Kaplan and Meier estimation of the recipient survival
plot(survfit(Surv(death.time/365.25, death) ~ 1, data = dataDIVAT3),
 xlab="Post transplantation time (in years)", ylab="Patient survival",
 mark.time=FALSE)
}

\keyword{Dataset}

