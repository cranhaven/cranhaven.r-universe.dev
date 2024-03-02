\name{dataDIVAT2}
\alias{dataDIVAT2}
\docType{data}
\title{
A Sample from the DIVAT Data Bank.
}
\description{
A data frame with 1912 French kidney transplant recipients from the DIVAT cohort.
}
\usage{data(dataDIVAT2)}
\format{
A data frame with the 4 following variables:
  \describe{
    \item{\code{age}}{This numeric vector provides the age of the recipient at the transplantation (in years).}
	\item{\code{hla}}{This numeric vector provides the indicator of transplantations with at least 4 HLA incompatibilities between the donor and the recipient (1 for high level and 0 otherwise).}
    \item{\code{retransplant}}{This numeric vector provides the indicator of re-transplantation (1 for more than one transplantation and 0 for first kidney transplantation).}
    \item{\code{ecd}}{The Expended Criteria Donor (1 for transplantations from ECD and 0 otherwise). ECD are defined by widely accepted criteria, which includes donors older than 60 years of age or 50-59 years of age with two of the following characteristics: history of hypertension, cerebrovascular accident as the cause of death or terminal serum creatinine higher than 1.5 mg/dL.}
    \item{\code{times}}{This numeric vector is the follow up times of each patient.}
    \item{\code{failures}}{This numeric vector is the event indicator (0=right censored, 1=event). An event is considered when return in dialysis or patient death with functioning graft is observed.}
  }
}

\references{
Le Borgne F, Giraudeau B, Querard AH, Giral M and Foucher Y. Comparisons of the performances of different statistical tests for time-to-event analysis with confounding factors: practical illustrations in kidney transplantation. Statistics in medicine. 30;35(7):1103-16, 2016. <doi:10.1002/ sim.6777>
}

\source{URL: www.divat.fr}

\examples{

data(dataDIVAT2)

# Compute the non-adjusted Hazard Ratio related to the ECD versus SCD
cox.ecd<-coxph(Surv(times, failures) ~ ecd, data=dataDIVAT2)
summary(cox.ecd) # Hazard Ratio = 1.97
}

\keyword{Dataset}
