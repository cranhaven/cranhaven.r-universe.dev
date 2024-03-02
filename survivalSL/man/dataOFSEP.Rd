\name{dataOFSEP}
\alias{dataOFSEP}
\docType{data}
\title{
A Simulated Sample from the OFSEP Cohort.
}
\description{
A data frame with 1300 simulated French patients with multiple sclerosis from the OFSEP cohort. The baseline is 1 year after the initiation of the first-line treatment.}
\usage{data(dataOFSEP)}
\format{
  A data frame with 1300 observations for the 3 following variables:
  \describe{
    \item{\code{time}}{This numeric vector represents the follow up time in years (until disease progression or censoring)}
    \item{\code{event}}{This numeric vector represents the disease progression indicator at the follow-up end (1=progression, 0=censoring)}
    \item{\code{age}}{This numeric vector represents the patient age (in years) at baseline.}
    \item{\code{duration}}{This numeric vector represents the disease duration (in days) at baseline.}  
    \item{\code{period}}{This numeric vector represents the calendar period: 1 in-between 2014 and 2018, and 0 otherwise.}  
    \item{\code{gender}}{This numeric vector represents the gender: 1 for women.}  
    \item{\code{relapse}}{This numeric vector represents the diagnosis of at least one relapse since the treatment initiation : 1 if at leat one event, and 0 otherwise.}
    \item{\code{edss}}{This vector of character string represents the EDSS level: "miss" for missing, "low" for EDSS between 0 to 2, and "high" otherwise.}
    \item{\code{t1}}{This vector of character string represents the new gadolinium-enhancing T1 lesion: "missing", "0" or "1+" for at least 1 lesion.}
    \item{\code{t2}}{This vector of character string represents the new T2 lesions: "no" or "yes".}
    \item{\code{rio}}{This numeric vector represents the modified Rio score.}   
  }
}

\examples{
data(dataOFSEP)

### Kaplan and Meier estimation of the disease progression free survival
plot(survfit(Surv(time, event) ~ 1, data = dataOFSEP),
     ylab="Disease progression free survival",
     xlab="Time after the first anniversary of the first-line treatment in years")
}

\keyword{Dataset}

