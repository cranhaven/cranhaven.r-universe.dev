\name{meta}
\alias{meta}
\docType{data}
\title{ Results of 22 Clinical Trials of beta-Blockers    }
\description{
  Results of 22 clinical trials of beta-blockers for reducing mortality
  after myocardial infection. Used for meta-analysis.
}
\usage{data(meta)}
\format{
  A data frame with 22 observations on the following 5 variables.
  \describe{
    \item{\code{study}}{id code of study}
    \item{\code{control.deaths}}{number of deaths in control group}
    \item{\code{control.total}}{total number of patients in control group}
    \item{\code{treated.deaths}}{number of deaths in treatment group}
    \item{\code{treated.total}}{total number of patients in treatment group}
  }
}
\details{
   The 22 clinical trials each consist of two groups of heart attack patients
   randomly allocated to receive or not receive  beta-blockers ( a family of 
   drugs
   that affect the central nervous system and can relax the 
   heart musckles).    
}
\examples{
data(meta)
names(meta)
# Calculating empirical log-odds and its sampling variances:
y <- apply(meta, 1, function(x) log( (x[4]/(x[5]-x[4]))/(x[2]/(x[3]-x[2])) ) )
s2 <- apply(meta, 1, function(x) 1/(x[5]-x[4]) + 1/x[4] +1/(x[3]-x[2]) + 1/x[2] )  
cbind("Study number"=meta[,1], "empirical log odds"=y, "empirical sampling variance of y"=s2)
#if(require(meta)){
#   funnel(y, sqrt(s2))
#   radial(y, sqrt(s2))
#}

}
\keyword{datasets}
\concept{meta analysis}

