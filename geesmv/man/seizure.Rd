\name{seizure}
\alias{seizure}
\title{
Epiliptic seizure counts from the Randomized Progabide Trial
}
\description{
The data consists of 59 subjects from the randomized progabide trial.
}
\usage{
seizure
}
%- maybe also 'usage' for other objects documented here.
\format{
 \itemize{
 \item{base:}{baseline epiliptic seizure counts}
  \item{trt:}{the indicator for treatment (1=progabide; 0=control)}
  \item{age:}{baseline age for each subject}
  \item{y1:}{epiliptic seizure counts in the first week}
  \item{y2:}{epiliptic seizure counts in the second week}
  \item{y3:}{epiliptic seizure counts in the third week}
  \item{y4:}{epiliptic seizure counts in the forth week}
  }
 }
  
\details{
The outcome is the count number of epiliptic seizures in each of four consecutive two-week intervals, and the variables recorded include baseline epiliptic seizure counts in an eight-week interval prior to the treatment assignment, the indicator for treatment (1=progabide; 0=control), and baseline age.
}
\source{
Thall PF, and Vail SC. Some covariance models for longitudinal count data with overdispersion. Biometrics 1990; 46: 657-671.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{seizure}
\keyword{GEE}% __ONLY ONE__ keyword per line
