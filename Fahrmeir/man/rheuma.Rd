\name{rheuma}
\alias{rheuma}
\docType{data}%
%# Example 3.9 page 91.  
\concept{compound cumulative model}
\concept{two-step cumulative model}
\title{ Data from Patients with Acute Rheumatoid Arthritis  }
\description{
   Data from patients with acute rheumathoid arthritis. A new agent 
   was compared with an active control, and each patient was evaluated 
   on a five-point assessment scale. 
}
\usage{data(rheuma)}
\format{
  A data frame with 10 observations on the following 3 variables.
  \describe{
    \item{Drug}{a factor with levels \code{Active.control} \code{New.agent}}
    \item{Improvement}{an ordered factor with levels \code{Much.worse} < 
                         \code{Worse} < \code{No.change} < \code{Improved} < 
                         \code{Much.improved}}
    \item{n}{number of patients in group}
  }
}
\details{
  The global assessment in this example may be subdivided in the coarse 
  response "improvement", "no change" and "worse". On a higher level 
  improvement is split into "much improved" and "improved", 
   while the "worse" category is split into "worse" and "much worse".
}
\source{
   Ludwig Fahrmeir, Gerhard Tutz (1994):  Multivariate Statistical Modelling
   Based on Generalized Linear Models. Springer Series in Statistics. 
   Springer Verlag. New-York  Berlin Heidelberg
}
\examples{
str(rheuma)
summary(rheuma) 
}
\keyword{datasets}
