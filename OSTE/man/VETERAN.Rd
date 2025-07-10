\name{VETERAN}
\alias{VETERAN}
\docType{data}
\title{Data on randomized trial of two treatment procedures for lung cancer.
}
\description{The data set consist of a total 137 observations on 8 variables. The variables consist of the type of lung cancer treatment i.e 1 (standard) and 2 (test drug), cell Type, Status, that denotes the status of the patient as 1 (dead) or 0 (alive), survival time in days since the treatment, Diag, the time since diagnosis in months, age in years, the Karnofsky score, therapy that denotes any prior therapy 0 (none), 1 (yes).
}
\usage{data("VETERAN")}
\format{
  A data frame with 137 observations on the following 8 variables.
  \describe{
    \item{\code{trt}}{ a numeric vector denoting type of lung cancer treatment i.e 1 (standard) and 2 (test drug).}
    \item{\code{celltype}}{ a factor with levels \code{squamous}, \code{smallcell}, \code{adeno} and \code{large}.}
    \item{\code{time}}{ a numeric vector denoting survival time in days since the treatment.}
    \item{\code{status}}{ a numeric vector that denotes the status of the patient as 1 (dead) or 0 (alive).}
    \item{\code{karno}}{ a numeric vector denoting the Karnofsky score.}
    \item{\code{diagtime}}{ a numeric vector denoting the time since diagnosis in months.}
    \item{\code{age}}{ age in years.}
    \item{\code{prior}}{ a numeric vector denoting prior therapy; 0 (none), 1 (yes).}
  }
}
\references{
Therneau T (2015). A Package for Survival Analysis in S. version 2.38, <URL:
https://CRAN.R-project.org/package=survival>.

Terry M. Therneau and Patricia M. Grambsch (2000). Modeling Survival Data: Extending the
Cox Model. Springer, New York. ISBN 0-387-98784-3
}
\examples{
#To load the data
data(VETERAN)
# To see the structure
str(VETERAN) 
#etc.
}
\keyword{datasets}