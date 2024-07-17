\name{daPe}
\alias{daPe}
\docType{data}
\title{Program Effectiveness of a New Method of Teaching Economics}
\description{This data set contains the evaluation results of a new program of teaching in economics. There are 32 observations for 4 variables.
\tabular{ll}{
\code{grade } \tab a binary variable indicating grade increase (1) and decrease (0) after participation. \cr
\code{gpa   } \tab a continous variable measuring studens' grade point average. \cr
\code{tuce  } \tab a continous variable measuring students' scores on an economics test. \cr
\code{psi   } \tab a binary variable indicating whether a student participates the program or not. \cr
}
}
\usage{data(daPe)}

\format{A data frame of cross sectional data with 32 observations and 4 variables.}

\details{Evaluation results on 32 students of the impact of a new teaching methods.}

\source{Spector, L.C., and M. Mazzeo. 1980. Probit analysis and economic education. Journal of Economic Education 11(2):37-44.}

\examples{
data(daPe)
dim(daPe)
summary(daPe)
daPe
}
\keyword{datasets}