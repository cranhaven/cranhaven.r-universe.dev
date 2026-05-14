\name{chicks}
\alias{chicks}
\docType{data}
\title{Chicks' weight data}
\description{
This real data is reported by Chang and Yen (2011). Eighteen chicks were randomly assigned to three treatments with six chicks in each for balanced data.
}
\usage{data("chicks")}
\format{
  A data frame with 18 observations on the following 2 variables.
  \describe{
    \item{\code{diet}}{Diets of different protein content}
    \item{\code{weight}}{Chicks' weight (in grams)}
  }
}
\details{
Treatment 1 (Diet 1) had the diet with the lowest level of protein; treatment 2 (Diet 2) had the diet with a medium level of protein; and treatment 3 (Diet 3) had the diet with the highest level of protein. Does the average weight of chicks increase with the amount of protein? Hypothesis is being investigated.
}
\source{
Chang, C. H. and Yen, C.H. (2011). A Nonparametric Test for the Ordered Alternative Based on Fast Discrete Fourier Transform Coefficient. \emph{Journal of Testing and Evaluation}, \bold{39:6}, 1131-1143.
}

\examples{

library(npordtests)

data(chicks)
levels(chicks$diet) <- c("diet 1", "diet 2", "diet 3")
boxplot(weight~diet,data=chicks,xlab="Diet")
}
\keyword{datasets}
