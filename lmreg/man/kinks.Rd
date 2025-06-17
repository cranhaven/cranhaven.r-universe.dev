\name{kinks}
\alias{kinks}
\docType{data}
\title{
Kink bands in rocks
}
\description{
Measurements of an angular dimension (beta angle) found in kink bands of Daling phyllite in the Darjeeling-Sikkim Himalayas.
}
\usage{data(kinks)}
\format{
  A data frame with 100 observations on the following 3 variables.
  \describe{
    \item{\code{beta}}{Beta angle in kink bands (in degrees)}
    \item{\code{order}}{Fold order (1 = main fold, 2 = sub-fold, 3,4 = sub-folds of successively higher order)}
    \item{\code{type}}{Type of kink band (1 = conjugate, 2 = dextral, 3 = sinistral)}
  }
}

\source{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach, World Scientific Publishing Co., Table 6.8.
}

\examples{
data(kinks)
head(kinks) 
}
\keyword{datasets}
