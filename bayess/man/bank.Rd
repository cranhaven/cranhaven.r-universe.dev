\name{bank}
\alias{bank}
\docType{data}
\title{
bank dataset (Chapter 4)%%   ~~ data name/kind ... ~~
}
\description{
The bank dataset we analyze in the first part
of Chapter 3 comes from Flury and Riedwyl (1988) and
is made of four measurements on 100 genuine Swiss banknotes and 100 counterfeit ones.
The response variable \eqn{y}{y} is thus the status of the banknote, where 0 stands 
for genuine and 1 stands for counterfeit, while the explanatory factors are bill
measurements.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(bank)}
\format{
  A data frame with 200 observations on the following 5 variables.
  \describe{
    \item{\code{x1}}{length of the bill (in mm)}
    \item{\code{x2}}{width of the left edge (in mm)}
    \item{\code{x3}}{width of the right edge (in mm)}
    \item{\code{x4}}{bottom margin width (in mm)}
    \item{\code{y}}{response variable}
  }
}
\source{
Flury, B. and Riedwyl, H. (1988) \emph{Multivariate Statistics. A Practical Approach}, Chapman and Hall,
London-New York.
}
\examples{
data(bank)
summary(bank)
}
\keyword{datasets}
