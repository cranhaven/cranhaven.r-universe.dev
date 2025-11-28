\name{mpg}
\alias{mpg}
\docType{data}
\title{Hocking(1976) Miles Per Gallon data: a Multiple Regression Benchmark}
\description{Performance data for the 1973-74 models of 32 autos from the Road Tests
 Section of \emph{Motor Trends} magazine. NOTE: data(mtcars) loads essentially the
 same data, but most variable names are then different and have fewer characters.}
\usage{data(mpg)}
\format{
  A data frame of 11 variables on 32 autos; no NAs.
  \describe{
    \item{cylnds}{number of cylinders.}
    \item{cubins}{cubic inches of engine displacement.}
    \item{hpower}{engine horsepower.}
    \item{weight}{total weight in pounds.}
    \item{mpg}{miles per gallon.}
    \item{shape}{engine shape (0 = V, 1 = Straight).}
    \item{transm}{transmission type (0 = Automatic, 1 = Manual).}
    \item{speeds}{number of forward speeds.}
    \item{carbs}{number of carburetors.}
    \item{fratio}{final drive rtaio.}
    \item{qmilt}{quarter mile time.}
  }
}
\references{
Hocking RA. The Analysis and Selection of Variables in Regression.
Biometrics 1976; 32: 1-51.

Henderson HV, Velleman PF. Building multiple regression models interactively.
Biometrics 1981; 37: 391-411.
}
\keyword{datasets}
