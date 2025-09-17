\name{Coef}
\alias{Coef}
\title{Extracts Regression Coefficients}
\description{
   extracts model coefficients from object of class "ilse".
}
\usage{
  Coef(object)
}
\arguments{
  \item{object}{an object of class "ilse".}
}

\value{
 Coefficients extracted from object.
}
\seealso{
coef, coefficient
}
\examples{
# example one
data(nhanes)
NAlm2 <- ilse(age~., data=nhanes)
Coef(NAlm2)
}
