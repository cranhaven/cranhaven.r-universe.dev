\name{nhanes}
\alias{nhanes}
\title{NHANES example - all variables numerical}
\description{
   A small data set with missing values.
}
\format{
  A data frame with 25 observations on the following 4 variables.
age: {Age group (1=20-39, 2=40-59, 3=60+).}

{bmi: }{Body mass index (kg/m**2).}

{hyp: }{Hypertensive (1=no,2=yes).}

{chl: }{Total serum cholesterol (mg/dL).}
}
\details{
  A small data set with all numerical variables. The data set nhanes2 is the same data set, but with age and hyp treated as factors.
}

\source{
 Schafer, J.L. (1997). Analysis of Incomplete Multivariate Data. London: Chapman & Hall. Table 6.14.
}
\examples{
# example one
data(nhanes)
bw <- 1
ilse(age~., data=nhanes,bw=bw)
}
