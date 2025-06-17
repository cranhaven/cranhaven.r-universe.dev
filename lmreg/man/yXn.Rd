\name{yXn}
\alias{yXn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Prepare design matrix for nested model with groups and subgroups
}
\description{
Prepares design matrix for nested model with groups and subgroups and response vector in corresponding order.
}
\usage{
yXn(response, group, subgroup)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{response}{
Response vector as provided (numeric).
}
  \item{group}{
Vector of group labels as provided (either numeric or character).
}
  \item{subgroup}{
Vector of subgroup labels as provided (either numeric or character).
}
}
\value{
Returns a list with following components.
  \item{X}{
A binary matrix with number of rows equal to length of response and number of columns equal to the total number of levels of treatments and blocks plus one. Each row has exactly three 1s: in the first position and in the two positions representing the group and the subgroup.
}
  \item{y}{
Numeric vector of response values, permuted to correspond with the rows of X.
}
}
\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}

\examples{
data(kinks)
yXn(kinks$beta,kinks$type,kinks$order)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ array }
\keyword{ manip }
\keyword{ category }
