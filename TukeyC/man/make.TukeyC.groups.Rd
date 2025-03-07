\name{make.TukeyC.groups}
\alias{make.TukeyC.groups}

\title{
  Make Tukey Groups
}

\description{
  Builds groups of means, according to the method of Tukey.
}

\usage{
  make.TukeyC.groups(x)
}

\arguments{
  \item{x}{A square matrix where the lines and colums are all levels of the
    factor being test sorted in decresing order. The matrix content are
    \code{TRUE} or \code{FALSE}, i.e, indicating whether or not equality
    between the levels of factor.}
}

\details{
  This function is an algorithm used to build up the groups of means, 
  allowing for overlapping.
}

\value{
  The object returned by this function is a character matrix and the
  rownames are the levels of the factor being tested sorted in decreasing
  order. The matrix has the same number of columns as the number of groups
  generated.
}

\note{
  This function is mainly for internal use in the TukeyC package.
}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})}

\keyword{package}
