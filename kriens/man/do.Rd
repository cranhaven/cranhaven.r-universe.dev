\name{do}
\alias{do}
\title{
Compose and Forget in one go.
}
\description{
\code{do} allows to specify the list of function directly as its arguments.
It return a function which is the composition of every argument with the continuation already forgotten.
}
\usage{
do(...)
}
\arguments{
  \item{\dots}{
The functions that must be composed together.
}
}
\value{
A function of the type \code{g(x)} which can be directly used on the input.
}
\author{
Matteo Provenzano\cr
\url{http://www.alephdue.com}
}
\seealso{
\code{\link{path}}, \code{\link{forget}}
}
\examples{
# define a function that doubles its argument
times.two <- function(x, ret) {
  ret(x*2)
}

# define a function that loops over a list of list and double every element
loop <- do(lapply, lapply, times.two)

#returns list(list(2, 4, 6), list(8,10,12))
loop(list(list(1,2,3),list(4,5,6)))
}
