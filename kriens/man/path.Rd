\name{path}
\alias{path}
\title{
Compose all the function in a list
}
\description{
It applies the \code{compose} opertor recursively on all the elements of the list provided as argument
}
\usage{
path(fs)
}
\arguments{
  \item{fs}{
The list of the functions that must be composed together (e.g: list(f1, f2, f3, ...)).
}
}
\value{
A function of the type \code{g(x, ret)} result of the pairwise composition of each element in the list.
}
\author{
Matteo Provenzano\cr
\url{http://www.alephdue.com}
}

\examples{
# define a function that doubles its argument
times.two <- function(x, ret) {
  ret(x*2)
}

# define a function that loops over a list of list and double every element
loop <- forget(path(list(lapply, lapply, times.two)))

#returns list(list(2, 4, 6), list(8,10,12))
loop(list(list(1,2,3),list(4,5,6)))
}
