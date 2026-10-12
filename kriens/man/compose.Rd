\name{compose}
\alias{compose}
\alias{\%.\%}
\title{
Continuation Passing Style Function Composition
}
\description{
It allows to compose two functions of the form \code{f(x, ret)} and \code{g(x, ret)} returning a function \code{h(x,ret)} which is the composition \eqn{f \circ g}{f . g}.
It implements the composition operator of the Continuation category.

The the composition has the following properties:
\enumerate{
 \item{Associativity:}{ \eqn{h \circ (f \circ g) = ( h \circ g) \circ f}{h . (f . g) = ( h . g) . f}}
 \item{Unity:}{ \eqn{f \circ identity2 = f = identity2 \circ f}{f . identity2 = f = identity2 . f}}
}

In order for these relations to hold, the function \code{f} and \code{g} must not deal with global mutable states.

}
\usage{
compose(f, g)
}
\arguments{
  \item{f}{
The first function that must be composed
}
  \item{g}{
The first function that must be composed
}
}
\value{
Rerturns the composite function of \code{f} and \code{g}
}

\author{
Matteo Provenzano\cr
\url{http://www.alephdue.com}
}
\note{
The composition is performed from left to right i.e. such that the first function executed is \code{f}.
}
\seealso{
\code{\link{forget}}
}
\examples{
# Example 1

# define an arrow in the Continuation category.
# this function applies the continuation to the
# increment of its argument and then decrements it.
one <- function(x, ret) {
    return(ret(x+1) - 1)
}

# define another arrow in the Continuation category.
# this function doubles its argument.
two <- function(x, ret) {
    return(ret(2*x))
}

# create the composition
# this is exactly the same as one %.% two
composite <- compose(one, two)

# build the function (forget the continuation)
execute1 <- forget(composite)
execute1(1)
# returns 3

# Example 2
# compose the function further to loop over an array of elements
# lapply and sapply are already arrow in the Continuation category
loop <- compose(lapply, composite)

# build the function
execute2 <- forget(loop)
execute2(1:10)
}
