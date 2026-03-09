\name{wowa.WAn}
\alias{wowa.WAn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extension of binary averaging
}
\description{Function for calculating a binary tree multivariate extension of a binary averaging function
}
\usage{
  wowa.WAn(x, w, n, Fn, L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Vector of inputs}
  \item{w}{The weightings vector}
  \item{n}{Dimension of the array x  (and w)}
  \item{Fn}{Bivariate symmetric mean that is extended to n arguments}
  \item{L}{The number of levels of the binary tree (see docs)}
}

\value{
  \item{output}{The output is  Weighted n-variate mean extending Fn }
}
\references{
[1]G. Beliakov, H. Bustince, and T. Calvo. A Practical Guide to Averaging Functions.
	Springer, Berlin, Heidelberg, 2016.

[2]G. Beliakov. A method of introducing weights into OWA operators and other
	symmetric functions. In V. Kreinovich, editor, Uncertainty Modeling. Dedicated
	to B. Kovalerchuk, pages 37-52. Springer, Cham, 2017.

[3]G. Beliakov. Comparing apples and oranges: The weighted OWA function, Int.J. Intelligent Systems, 33, 1089-1108, 2018.

[4]V. Torra. The weighted OWA operator. Int. J. Intelligent Systems, 12:153-166, 1997.

[5]G. Beliakov and J.J. Dujmovic , Extension of bivariate means to weighted means of several arguments by using binary trees, 
Information sciences, 331,  137-147,  2016.

[6]	J.J. Dujmovic and G. Beliakov. Idempotent weighted aggregation based on binary
	aggregation trees. Int. J. Intelligent Systems  32,  31-50,  2017.
}
\author{
   Gleb Beliakov, Daniela L. Calderon, Deakin University
}

\examples{
      Fn <- function( x, y) { # just a simple arithmetic mean, 
	# but can be more complex functions (eg heronian, Logaritmic means)
		out <- (x+y)/2	
		return(out)
       }

   n <- 4
   example <- wowa.WAn(c(0.3,0.4,0.8,0.2),  c(0.4,0.3,0.2,0.1), n, Fn, 10)
   example
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ WAn}
\keyword{WOWA}% __ONLY ONE__ keyword per line