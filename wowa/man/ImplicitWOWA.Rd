\name{wowa.ImplicitWOWA}
\alias{wowa.ImplicitWOWA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Impicit Weighted OWA Computation Function
}
\description{Function for Calculating implicit Weighted OWA function
}
\usage{
  wowa.ImplicitWOWA(x, p, w, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The vector of inputs}
  \item{p}{The weights of inputs x}
  \item{w}{The OWA weightings vector}
  \item{n}{Dimension of the vector  x}
  
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
  \item{output}{The value of the Impicit Weighted OWA }
}
\references{
  %% ~put references to the literature/web site here ~
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
  %%  ~~who you are~~
    Gleb Beliakov, Daniela L. Calderon, Deakin University
}

\examples{
    n <- 4
    example <- wowa.ImplicitWOWA(c(0.3,0.4,0.8,0.2), c(0.3,0.25,0.3,0.15), 
                     c(0.4,0.35,0.2,0.05), n)
    example	
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Implicit WOWA}
\keyword{WOWA}% __ONLY ONE__ keyword per line