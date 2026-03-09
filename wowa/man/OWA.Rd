\name{wowa.OWA}
\alias{wowa.OWA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Ordered weigted average function 
}
\description{Function for computing the ordered weigted averages
}
\usage{
  wowa.OWA(n, x, w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{Dimension of the vector x}
  \item{x}{The vector of inputs}
  \item{w}{The OWA weights }
  }
  
  \value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    \item{output}{The value of the ordered weighted average.}
  }
  \references{
    %% ~put references to the literature/web site here ~
[1]G. Beliakov, H. Bustince, and T. Calvo. A Practical Guide to Averaging Functions.
	Springer, Berlin, Heidelberg, 2016.   
  }
  \author{
    %%  ~~who you are~~
     Gleb Beliakov, Daniela L. Calderon, Deakin University
  }
  
  \examples{
     n <- 4
     wowa.OWA(n, c(0.3,0.4,0.8,0.2), c(0.4,0.35,0.2,0.05))
  }
  % Add one or more standard keywords, see file 'KEYWORDS' in the
  % R documentation directory.
  \keyword{ OWA}
  \keyword{OWA}% __ONLY ONE__ keyword per line