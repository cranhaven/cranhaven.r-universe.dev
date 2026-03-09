\name{wowa.weightedf}
\alias{wowa.weightedf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Ordered weigted averages fuzzy system 
}
\description{Function for order weigted averages
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  wowa.weightedf(x, p, w, n, Fn, L)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Vector of inputs}
  \item{p}{The weights of inputs x}
  \item{w}{The OWA weightings vector}
  \item{n}{Dimension of the array x}
  \item{Fn}{Base function defined in R}
  \item{L}{the number of levels of the n-ary tree}
  }
  
  \value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    \item{output}{The output is the order of the weigted average.}
  }
  \references{
    %% ~put references to the literature/web site here ~
[1]G. Beliakov, H. Bustince, and T. Calvo. A Practical Guide to Averaging Functions.
	Springer, Berlin, Heidelberg, 2016.
[2]G. Beliakov. A method of introducing weights into OWA operators and other
	symmetric functions. In V. Kreinovich, editor, Uncertainty Modeling. Dedicated
	to B. Kovalerchuk, pages 37â€“52. Springer, Cham, 2017.
[3]G. Beliakov. Comparing apples and oranges: The weighted OWA function, submitted, 2017.
[4]V. Torra. The weighted OWA operator. Int. J. Intelligent Systems, 12:153â 166, 1997.
[5]G. Beliakov. A method of introducing weights into OWA operators and other
	symmetric functions. In V. Kreinovich, editor, Uncertainty Modeling. Dedicated
	to B. Kovalerchuk, pages 37-52. Springer, Cham, 2017.
[6]	J.J. Dujmovic and G. Beliakov. Idempotent weighted aggregation based on binary
	aggregation trees. Int. J. Intelligent Systems, in press, DOI:10.1002/int.21828, 2017.
  }
  \author{
    %%  ~~who you are~~
      Gleb Beliakov, Daniela L. Calderon, Deakin University
  }
  
  \examples{
  
      Fn <- function(n, x, w) {
  	  out <- 0.0
	  for(i in 1:n) out<- out+x[i]*w[i];
          return("infn :", out)
       }
      n <- 4

        example <- wowa.weightedf(c(0.3,0.4,0.8,0.2), c(0.3,0.25,0.3,0.15), c(0.4,0.35,0.2,0.05), n, Fn,  10)
	example
    
  }
  % Add one or more standard keywords, see file 'KEYWORDS' in the
  % R documentation directory.
  \keyword{ weightedf}
  \keyword{weightedf}% __ONLY ONE__ keyword per line