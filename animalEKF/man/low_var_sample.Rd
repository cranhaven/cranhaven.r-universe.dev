\name{low_var_sample}
\alias{low_var_sample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample particles using low-variance sampling.
}
\description{
Sample particles using low-variance sampling.
}
\usage{
low_var_sample(wts, M=length(wts))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wts}{
Vector of weights.  
}
  \item{M}{
The number of items to sample. When resampling, should be number of particles \code{npart}.
}
}
\details{
Low-variance sampling guarantees items will be sampled in proportion to their weights.  With random sampling with replacement (\code{sample} function), there is some variability in the final proportions of items.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A numeric vector of length \code{M}.
}
\references{
James Edward Baker. Reducing bias and inefficiency in the selection algorithm. Proceedings of the Second International Conference on Genetic Algorithms on Genetic Algorithms and Their Application, 1987.
}
\author{
Samuel Ackerman
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
n <- 20
w <- runif(n)

#can use M != to length(wts)
low_var_sample(wts=w, M=15)

lv <- low_var_sample(wts=w, M=n)

#usual resampling with weights
a <- sample(x=1:n, size=n, prob=w, replace=TRUE)

#the first should be more in proportion to weights
table(lv)/n
table(a)/n
#proportions
w/sum(w)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }% use one of  RShowDoc("KEYWORDS")

