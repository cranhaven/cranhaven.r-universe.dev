\name{top_thresh}
\alias{top_thresh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Thresholding to a given threshold of the smallest values
}
\description{
This function keeps only the K largest values of the vector and sets the others to the smallest value among the K largest.
}
\usage{
top_thresh(vect,thresh)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vect}{
vector to threshold
}
  \item{thresh}{
threshold
}
}

\value{
This function returns the thresholded vector.
}
\author{
Wencan Zhu, Celine Levy-Leduc, Nils Ternes
}

\examples{
x=sample(1:10,10)
thresh=3
top_thresh(x,thresh)

## The function is currently defined as
function (vect, thresh) 
{
    sorted_vect <- sort(abs(vect),decreasing=TRUE)
    v = sorted_vect[thresh]
    ifelse(abs(vect) >= v, vect, v)
  }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~~thresholding }% use one of  RShowDoc("KEYWORDS")
