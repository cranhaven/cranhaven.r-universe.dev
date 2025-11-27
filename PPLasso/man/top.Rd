\name{top}
\alias{top}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Thresholding to 0
}
\description{
This function keeps only the K largest values of the vector and sets the others to 0.
}
\usage{
top(vect, thresh)
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
top(x,thresh)

## The function is currently defined as
function(vect, thresh){
  sorted_vect <- sort(abs(vect),decreasing=TRUE)
  v<-sorted_vect[thresh]
  ifelse(abs(vect)>=v,vect,0)
}
}
