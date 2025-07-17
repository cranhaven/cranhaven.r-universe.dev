\name{example.data}
\alias{example.data}
\docType{data}
\title{
Toy two-class gene expression dataset.
}
\description{
A dataset with 200 genes and 2 classes of data, each with 100 observations.  The two classes' data matrices are stored in a list.  
}
\usage{data(example.data)}
\format{
  The format is:
List of 2
 $ : num [1:100, 1:200] 0.395 -2.03 -1.704 -0.469 1.75 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : NULL
  .. ..$ : chr [1:200] "gene 1" "gene 2" "gene 3" "gene 4" ...
 $ : num [1:100, 1:200] -1.548 1.45 -0.812 -0.589 0.69 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : NULL
  .. ..$ : chr [1:200] "gene 1" "gene 2" "gene 3" "gene 4" ...
}

\examples{
data(example.data)
str(example.data) 
}
\keyword{datasets}
