\name{rand_tensor}
\alias{rand_tensor}
\title{Tensor with Random Entries}

\description{
  Generate a Tensor with specified modes with iid normal(0,1) entries.
}
\usage{
  rand_tensor(modes = c(3, 4, 5), drop = FALSE)
}
\arguments{
  \item{modes}{: the modes of the output Tensor}
  \item{drop}{: whether or not modes equal to 1 should be dropped}
}
\value{
  a Tensor object with modes given by modes

}
\author{
  Kyle Caudle

  Randy Hoover

  Jackson Cates
}
\references{
Imported from rTensor package version 1.4.8.
}
\examples{
  rand_tensor()
  rand_tensor(c(4,4,4))
  rand_tensor(c(10,2,1),TRUE)
}
