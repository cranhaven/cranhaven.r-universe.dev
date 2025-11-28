\name{write.gmt}
\alias{write.gmt}
\title{
Write a GMT file
}
\description{
This function write a file containing the Metabolite Set informtation in the format GMT defined by GenePattern.
}
\usage{
write.gmt(sub,address,min_entry=2,max_entry=50)
}
\arguments{
  \item{sub}{A matrix.}
  \item{address}{The address of the file should be saved.}
  \item{min_entry}{The minimum number of metabolites for each metabolite set.}
  \item{max_entry}{The maximum number of metabolites for each metabolite set.}
}

\value{
No return value. If an invalid address is inserted, the function will generate an error.
}
\seealso{
  \code{\link{write.gct}}, \code{\link{write.cls}}
}


