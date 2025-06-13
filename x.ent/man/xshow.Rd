\name{xshow}
\alias{xshow}
\alias{xfile}
\title{Show results}
\description{
Show results after calling the function \code{xparse}.
}
\usage{
  xshow(e=NULL,sort="a")
}
\arguments{
  \item{e}{an entity or a list of entities that you want display, default e = NULL => display all columns}
  \item{sort}{type sort of data, default sort = "a" => sorted by alphabet, sort = "f" => sorted by frequency.}
}
\details{
  Show results after calling function \code{xparse}.
  The result file has format:
  \enumerate{
    \item{entity} {file1:entity1:$:data1:data2:data3:}
    \item{relation} {file1:entity1:entity2:$$:data_e1:data_e2:negation}
  }
}
\examples{
  xfile() #show all names of files in corpus
  xshow() #all columns
  xshow(e="p",sort="a") #show result of entity "p", sorted by alphabet
  xshow(e="p",sort="f")
  xshow(e=c("p","m"))
}
\author{Tien T. Phan}
\seealso{
  \code{\link{xparse}}  call the main function of module extraction written by Perl\cr
  \code{\link{xshow}} display results
}