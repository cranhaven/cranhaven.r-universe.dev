\name{xprop}
\alias{xprop}
\title{
  Graph \code{xprop}
}
\description{
  This visualization is  a type of 100\% stacked histograme. The graph \code{xprop} shows the distribution of the relationship between entities in the corpus. The total of the bar represents 100\%.  
}
\usage{
  xprop(v1,v2,type=1)
}
\arguments{
  \item{v1}{ a vector of values}
  \item{v2}{ a vector of values}
  \item{type}{ type of graph}
}
\details{
  After calling the function \code{xparse}, the result has the following format:
  \enumerate{
              \item file_name:entity:$:list_value_found
              \item ...
              \item file_name:entity1:entity2:...:$$:value_e1:value_e2:....:negation
            }
  Function \code{xprop} will convert the data format above to a data frame such as:
  \enumerate{
              \item a list of columns that call the values of v2. Those columns will contain a value 0 or 1.
              \item a column has a name "cat" - categorie.
              \item a column has a name "val" - value.
\cr Each line discribes the relevant information between values of vector \code{v1} and values of vector \code{v2}. If there exists a relationship between a value of \code{v1} with a value of \code{v2} then the column of value v2 will be 1, the column "cat" carrying value is the value of \code{v2} and the column "val" has the value current of v1.   
            }
}
\author{
  Tien T. Phan
}
\seealso{
  \code{\link{xhist}} type graphique histogram\cr
  \code{\link{xplot}} type graphique plot\cr
}
\examples{
  xprop(v1=c("chou","colza"),v2=c("mouche du chou","rouille"))
  v1 = as.vector(xdata_value("p")[["value"]])
  v2 = as.vector(xdata_value("b")[["value"]])
  xprop(v1,v2,type=2)
}
\keyword{graphe}
\keyword{graphique proprotion}