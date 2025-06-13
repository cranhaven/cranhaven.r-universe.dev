\name{xdata}
\alias{xdata}
\alias{xdata_value}
\title{
Transform the results to data frame
}
\description{
This is a function using transformation of results to data frame.
}
\usage{
  xdata(e = NULL)
  xdata_value(v, sort = "a")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e}{a vector of a entity or a list of entities, if it is nul, it shows all entities and relations that it is configured in the configuration file}
  \item{v}{a entity}
  \item{sort}{with the function \code{xdata_value}, variable "sort" allows you to sort values following frequency or alphabetically}
}
\details{
 The data frame contains the columns of the name of entity or relationship and the rows of values of named entity.
}
\value{
 \item{data frame}{return a data frame}
}
\author{
Tien T. Phan
}

\examples{
  xdata() #show all entities
  xdata(c("p","b")) #show two entities: "p", "b"
  xdata_value("p") #show only values of entity "p"
  #there are two columns "value" et "freq" in this data frame
  xdata_value("p")[["value"]] #convert to a verctor
}

\seealso{
  \code{\link{xparse}}  call the main function of module extraction written by Perl\cr
}
\keyword{ xdata }
\keyword{ xdata_value }
