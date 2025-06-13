\name{add_unique}
\alias{add_unique}
\title{
  Add a value to a current list that every value is unique
}
\description{
 Add a value to an existing list of values. These values are unique in the list. 
}
\usage{
  add_unique(list, value)
}
\arguments{
  \item{list}{: a list of values }
  \item{value}{: a value that we want to add to the list }
}
\value{
  \item{list}{return a list that elements in the list aren't duplicated}
}
\examples{
  list1= c("a","b","c")
  value = "a"
  list1 <- add_unique(list1,value)
}
\keyword{add unique}
