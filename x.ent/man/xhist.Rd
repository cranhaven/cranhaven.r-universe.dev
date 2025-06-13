\name{xhist}
\alias{xhist}
\title{
  Graph \code{xhist}
}
\description{
The function \code{xhist} in x.ent is a graphical representation of the distribution of entities with time.
}
\usage{
  xhist(v = "")
}
\arguments{
  \item{v}{a value of entity or the relations between entities }
}
\details{
Result after calling the function \code{xparse} has the following format:
\enumerate{
            \item file_name:entity:$:list_value_found
            \item ...
            \item file_name:entity1:entity2:...:$$:value_e1:value_e2:....:negation
          }
Function \code{xhist} will convert the data format above to a data frame. The histogram uses this data frame to display graphs.
The data frame format: 
\enumerate{
            \item column file : name of file
            \item column date : (format mm.yyyy)
            \item column value_date, this value is used for creating histogram
            \item column visible: if visible = 1 then this record will be used in histogram   
          }
}
\value{
This function returns a data frame so that users can check or use it to create new graphs.
\item{dataframe}{return a data frame}
}
\examples{
  xhist() #all documents
  xhist(v="colza") #only documents contain "colza"
  xhist(v="colza:altise") #only documents contain a relation "colza:altise"
}
\seealso{
  \code{\link{xplot}} type graphique plot\cr
  \code{\link{xshow}} display the results of extracted data\cr
  \code{\link{xshow}} display results
}
\keyword{histogram}
\keyword{graph}
