\name{readMet}
\alias{readMet}
\title{
Metabolite Cards Reading
}
\description{
This function extract metabocards of your metabolites dataset from http://www.hmdb.ca/metabolites/ database and store all of this information in a list.
}
\usage{
readMet(ID, address =  c("http://www.hmdb.ca/metabolites/"),remove=TRUE) 
}

\arguments{
  \item{ID}{
A vector containg the HMDBcodes (i.e., metabolite IDs) of metabolites dataset. 
}
  \item{address}{
Optional address where the MetaboCards are located. The default address is http://www.hmdb.ca/metabolites/.
}
  \item{remove}{
A logic value. If true, missing and wrong HMDB IDs are removed.
}
}

\value{
  A list containing all the information related to the metabocards.

}

\seealso{
  \code{\link{nameMet}}
}

\examples{
\donttest{

ID=c("HMDB0000122","HMDB0000124","HMDB0000243","HMDB0000263")
doc=readMet(ID) 

}
}

