\name{ranova}
\Rdversion{1.1}
\alias{ranova}
\title{
Analysis of Variance on per capita growth rate pseudovalues
}
\description{
Accepts as input a series (list) of individual life tables (see \code{\link{r}},\code{\link{petitr}},\code{\link{life1}}), calculates the Jackknife estimator of r (per capita growth rate) on each table, and achieves a one way analysis of variance on the set of pseudovalues corresponding to each table.
}
\usage{
ranova(listab, levels = NULL)
}
\arguments{
  \item{listab}{
list of life tables. must be of class list, and each table of class data.frame
}
  \item{levels}{
a character vector giving level names for each life table. If NULL, the levels are named l1,l2, etc..
}
}
\value{
a data frame with two columns: the set of pseudovalues, and a factor named pop. Can be retrieved and used for more sophisticated factor organisation
}
\references{
Lotka 1924, Birch 1948, Wratten 1982
}
\author{
Jean-Sebastien Pierre
}

\seealso{
 \code{\link{r}}, and \code{\link{xlxmx}}, called by  \code{\link{petitr}}~
}
\examples{
data(life1,life2,life3)
ranova(list(life1,life2,life3))

}
\keyword{ misc }
