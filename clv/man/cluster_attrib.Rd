\name{cls.attrib}
\title{Mean, cluster size and center - cluster utilities}
\description{
	Mean, center of each cluster, number of objects in each cluster - informations retrieved from partitioned data
	using \code{cls.attrib}.
}
\usage{
cls.attrib(data, clust)
}
\alias{cls.attrib}
\arguments{
 \item{data}{
		\code{numeric matrix} or \code{data.frame} where columns correspond to variables and rows to 
		observations.
	}
  \item{clust}{
		integer \code{vector} with information about cluster id the object is assigned to.
		If vector is not integer type, it will be coerced with warning.
	}
}
\value{
	As a result function returns object of \code{list} type which contains three objects with information about:\cr
		\code{mean} - \code{numeric vector} which represents mean of given \code{data}, \cr
		\code{cluster.center} - \code{numeric matrix} where columns correspond to variables and rows to observations,\cr
		\code{cluster.size} - \code{integer vector} with information about size of each cluster.
}

\seealso{
		Result of function is mostly used to compute following indicies:
		\code{\link{clv.Dis}}, \code{\link{wcls.matrix}}, \code{\link{bcls.matrix}}.
	}
\examples{
# create "data" matrix
mx <- matrix(0,4,2)
mx[2,1] = mx[3,2] = mx[4,1] = mx[4,2] = 1
# give information about cluster assignment
clust = as.integer(c(1,1,2,2))
cls.attrib(mx,clust)
}
\author{Lukasz Nieweglowski}
\keyword{cluster}