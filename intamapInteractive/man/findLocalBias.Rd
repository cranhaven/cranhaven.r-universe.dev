\name{findLocalBias}
\alias{findLocalBias}
\alias{removeLocalBias}
\title{ Finds (and removes) biases between overlapping networks}
\description{
The function tries to identify differences between different networks
of observation stations that share a region. From these differences, 
biases are estimated, and can be removed.
}
\usage{
findLocalBias(object, gid = "group",
              formulaString = value ~ 1, regCode="regCode",...)
removeLocalBias(object, localBias, gid = "group", formulaString = value ~ 1, 
                regCode = "regCode")
}
\arguments{
\item{object}{ data frame with observations}
\item{gid}{name of column identifying groups of local networks}
\item{formulaString}{formula that defines the dependent variable as a linear model 
      of independent variables; suppose the dependent variable has name \code{z}, 
      for ordinary and simple kriging use the formula \code{z~1}; 
      for universal kriging, suppose \code{z} is linearly dependent on 
      \code{x} and \code{y}, use the formula \code{z~x+y}}
\item{regCode}{the column name of regions in the \code{object}, if existing}
\item{localBias}{ List of data frames, for a single region, or for 
                  each of the regions, each containing
                  biases for different networks in the region(s), result of 
                  \code{findLocalBias}}
\item{...}{arguments to be passed to sub-functions}
} 



\details{
\code{findLocalBias} tries to identify biases between overlapping networks, i.e. when
there is no boundary between different networks sampling the same type of data.
This can typically happen if different governmental bodies are responsible for
different types of measurement, e.g. one measuring the situation around populated
areas, the other one measuring close to water bodies. 

The function will then try to find the difference between the different networks, 
and estimate the individual bias for each network, relative to a reference value,
usually the average of all networks. The method is not recommended if there
can be assumed to be a dependency beteween the process and the networks.

\code{removeLocalBias} removes the bias estimated in \code{findLocalBias}.
}

\value{ 
From \code{findLocalBias}: A list consisting of one element for each regional 
network, or an element \code{single}
if only one regional network is apparent. Each of these elements is again a list 
consisting of several other elements, where \code{bias} is the interesting one.
The remaining elements are only necessary for debugging purposes. The elements
D, V and Q refers to the matrices with same names in Skoien et al. (2009), i.e.
the relationship matrix, the variance matrix and the difference matrix.

From \code{removeLocalBias}: A \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}}
with the biases subtracted.

}

\references{ 

Skoien, J. O., O. P. Baume, E. J. Pebesma, and G. B. M. Heuvelink. 2010. 
Identifying and removing heterogeneities between monitoring networks. 
Environmetrics 21(1), 66-84.


}
\author{ Jon Olav Skoien }
\examples{

# Assuming that the soil type is the source of biases
data(meuse)
coordinates(meuse) = ~x+y


lb = findLocalBias(meuse,gid = "soil",formulaString=as.formula(zinc~1))
lb$single$bias

meuseUnbias = removeLocalBias(meuse,localBias = lb, gid = "soil",
    formulaString = zinc~1)
}
\keyword{spatial}
