\name{DGobj.rawdata}
\alias{DGobj.rawdata}
\title{
Construction of a DG object from raw data
}
\description{
Construction of a \link{DG object} from raw demographic and genetic data.
}
\usage{
DGobj.rawdata(demographic.coord, demographic.measures, genetic.coord, 
 genetic.frequencies)
}
\arguments{
  \item{demographic.coord}{
[2-column matrix] Coordinates of sites where demographic measurements were made.
}
  \item{demographic.measures}{
[2-column matrix] Demographic measurements (e.g. pathogen intensity). The first column contains measurements at the first sampling time. The second column contains measurements at the second sampling time.
}
  \item{genetic.coord}{
[2-column matrix] Coordinates of sites where genetic samples were collected.
}
  \item{genetic.frequencies}{ 
 [Matrix] with frequencies of genetic samples from all sampled strains. Each column corresponds to a given strain.
}
}

\value{ 
	An object from the \link{DG class}.
}

\note{
	Demographic measurements, say \eqn{Y_i(t_1)} and \eqn{Y_i(t_2)}, made at sampling sites \eqn{i\in\{1,\ldots,I\}} and at the first and second sampling times, respectively, are transformed into the values 
\eqn{ Z_i=\log\left(\frac{1+Y_i(t_2)}{1+Y_i(t_1)}\right)} characterizing the temporal growth of the epidemic in space. The growth variable \eqn{Z_i} is given in the thrid column of the demographic slot of the returned DG object.
}

\references{
Soubeyrand S., Tollenaere C., Haon-Lasportes E. & Laine A.-L. (2014). Regression-based ranking of pathogen strains with respect to their contributions to natural epidemics. PLOS ONE 9(1): e86591.
}
\author{
Soubeyrand, S., Tollenaere, C., Haon-Lasportes, E. and Laine, A.-L. 
}
\seealso{
	\link{DGobj-class}, \link{DGobj.simul.mechanistic}, \link{DGobj.simul.regression}
}
\examples{
## load the powdery mildew data set
data(powderymildew)

## create a DG object from this data set
DGdata=DGobj.rawdata(demographic.coord=powderymildew$demographic.coord,
 genetic.coord=powderymildew$genetic.coord,
 demographic.measures=powderymildew$demographic.measures,
 genetic.frequencies=powderymildew$genetic.frequencies)

summary(DGdata)
}
\keyword{ datagen }
