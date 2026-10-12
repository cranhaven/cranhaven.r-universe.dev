\name{powderymildew}
\alias{powderymildew}
\docType{data}
\title{
Demographic and genetic real data
}
\description{
Demographic and genetic data collected during an epidemic of powdery mildew of Plantago lanceolata.
}
\usage{data(powderymildew)}
\format{
  The format is: List of 4 components

\code{$demographic.coord}{
 	'data.frame':	216 obs. of  2 variables (coordinates of the 216 sites with demographic data).
 }   

\code{$genetic.coord}{
	'data.frame':	22 obs. of  2 variables (coordinates of the 22 sites with genetic data).
}

\code{$demographic.measures}{
	num [1:216, 1:2] Pathogen demographic measurements at week 32 and week 34 for sites whose coordinates are given in \code{$demographic.coord}.
	}
	
\code{$genetic.frequencies}{
	num [1:22, 1:5] Frequencies of strains 1 to 5 for sites whose coordinates are given in  \code{$genetic.coord}.
	}
	
See the examples section to visualize the data set.
}

\references{
Soubeyrand S., Tollenaere C., Haon-Lasportes E. & Laine A.-L. (2014). Regression-based ranking of pathogen strains with respect to their contributions to natural epidemics. PLOS ONE 9(1): e86591.
}

\seealso{
	\link{DGobj-class}, \link{DGobj.rawdata}
}

\examples{
## load the powderymildew data set
data(powderymildew)

## names of items of powderymildew
names(powderymildew)

## print powderymildew
print(powderymildew)

## alternatives to print one of the items of powderymildew, e.g. the 4th items:
print(powderymildew$genetic.frequencies)
print(powderymildew[[4]])
}

\keyword{datasets}
