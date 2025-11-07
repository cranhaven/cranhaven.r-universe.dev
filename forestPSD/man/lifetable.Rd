\name{lifetable}
\alias{lifetable}
\title{
Generte the static life table to analyze the population dynamic changes.
}
\description{
Static life tables were used to analyze the dynamic changes.
}
\usage{
lifetable(ax)
}
\arguments{
  \item{ax}{
Population number of within different age class.
}
}
\details{
Generte the static life table to analyze the population dynamic changes.
}
\value{
Result returns the results of a static life table, which includes the following parameters,
ax: existing individual number within age class x;
lx: standardized survival number at the beginning of age class x (generally converted to 1000);
lnlx: logarithmicstandardized survival number;
dx: standardized death number within the interval from age class x to x + 1; qx: mortality rate;
Lx: average survival number within the interval from ageclass x to x + 1;
Tx: total survival number from age class x and beyond;
ex: life expectancy of individuals entering age class x;
Sx: survival rate;
Kx: disappearance rate of the population.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\references{
Zhang Y, Wang J, Wang X, Wang L, Wang Y, Wei J, et al. 2024. Population structures and dynamics of Rhododendron communities with different stages of succession in northwest Guizhou, China. Plants-Basel 13.
}
\examples{
data(Npop)
lifetable(ax=Npop$ax)
}
