\name{dyn}
\alias{dyn}
\title{
Quantification of population dynamics
}
\description{
The analysis method of replacing age structure with diameter class structure.
}
\usage{
dyn(ax)
}
\arguments{
  \item{ax}{
population number of within different age class.
}
}
\details{
Quantitative method was employed to analyze the dynamics of the individual number between adjacent diameter classes for the populations
}
\value{
Result returns the results of population dynamics analysis.
}
\author{
Zongzheng Chai, chaizz@126.com
}
\references{
Zhang Y, Wang J, Wang X, Wang L, Wang Y, Wei J, et al. 2024. Population structures and dynamics of Rhododendron communities with different stages of succession in northwest Guizhou, China. Plants-Basel 13.
}
\examples{
data(Npop)
dyn(ax=Npop$ax)
}
