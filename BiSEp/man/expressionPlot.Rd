\name{expressionPlot}
\alias{expressionPlot}
\title{expressionPlot: Create visualisations from BIGEE output}
\description{
Takes the output from the function BISEP and two gene names that correspond to a relevant gene pair.  Gene names must be available in the input BISEP object.
}
\usage{
expressionPlot(
	bisepData=data,
	gene1,
	gene2
	)
}
\arguments{
  \item{bisepData}{This should be the output from the BISEP function.
}
  \item{gene1}{The first gene whose expression you would like to plot.
}
  \item{gene2}{The second gene whose expression you would like to plot.
}
}
\details{
The function will return an error if any of the input information is incorrect or missing.  The resulting plot will be returned in real time.
}
\value{
A scatter plot of the two genes you have identified as bimodal.  The red lines correspond to the mid-points of the bimodal distribution for these two genes.  Ideally the lower left quadrant would be empty when observing a candidate SL interaction.
}

\author{
Mark Wappett
}

\examples{
data(BISEP_data)
data(MUT_data)
expressionOut <- expressionPlot(BISEP_data, gene1="SMARCA1", gene2="SMARCA4")
}

\keyword{ Expression }
\keyword{ SyntheticLethal }
