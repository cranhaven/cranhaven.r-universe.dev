\name{quefts_soil}

\alias{quefts_soil}


\title{
soil parameters
}

\description{
Example soil parameters.
}

\usage{
quefts_soil()
}


\details{

\tabular{lll}{
	\tab \bold{Input Parameters} \tab \bold{Explanation} \cr
	\tab \code{N_base_supply, P_base_supply, K_base_supply} \tab Potential supply (kg/ha) of N, P and K of the (unfertilized) soil in a growing season of standard length (default is 120 days). See \code{\link{nutSupply2}} to estimate that. \cr
	\tab \code{N_recovery, P_recovery, K_recovery} \tab Fertilizer recovery, that is, the fraction of applied fertilizer that can be taken up by the plant. \cr
	\tab \code{UptakeAdjust} \tab Two-column matrix to compute the fraction uptake from soil supply as function of length of season. The default standard season length is 120 days. The first column is the length of the growing season, the second column is the fraction uptake. Intermediate values are computed by linear interpolation. \cr
}
}


\value{
list with soil parameters
}

\examples{
soiltype <- quefts_soil()
str(soiltype)
}
