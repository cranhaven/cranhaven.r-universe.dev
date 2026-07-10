\name{concrete}
\alias{concrete}
\docType{data}
\title{Concrete Slump Test Data}
\description{
This dataset contains measurements related to the slump test of concrete, including input variables (concrete ingredients) and output variables (slump, flow, and compressive strength).
}
\usage{
concrete
}
\format{
A data frame with 103 rows and 10 columns.
\itemize{
\item \code{Cement}: Amount of cement (kg in one M^3 concrete).
\item \code{Slag}: Amount of slag (kg in one M^3 concrete).
\item \code{Fly_ash}: Amount of fly ash (kg in one M^3 concrete).
\item \code{Water}: Amount of water (kg in one M^3 concrete).
\item \code{SP}: Amount of superplasticizer (kg in one M^3 concrete).
\item \code{Coarse_Aggr}: Amount of coarse aggregate (kg in one M^3 concrete).
\item \code{Fine_Aggr}: Amount of fine aggregate (kg in one M^3 concrete).
\item \code{SLUMP}: Slump of the concrete (cm).
\item \code{FLOW}: Flow of the concrete (cm).
\item \code{Compressive_Strength}: 28-day compressive strength of the concrete (MPa).
}
}
\examples{
# Load the dataset
data(concrete)

# Print the first few rows of the dataset
print(head(concrete))

}
\keyword{datasets}
