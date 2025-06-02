\name{COSha30}
\alias{COSha30}
\docType{data}
\title{
Soil organic carbon database at a sampling depth of 0-30 cm
}
\description{
Soil organic carbon database of samples taken in several soil and land cover types at La Libertad Research Center at a sampling depth of 0-30 cm
}
\usage{data(COSha30)}
\format{
  A data frame with 118 observations on the following 10 variables:
  \describe{
    \item{\code{ID}}{ID of each sampling site}
    \item{\code{x}}{\emph{x}-coordinate of each site. Spatial reference system: UTM 18N}
    \item{\code{y}}{\emph{y}-coordinate of each site. Spatial reference system: UTM 18N}
    \item{\code{DA30}}{measured soil bulk density (g cm\eqn{^{-3}})}
    \item{\code{CO30}}{measured soil carbon concentration (\%)}
    \item{\code{COB1r}}{land cover at each sampling site in 2007. See details below}
    \item{\code{S_UDS}}{soil type at each sampling site. See details below}
    \item{\code{COSha30}}{calculated total soil carbon stock (t ha\eqn{^{-1}}). See details below}
    \item{\code{Cor4DAidep}}{total soil carbon stock (t ha\eqn{^{-1}}) corrected by soil compaction factors}
    \item{\code{CorT}}{corrected total soil carbon stock with Box-Cox transformation applied}
  }
}
\details{
A total of 150 samples for a 0-30 cm depth was collected and analyzed for soil bulk density and organic carbon concentration in 2007 at La Libertad Research Center in Villavicencio, Colombia. The samples were taken in soils under different land cover types: rice crops (\code{Az}), citrus crops (\code{Ci}), forest plantations (\code{Cpf}), annual crops (\code{Ctv}), grasses (\code{P}), and oil palm crops (\code{Pl}).
In the soil type names, the first two letters correspond to the short name of the soil series, the lower-case letters indicate the slope class, and the number denotes the type of soil drainage. 

Total soil carbon stock \eqn{COSha} was calculated as follows (Guo & Gifford, 2002):
\deqn{
COSha=DA*CO*d
}
where \eqn{DA} is soil bulk density (g cm\eqn{^{-3}}), \eqn{CO} is soil organic carbon concentration (\%) and \eqn{d} is sampling depth (cm).

Given that the data did not fit a normal distribution, a Box-Cox transformation was applied (Box & Cox, 1964). Some samples were discarded for the design of sampling networks. The complete database and description can be found in Santacruz (2010) and in Santacruz et al., (2014). 
}
\source{
Santacruz, A. 2010. \emph{Design of optimal spatial sampling networks for the monitoring of soil organic carbon at La Libertad Research Center through the application of genetic algorithms}. M.Sc. Thesis. National University of Colombia, Bogota. 162 p. (In Spanish)
}
\references{
Santacruz, A., Rubiano, Y., Melo, C., 2014. \emph{Evolutionary optimization of spatial sampling networks designed for the monitoring of soil carbon}. In: Hartemink, A., McSweeney, K. (Eds.). \emph{Soil Carbon. Series: Progress in Soil Science}. (pp. 77-84). Springer. \href{https://link.springer.com/chapter/10.1007/978-3-319-04084-4_8}{[link]}

Santacruz, A., 2011. \emph{Evolutionary optimization of spatial sampling networks. An application of genetic algorithms and geostatistics for the monitoring of soil organic carbon}. Editorial Academica Espanola. 183 p. ISBN: 978-3-8454-9815-7 (In Spanish)

Guo, L., Gifford, R., 2002. \emph{Soil carbon stocks and land use change: a meta analysis}. Global Change Biology 8, 345-360.

Box, G., Cox, D., 1964. \emph{An analysis of transformations}. Journal of the Royal Statistical Society. Series B (Methodological) 26 (2), 211-252.
}
\seealso{
\code{\link{COSha30map}}
}
\examples{
data(COSha30)
str(COSha30)
}
\keyword{datasets}
