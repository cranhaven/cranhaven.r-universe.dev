\name{VegCOV}
\alias{VegCOV}
\title{
Detrmining Erosion Cover Factor (C Factor) from Vegetation Cover Fraction
}
\description{
To estimate erosion model cover factor from vegetation cover fraction
}
\usage{
VegCOV(pcover, model="cai")
}
\arguments{
  \item{pcover}{
percent vegetation cover
}
 \item{model}{
model for estimating C factor from percent vegetation cover
}
}
\details{
This function can be used to estimate the C factor for erosion modelling by use of percent vegetation cover. The function contains five models: cai, xu, hurni, jin, and liu. When the percent cover ('pcover') is given and the model to use (e.g., 'cai'), the function returns dimensionless C factor for erosion modelling. C factor ranges between 0 and 1 (0< cfactor<=1)
}
\value{
A numerical value between 0 and 1
}
\references{
C.F. Cai, S.W. Ding, Z.H. Shi, L. Huang, G.Y. Zhang. Study of applying USLE and geographical information system IDRISI to predict soil erosion in small watershed. J. Soil Water Conserv., 14 (2) (2000), pp. 19-24

Taye, G., Vanmaercke, M., Poesen, J., Van Wesemael, B., Tesfaye, S., Teka, D., Nyssen, J., Deckers, J., Haregeweyn, N., 2018. Determining RUSLE P‐ and C‐factors for stone bunds and trenches in rangeland and cropland, North Ethiopia. Land Degrad Dev 29, 812–824. https://doi.org/10.1002/ldr.2814
}
\author{
Christian Thine Omuto}
\note{
The function does not accept percent cover values that are less than 0 and more than 100.
}


\seealso{
\code{\link{NDVIcoverfactor}}
}
\examples{
VegCOV(20,"cai")
}
