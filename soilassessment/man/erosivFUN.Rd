\name{erosivFUN}
\alias{erosivFUN}
\title{
A function to estimate rainfall erosivity from annual rainfall amounts
}
\description{
This function assumes an algebraic relationship between annual rainfall amounts and rainfall erosivity. The relationship has constants that may depend of certain regions.
}
\usage{
erosivFUN(rain,A,B, model)
}
\arguments{
  \item{rain}{
annual rainfall amounts in mm or Fourier index of rainfall
}
  \item{A}{
independent constant of the algebraic relationship between rainfall mounts and erosive energy (Energy=A+-B*rainfall)
}
  \item{B}{
rainfall coefficient of the algebraic relationship between rainfall mounts and erosive energy (Energy=A+-B*rainfall)
}
  \item{model}{
model defining the algebraic relationship between rainfall mounts and erosive energy. The model can be linear, power, logarithmic, Fourier, and exponential
}
}
\value{
rainfall erosivity in MJ mm/ha/hr/yr
}
\references{
Morgan RPC. 2005. Soil erosion and conservation. Blackwell. UK
Benavidez R, Bethana J, Maxwell D, Norton K. 2018. A review of the (Revised) Universal Soil Loss Equation ((R)USLE): with a view to increasing its global applicability and improving soil loss estimates. Hydrol. Earth Syst. Sci., 22, 6059–6086
}
\author{
Christian Thine
}
\seealso{
\code{\link{erodFUN}}, \code{\link{sloplenFUN}}
}
\examples{
erosivFUN(587,151, 0.63, "linear")
}
