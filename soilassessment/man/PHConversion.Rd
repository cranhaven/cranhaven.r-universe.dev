\name{PHConversion}
\alias{PHConversion}
\title{
Models for converting soil pH (KCl or CaCl) to the equivalent pH (water)
}
\description{
A suit of functions for converting soil pH (KCl or CaCl2) to the equivalent pH (water)
}
\usage{
PHConversion(ph, model,phtype)
}
\arguments{
  \item{ph}{
a vector or single value of soil ph in KCl or CaCl2 to be converted to ph (water)
}
  \item{model}{
functional model for relating ph in KCl or CaCl to be converting and equivalent ph (water). Models considered are second order kabala, sadovski, davies, brennan functions. The default is kabala
}
  \item{phtype}{
KCl or CaCl2 solution for ph. The default is CaCl2
}
}
\details{
ph conversion models are those in the literature
}
\value{
numeric value of equivalent ph (water)
}
\references{
Davies, B.E. (1971). A Statistical Comparison of pH Values of some English Soils after Measurement in both Water and 0.01M Calcium Chloride. Soil Science Society of America Journal 35, 551–552. https://doi.org/10.2136/sssaj1971.03615995003500040022x

Kabała, C., Musztyfaga, E., Gałka, B., Łabuńska, D., Mańczyńska, P. (2016). Conversion of Soil pH 1:2.5 KCl and 1:2.5 H2O to 1:5 H2O: Conclusions for Soil Management, Environmental Monitoring, and International Soil Databases. Pol. J. Environ. Stud. 25, 647–653. https://doi.org/10.15244/pjoes/61549

Miller, R.O., Kissel, D.E. (2010). Comparison of Soil pH Methods on Soils of North America. Soil Sci. Soc. Am. J. 74, 310–316. https://doi.org/10.2136/sssaj2008.0047

Sadovski, A.N. (2019). Study on pH in water and potassium chloride for Bulgarian soils. EURASIAN JOURNAL OF SOIL SCIENCE (EJSS) 8, 11–16. https://doi.org/10.18393/ejss.477560
}
\author{
Christian Thine Omuto
}
\note{
ph ranges between 1 and 14
}
\seealso{
ME_PHharm, ME_ECharm, ECconversion1
}
\examples{
testdata=data.frame(PHKC=c(6.45,8.34,5.07,12.17, 4.219),TEX=c("Cl","SaCl","LoSa", "Si","SaClLo"))
testdata$PHs1=PHConversion(testdata$PHKC,"kabala","kcl")
}
