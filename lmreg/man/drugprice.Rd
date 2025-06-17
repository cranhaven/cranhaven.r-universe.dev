\name{drugprice}
\alias{drugprice}
\docType{data}
\title{
Price of drugs under generic and brand names
}
\description{
Across-countries median of median price ratio (MPR) of some medicines available in the private market under the generic name and the brand name of the originator (Gelders et al., 2005).
}
\usage{data(drugprice)}
\format{
  A data frame with 13 observations on the following 2 variables.
  \describe{
    \item{\code{Drug}}{Generic name of drug, a character vector}
    \item{\code{Quantity}}{Unit for price computation, a character vector}
    \item{\code{OriginatorMPR}}{Originator median price ratio, a numeric vector}
    \item{\code{GenericMPR}}{Generic median price ratio, a numeric vector}
  }
}

\details{
The data comes from a World Health Organization (WHO) commissioned study on variation of drug prices over a number of developing countries. For comparability, the price in a particular region is expressed as a ratio (called median price ratio or MPR) with respect to the organization's drug price indicator median values. The data reflect the across-country median of these ratios in respect of 13 medicines, most of which are in the WHO list of essential medicines.
}

\source{
Gelders, S., Ewen, M., Noguchi, N. and Laing R. (2005). Price, Availability and Affordability: An International Comparison of Chronic Disease Medicines, Background report prepared for the WHO Planning Meeting on the Global Initiative for Treatment of Chronic Diseases, Cairo, December 2005.
}

\examples{
data(drugprice)
head(drugprice)
}
\keyword{datasets}
