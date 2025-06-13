\name{growth}
\encoding{ascii}
\alias{growth}
\docType{data}
\title{
Example Data for pampe function from the pampe package
}
\description{
Dataset for the example from the pampe function of the pampe package. The growth dataset, obtained from the supplemental materials of Hsiao, Steve Ching and Ki Wan (2012) contains information on the quarterly real GDP growth rate of 24 countries (the donor pool) and Hong Kong from 1993 Q1 to 2008 Q1, computed as the change with respect to the same quarter in the previous year. The data is organized in standard cross-sectional data format, with the variables (the quarterly real GDP growth rate of the countries in the donor pool act as explanatory variables) extending across the columns and the quarters (time-periods) extending across rows. In this case the treated unit, Hong Kong, is in the first column whereas the possible controls (countries in the original donor pool) are in columns 2:25. It is important for the user to have his or her data in this standard format to correctly apply the methodology. Naming the rows and especially the columns is also strongly recommended. Be careful to avoid characters such as spacing when naming.
}
\usage{data(growth)}
\format{
  A data frame with 61 observations on 25 variables (countries/units).
}

\source{
Hsiao C, Steve Ching H, Ki Wan S (2012). "A Panel Data Approach for Program Evaluation: Measuring the Benefits of Political and Economic Integration of Hong Kong with Mainland China". Journal of Applied Econometrics, 27(5), 705-740. ISSN 1099-1255. doi:10.1002/ jae.1230. URL http://dx.doi.org/10.1002/jae.1230.
}
\seealso{
\code{\link{pampe}}
}
\keyword{datasets}
