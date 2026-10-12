\name{ResidualOil}
\alias{ResidualOil}
\docType{data}
\title{US Residual Oil Production (Quarterly 1983-2016)}
\description{
Quarterly production of residual oil in the U.S. from 1983 to 2016
}
\format{
  A data frame with 136 observations on the following 7 variables.
  \describe{
    \item{\code{Year}}{Year (1983 to 2016)}
    \item{\code{Qtr}}{Month (\code{1}=Jan-Mar, \code{2}=Apr-June, \code{3}=July-Sep, \code{4}=Oct-Dec)}
    \item{\code{t}}{Time index (1 to 136)}
    \item{\code{Oil}}{Residual fuel oil distribution (in million gallons/day)}
    \item{\code{LogOil}}{Natural logarithm of Oil}
  }
}
\details{
The U.S. Energy Information Administration tracks the production and distribution of various types of petroleum products. The category for this dataset is called residual oil, which are heavier oils (often called No. 5. and No. 6) that remain after lighter oils (such as No. 4 home heating oil) are distilled away in the refining process. It is used in steam-powered ships, power plants, and other industrial applications.
}
\source{
U.S. Energy Information Administration website - Refiner sales volumes for residual fuel oil and No. 4 heating oil at https://www.eia.gov/petroleum/data.php#consumption.  Specific webpage is \cr https://www.eia.gov/dnav/pet/pet_cons_refres_d_nus_VTR_mgalpd_m.htm.
}
\keyword{datasets}
