\name{GrinnellHouses}
\alias{GrinnellHouses}
\docType{data}
\title{House Sales in Grinnell, Iowa}
\description{
Data on houses sold between 2005 and 2015 in Grinnell, Iowa
}
\format{
  A data frame with 929 observations on the following 15 variables.
  \describe{
    \item{\code{Date}}{Coded value for date of sale (Jan 1, 2005=16436)}
    \item{\code{Address}}{Street address of the house}
    \item{\code{Bedrooms}}{Number of bedrooms}
    \item{\code{Baths}}{Number of bathrooms}
    \item{\code{SquareFeet}}{The square footage of the home's living space}
    \item{\code{LotSize}}{Lot size (in acres)}
    \item{\code{YearBuilt}}{Year the house was built; many pre-1900 homes are listed as 1900}
    \item{\code{YearSold}}{The year the house was sold, for this case}
    \item{\code{MonthSold}}{The month the house was sold (1=Jan, 2=Feb, to 12=Dec)}
    \item{\code{DaySold}}{Day of the month the house was sold (1 to 31)}
    \item{\code{CostPerSqFt}}{SalePrice / SquareFeet (round to nearest penny)}
    \item{\code{OrigPrice}}{List price of the house when originally put on the market (dollars)}
    \item{\code{ListPrice}}{List price at the time of sale (dollars)}
    \item{\code{SalePrice}}{Sale price of the house (dollars)}
    \item{\code{SPLPPct}}{(Sale_Price / List_Price ) * 100}
  }
}
\details{
A local Grinnell realtor, Matt Karjalahti, put these data together to see what patterns might be found, perhaps with an improvement in how one sells houses or buys them.  He asked Grinnell College economists, Lee Logan and Eric Ohrn, to help with the analysis and we obtained the data from them.
}
\source{
Thanks to Grinnell realtor Matt Karjalahti who originally collected the data and Grinnell College economists Lee Logan and Eric Ohrn who gave us the data.


}
\keyword{datasets}
