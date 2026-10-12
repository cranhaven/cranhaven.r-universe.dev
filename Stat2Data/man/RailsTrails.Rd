\name{RailsTrails}
\alias{RailsTrails}
\docType{data}
\title{Homes in Northampton MA Near Rail Trails}
\description{
Sample of homes in Northampton, MA to see whether being close to a bike trail enhances the value of the home
}
\format{
  A data frame with 104 observations on the following 30 variables.
  \describe{
    \item{\code{HouseNum}}{Unique house number}
    \item{\code{Acre}}{Lot size for the house (in acres)}
    \item{\code{AcreGroup}}{Lot size groups (\code{<= 1/4 acre} or \code{> 1/4 acre})}
    \item{\code{Adj1998}}{Estimated 1998 price (in thousands of 2014 dollars)}
    \item{\code{Adj2007}}{Estimated 2007 price (in thousands of 2014 dollars)}
    \item{\code{Adj2011}}{Estimated 2011 price (in thousands of 2014 dollars)}
    \item{\code{BedGroup}}{Bedroom groups (\code{1-2 beds}, \code{3 beds}, or \code{4+ beds})}
    \item{\code{Bedrooms}}{Number of bedrooms }
    \item{\code{BikeScore}}{Bike friendliness (0-100 score, higher scores are better)}
    \item{\code{Diff2014}}{Difference in price between 2014 estimate and adjusted 1998 estimate (in thousands of dollars)}
    \item{\code{Distance}}{Distance (in feet) to the nearest entry point to the rail trail network}
    \item{\code{DistGroup}}{Distance groups, compared to 1/2 mile (\code{Closer} or \code{Farther Away})}
    \item{\code{GarageSpaces}}{Number of garage spaces (0-4)}
    \item{\code{GarageGroup}}{Any garage spaces? (\code{no} or \code{yes})}
    \item{\code{Latitude}}{Latitude (for mapping)}
    \item{\code{Longitude}}{Longitude (for mapping)}
    \item{\code{NumFullBaths}}{Number of full baths (includes shower or bathtub)}
    \item{\code{NumHalfBaths}}{Number of half baths (no shower or bathtub)}
    \item{\code{NumRooms}}{Number of rooms}
    \item{\code{PctChange}}{Percentage change from adjusted 1998 price to 2014 (value of zero means no change)}
    \item{\code{Price1998}}{Zillow 10 year estimate from 2008 (in thousands of dollars)}
    \item{\code{Price2007}}{Zillow price estimate from 2007 (in thousands of dollars)}
    \item{\code{Price2011}}{Zillow price estimate from 2011 (in thousands of dollars)}
    \item{\code{Price2014}}{Zillow price estimate from 2014 (in thousands of dollars)}
    \item{\code{SFGroup}}{SquareFeet group (\code{<= 1500 sf} or \code{> 1500 sf})}
    \item{\code{SquareFeet}}{Square footage of interior finished space (in thousands of sf)}
    \item{\code{StreetName}}{Street name}
    \item{\code{StreetNum}}{House number on street}
    \item{\code{WalkScore}}{Walk friendliness (0-100 score, higher scores are better)}
    \item{\code{Zip}}{Location (\code{1060} = Northampton or \code{1062} = Florence)  }
  }
}
\details{
This dataset comprises 104 homes in Northampton, MA that were sold in 2007. The authors measured the shortest distance from each home to a railtrail on streets and pathways with Google maps and recorded the Zillow.com estimate of each home's price in 1998 and 2011. Additional attributes such as square footage, number of bedrooms and number of bathrooms are available from a realty database from 2007. We divide the houses into two groups based on distance to the trail (DistGroup).
}
\source{
From July 2015 JSE Datasets and Stories:  "Rail Trails and Property Values: Is There an Association?", Ella Hartenian, Smith College and Nicholas J. Horton, Amherst College.

http://www.amstat.org/publications/jse/v23n2/horton.pdf
}
\keyword{datasets}
