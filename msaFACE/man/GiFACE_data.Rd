\name{GiFACE}
\alias{GiFACE}
\docType{data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Longterm time series of a Free Air Carbon Enrichment experiment (FACE)
}
\description{
The dataset contains aggregated data from one of the globally longest time series (1998-2013) from a Free Air Carbon Enrichment experiment, settled in Giessen, Germany (GiFACE).
}
\usage{
data(GiFACE)
}
\details{
  Within the experiment, a permanent temperate grassland is treated with elevated CO2 (~20\% during daylight hours). In three rings the air is enriched with CO2 (-> elevated) while other three rings act under atmospheric CO2 (-> ambient). The late-summer yields as well as the CO2 concentrations (averaged in the three months before harvest) were measured ring-wise and can be used as dependent and independent variables within the Moving Subset Analysis. Experiment-support variables describing the environmental conditions were aggregated to their average in the three months preceding harvest (respectively sum for solar radiation and precipitation). These environmental variables can be used (1) as forcing variables to derive the subsets (where regression within the moving subset is performed) and (2) be plotted as accompanying variables to get a clear picture of the environmental conditions within the respective subset. 
}
\references{
  W. A. Obermeier, L. W. Lehnert, C. I. Kammann, C. Mueller, L.
  Gruenhage, J. Luterbacher, M. Erbs, G. Moser, R. Seibert, N. Yuan,
  J. Bendix (under review) Reduced CO2 fertilization in temperate C3 grasslands
  under more extreme weather conditions. Nature Climate Change
}
\examples{
\dontrun{
## Workaround to import the data from the DOI (10.5678\LCRS\DAT.265)

## Load data from http://dx.doi.org/10.5678/LCRS/DAT.265 and extract csv-file
## Define variable 'filename' so that it points to the csv-file
filename <- ""

## Import data
import <- read.csv(filename,
                   stringsAsFactors = FALSE)

## Delete every second column (quality flag of database)
data <- import[,seq(1, ncol(import), 2)]

## Create a vector containing the years of data acquistion 
year_vec <- substr(data[,1],1,4)

## Delete further unnecessary columns
data <- data[,-c(1,2,13)]

## Perform Moving Subset Analysis for all forcing experiment support variables (default) with
## independent variable CO2 Concentration (CO2), dependent variable biomass (BY_T), 30 observations
## within each subset and the years as grouping factor
MSA_GiFACE <- moving_subset_analysis(data, "CO2", "BY_T", 30, group =  year_vec)

## Show summary
summary(MSA_GiFACE)
}
}
\format{\code{list}}
\author{
Wolfgang A. Obermeier, Lukas W. Lehnert, Joerg Bendix
}

\keyword{datasets, GiFACE}

