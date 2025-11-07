#' @title Automobile characteristics
#'
#' @description
#' Cars dataset with features including make, model, year, engine, and other
#' properties of the car used to predict its price.
#'
#' @details This package contains a detailed car dataset.
#`
#' @docType data
#' @keywords datasets
#' @name cardata
#' @usage cardata
#'
#' @format A data frame with 11914 rows and 16 variables. The variables are as follows:
#'
#' \describe{
#'   \item{make}{car brand}
#'   \item{model}{model given by its brand}
#'   \item{year}{year of manufacture}
#'   \item{engine_fuel_type}{type of fuel required by its manufacturer}
#'   \item{engine_hp}{engine horse power}
#'   \item{engine_cylinders}{number of cylinders}
#'   \item{transmission_type}{automatic vs. manual}
#'   \item{driven_wheels}{AWD, FWD, AWD}
#'   \item{number_of_doors}{Number of Doors}
#'   \item{market_category}{Luxury, Performance, Hatchback, etc.}
#'   \item{vehicle_size}{Compact, Midsize, Large}
#'   \item{vehicle_style}{Type of Vehicle: Sedan, SUV, Coupe, etc.}
#'   \item{highway_mpg}{highway miles per gallon}
#'   \item{city_mpg}{city miles per gallon}
#'   \item{popularity}{Popularity index}
#'   \item{msrp}{manufacturer's suggested retail price}
#' }
#'
#' @source Taken from Kaggle
#' \href{https://www.kaggle.com/CooperUnion/cardataset}{https://www.kaggle.com/CooperUnion/cardataset}.
#' @examples
#' summary(cardata)
#'
NULL
