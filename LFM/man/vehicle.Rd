\name{vehicle}
\alias{vehicle}
\docType{data}
\title{In Vehicle Coupon Recommendation Data}
\description{
This dataset contains information about coupon recommendations made to drivers in a vehicle, including various contextual features and the outcome of whether the coupon was accepted.
}
\usage{
vehicle
}
\format{
A data frame with multiple rows and 27 columns representing different features related to coupon recommendations.
\itemize{
\item \code{destination}: Driver's destination - No Urgent Place, Home, Work.
\item \code{passanger}: Passengers in the car - Alone, Friend(s), Kid(s), Partner.
\item \code{weather}: Current weather - Sunny, Rainy, Snowy.
\item \code{temperature}: Temperature in Fahrenheit - 55, 80, 30.
\item \code{time}: Time of day - 2PM, 10AM, 6PM, 7AM, 10PM.
\item \code{coupon}: Type of coupon - Restaurant(<$20), Coffee House, Carry out & Take away, Bar, Restaurant($20-$50).
\item \code{expiration}: Coupon expiration - 1d (1 day), 2h (2 hours).
\item \code{gender}: Driver's gender - Female, Male.
\item \code{age}: Driver's age group - 21, 46, 26, 31, 41, 50plus, 36, below21.
\item \code{maritalStatus}: Driver's marital status - Unmarried partner, Single, Married partner, Divorced, Widowed.
\item \code{has_Children}: Whether the driver has children - 1, 0.
\item \code{education}: Driver's education level - Some college - no degree, Bachelors degree, Associates degree, High School Graduate, Graduate degree (Masters or Doctorate), Some High School.
\item \code{occupation}: Driver's occupation - Various categories including Unemployed, Student, etc.
\item \code{income}: Driver's income range - Various ranges such as $37500 - $49999, $62500 - $74999, etc.
\item \code{Bar}: Frequency of bar visits per month - never, less1, 1~3, gt8, nan4~8.
\item \code{CoffeeHouse}: Frequency of coffeehouse visits per month - never, less1, 4~8, 1~3, gt8, nan.
\item \code{CarryAway}: Frequency of getting take-away food per month - n4~8, 1~3, gt8, less1, never.
\item \code{RestaurantLessThan20}: Frequency of visiting restaurants with average expense <$20 per month - 4~8, 1~3, less1, gt8, never.
\item \code{Restaurant20To50}: Frequency of visiting restaurants with average expense $20-$50 per month - 1~3, less1, never, gt8, 4~8, nan.
\item \code{toCoupon_GEQ15min}: Driving distance to the coupon location greater than 15 minutes - 0, 1.
\item \code{toCoupon_GEQ25min}: Driving distance to the coupon location greater than 25 minutes - 0, 1.
\item \code{direction_same}: Whether the coupon location is in the same direction as the current destination - 0, 1.
\item \code{direction_opp}: Whether the coupon location is in the opposite direction of the current destination - 1, 0.
\item \code{Y}: Whether the coupon was accepted - 1, 0.
}
}
\examples{
# Load the dataset
data(vehicle)

# Print the first few rows of the dataset
print(head(vehicle))

}
\keyword{datasets}
