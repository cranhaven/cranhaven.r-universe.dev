\name{review}
\alias{review}
\docType{data}
\title{Review}
\description{
This dataset contains travel reviews from TripAdvisor.com, covering destinations in 11 categories across East Asia. Each traveler's rating is mapped to a scale from Terrible (0) to Excellent (4), and the average rating for each category per user is provided.
}
\usage{
review
}
\format{
A data frame with multiple rows and 12 columns.
\itemize{
\item \code{User_ID}: Unique identifier for each user (Categorical).
\item \code{Art_Galleries}: Average user feedback on art galleries.
\item \code{Dance_Clubs}: Average user feedback on dance clubs.
\item \code{Juice_Bars}: Average user feedback on juice bars.
\item \code{Restaurants}: Average user feedback on restaurants.
\item \code{Museums}: Average user feedback on museums.
\item \code{Resorts}: Average user feedback on resorts.
\item \code{Parks_Picnic_Spots}: Average user feedback on parks and picnic spots.
\item \code{Beaches}: Average user feedback on beaches.
\item \code{Theaters}: Average user feedback on theaters.
\item \code{Religious_Institutions}: Average user feedback on religious institutions.
}
}
\examples{
# Load the dataset
data(review)

# Print the first few rows of the dataset
print(head(review))
}
\keyword{datasets}
