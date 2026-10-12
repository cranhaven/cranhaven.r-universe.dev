\name{Film}
\alias{Film}
\docType{data}
\title{Film Data from Leonard Maltin's Guide}
\description{
Film data from Maltin's Movie and Video Guide
}
\format{
  A data frame with 100 observations on the following 9 variables.
  \tabular{rl}{
    \code{Title} \tab {Movie title}\cr
    \code{Year} \tab {Year the movie was released}\cr
    \code{Time} \tab {Running time (in minutes)}\cr
    \code{Cast} \tab {Number of cast members listed in the guide}\cr
    \code{Rating} \tab {Maltin rating (range is 1 to 4, in steps of 0.5)}\cr
    \code{Description} \tab {Number of lines of text Maltin uses to describe the movie}\cr
    \code{Origin} \tab {Country: 0 = USA, 1 = Great Britain, 2 = France, 3 = Italy, 4 = Canada}\cr
    \code{Time_code} \tab {\code{long}=90 minutes or longer \code{short}=under 90 minutes}\cr
    \code{Good} \tab {\code{1}=rating of 3 stars or better \code{0}=any lower rating}\cr
  }
}
\details{
One statistician movie fan decided to use statistics to study the movie ratings in his favorite movie guide, Movie and
Video Guide (1996), by Leonard Maltin. Maltin rates movies on a one-star to four-star
system, in increments of half-stars, with higher numbers being better. The guide also includes additional information on each film.
The statistician used a random number generator to select a simple random sample
of 100 movies rated by the Guide.
}
\source{
Data from Leonard Maltin's Movie and Video Guide (1996)
}

\keyword{datasets}
