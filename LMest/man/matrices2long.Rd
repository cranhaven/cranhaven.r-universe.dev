\name{matrices2long}
\alias{matrices2long}
\title{From data in array  format to data in long format}
\description{Function to convert data with array format in data with long format.}

\usage{matrices2long(Y, X1 = NULL, X2 = NULL)
}
\arguments{
  \item{Y}{array of responses (\code{n x TT x r})}
  \item{X1}{array of covariates (\code{n x TT x nc1})}
  \item{X2}{array of covariates (\code{n x TT x nc2})}
}

\details{\code{Y}, \code{X1} and \code{X2} must have the same number of observations.}
\value{Returns a \code{data.frame} with data in long format. The first column indicates the name of the unit identifier, and the second column indicates the time occasions.
}

\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini
}


\examples{

### Example with data on self rated health

data(data_SRHS_long)
SRHS <- data_SRHS_long[1:1600,]

# Covariates

X <- cbind(SRHS$gender-1,
           SRHS$race == 2 | SRHS$race == 3,
           SRHS$education == 4,
           SRHS$education == 5,
           SRHS$age-50,
           (SRHS$age-50)^2/100)

# Responses

Y <- SRHS$srhs


res <- long2matrices(SRHS$id, X = X, Y = Y)

long <- matrices2long(Y = res$YY, X1 = res$XX)


}

