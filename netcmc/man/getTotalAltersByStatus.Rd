\name{getTotalAltersByStatus}
\alias{getTotalAltersByStatus}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
A function that generates a data.frame that stores the number of alters with a given level of a factor an individual has.
}

\description{
This is a function that can be used to generates a data.frame that stores the number of alters with a given level of a factor an individual has.
}

\usage{
getTotalAltersByStatus(individualID, status, alters)
}

\arguments{
  \item{individualID}{
  A data.frame which stores the IDs of the individuals that nominate alters.
  }
  \item{status}{
  A data.frame which stores the levels of a variable.
  }
  \item{alters}{
  A data.frame which stores the alters of a given individual.
  }
}

\value{
  \item{totalAltersByStatus }{The resultant data.frame.}
}

\author{
George Gerogiannis
}

\examples{
individualID = data.frame(c(1, 2, 3, 4))
status = data.frame(c(10, 20, 30, 20))
alters = data.frame(c(4, 3, 2, 1), c(3, 4, 1, 2), c(2, 1, 4, 3))
totalAltersByStatus = getTotalAltersByStatus(individualID, status, alters)

individualID = data.frame(c(1, 2, 3, 4))
status = data.frame(c("RegularSmoke", "Nonsmoker", "CasualSmoker", "Nonsmoker"))
alters = data.frame(c(4, 3, 2, 1), c(3, 4, 1, 2), c(5, 1, 5, 3))
totalAltersByStatus = getTotalAltersByStatus(individualID, status, alters)

individualID = data.frame(c(1, 2, 3, 4))
status = data.frame(c(NA, "Nonsmoker", "CasualSmoker", "Nonsmoker"))
alters = data.frame(c(4, 3, 2, 1), c(3, 4, 1, 2), c(5, 1, 5, 3))
totalAltersByStatus = getTotalAltersByStatus(individualID, status, alters)

individualID = data.frame(c(10, 20))
status = data.frame(c(NA, "Nonsmoker"))
alters = data.frame(c(NA, 10), c(20, NA))
totalAltersByStatus = getTotalAltersByStatus(individualID, status, alters)

individualID = data.frame(c(NA, 20))
status = data.frame(c("Smoker", "Nonsmoker"))
alters = data.frame(c(NA, 10), c(20, NA))
totalAltersByStatus = getTotalAltersByStatus(individualID, status, alters)
}
