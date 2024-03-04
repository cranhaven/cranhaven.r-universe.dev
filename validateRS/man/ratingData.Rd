\name{ratingData}
\alias{ratingData}
\docType{data}
\title{
Data for rating classes of S&P
}
\description{
Data for rating classes of S&P
}
\usage{data(ratingData)}
\format{
  A list with:
  \itemize{
    \item SP.p.0: vector with PDs of S&P classes
    \item SP.sizes: vector with sizes of S&P classes
  }
}
\details{
Data that can be used for the function \code{simul.scenario.rs}. For a large number of classes a lot of memory is required.  For computers with a limited amount of memory lesser classes can be obtained by indexing. 

The number of rated entities and the PDs over a one-year horizon in the six S&P classes reflect the statistics for S&P's global corporate ratings available at  https://www.nact.org/resources/NACT_2012_Global_Corporate_Default.pdf. S&P's rating grades are assigned to six classes according to the mapping report of the European Supervisory Authorities for the implementation of the Basel III banking regulation in the European Union (Capital Requirements Regulation (EU) No 575/2013) (see https://www.eba.europa.eu/regulation-and-policy/external-credit-assessment-institutions-ecai/draft-implementing-technical-standards-on-the-mapping-of-ecais-credit-assessments). 
}

\seealso{
\code{\link{simul.scenario.rs}}
}

\examples{
data(ratingData)
}
