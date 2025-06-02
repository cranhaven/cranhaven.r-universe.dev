\name{quefts_fert}

\alias{quefts_fert}

\title{
fertilizer parameters
}

\description{
Get a list with the default fertilization parameters
}

\usage{
quefts_fert()
}


\details{
\tabular{lll}{
	\tab \bold{Input Parameters} \tab \bold{Explanation} \cr
	\tab \bold{Management (fert)} \tab . \cr
	\tab \code{N, P, K} \tab N, P, and K fertilizer applied. \cr
}
}


\value{
list
}

\examples{
f <- quefts_fert()
str(f)
}
