\name{daLaw}
\alias{daLaw}
\docType{data}
\title{Data for statutory reforms and retention of prescribed fire liability laws on forest land by state}
\description{This data set contains a cross-sectional data set for current adoption of statutory laws with regard to prescribed fire liability on forest landowners. It has 50 observations and 16 variables.

\tabular{ll}{
\code{SHORT} \tab Two-letter abbreviations of 50 state names\cr
\code{STATE} \tab Full state names \cr
\code{Y} \tab Categorical dependent variable (Y = 0, 1, 2, or 3) \cr
\code{FYNFS} \tab National Forests area in a state (million acres) \cr
\code{FYIND} \tab Industrial forest land area in a state  (million acres) \cr
\code{FYNIP} \tab Nonindustrial private forest land area in a state (million acres) \cr
\code{AGEN} \tab Permanent forestry program personnel in a state \cr
\code{POPRUR} \tab Rural population in a state (million) \cr
\code{EDU} \tab Population 25 years and older with advanced degrees in a state (million) \cr
\code{INC} \tab Per capita income in a state (thousand dollars) \cr
\code{DAY} \tab The maximum length of legislative sessions in calendar days in a state \cr
\code{BIANN} \tab A dummy variable equal to one for states with annual legislative sessions, zero with biannual (or less) \cr
\code{SEAT} \tab Total number of legislative seats (Senate plus House) in the legislative body in a state \cr
\code{BICAM} \tab Level of bicameralism in a state, defined as the size of the Senate divided by the size of the House \cr
\code{COMIT} \tab Total number of standing committees in a state \cr
\code{RATIO} \tab Total number of standing committees in a state divided by the number of legislators \cr
}
}
\usage{data(daLaw)}

\format{A data frame object with 50 rows and 16 variables.}

\details{This is the final data set used in the study of Sun (2006).}

\source{See Table 2 in Sun (2006) for detail of data sources.}

\references{
Sun, C. 2006. State statutory reforms and retention of prescribed fire liability laws on US forest land. Forest Policy and Economics 9(4):392-402.
}

\examples{
data(daLaw)
str(daLaw); head(daLaw); names(daLaw) 
}
\keyword{datasets}