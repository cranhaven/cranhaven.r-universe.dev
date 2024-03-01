\name{bpdat}
\docType{data}
\alias{bpdat}
\title{Data on blood pressure and headaches}
\description{
The data comprise systolic and diastolic blood pressures taken twice a day for seven days, and start of headache in the next period. There are 71 headaches in 64 cases.
}
\usage{bpdat}
\format{A data frame containing 896 rows and 6 columns. The column names are 'case' (individual identifier), 'dow' (day of week, coded 1 for Monday,...,7 for Sunday), 'time' (time of reading, coded 1 for am, 2 for pm), 'sys' (systolic blood pressure), 'dia' (diastolic blood pressure), 'head' (1 if a headache started in the period up to the next blood pressure reading, or within 12 hours of the last reading).
}
%\source{}
\references{Farrington P., Whitaker H., and Ghebremichael-Weldeselassie Y. (2018). Self-controlled Case Series Studies: A modelling Guide with R. Boca Raton: Chapman & Hall/CRC Press.}
\keyword{datasets}