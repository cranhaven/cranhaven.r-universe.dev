\name{apdat}
\docType{data}
\alias{apdat}
\title{Data on antipsychotics and stroke}
\description{
The data comprise ages in days at first stroke and treatments with antipsychotics. There are 2000 simulated cases based on Douglas and Smeeth (2008), including 1500 without dementia and 500 with dementia.
}
\usage{apdat}
\format{A data frame containing 11792 rows and 8 columns. The column names are 'case' (individual identifier), 'sta' (age on the first day of the observation period), 'end' (age on last day of the observation period), 'stro' (age at first stroke), 'ap' (age at first day of antipsychotic treatment episode), 'endap' (age at last day of antipsychotic treatment episode), 'cen' (1 if observation ended before the end of the study, 0 otherwise), 'dem' (0 if the patient does not have dementia, 1 if the patient has dementia). The data are in format 'stack' with different exposure episodes on different rows.}
%\source{}
\references{Douglas, I. J. and Smeeth L. (2008). Exposure to antipsychotics and risk of stroke: Self-controlled case series study. British Medical Journal 337, a1227.
}
\keyword{datasets}
