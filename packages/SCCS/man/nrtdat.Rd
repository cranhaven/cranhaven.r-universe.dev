\name{nrtdat}
\docType{data}
\alias{nrtdat}
\title{Data on NRT and MI}
\description{
The data comprise ages in days at first treatment with nicotine replacement therapy (NRT) and first subsequent myocardial infarction (MI). There are 141 simulated cases based on Tata et al (2005).
}
\usage{nrtdat}
\format{A data frame containing 141 rows and 7 columns. The column names are 'case' (individual identifier), 'nrt' (age at initiation of NRT treatment), 'mi' (age at first subsequent MI), 'end' (age at end of nominal observation period, nrt + 365), 'act' (age at earliest of end and actual end of observation period), 'cage' (centred age, in years, at NRT), 'cen' (1 if act = end, 0 if act < end).
}
%\source{}
\references{Hubbard, R., Lewis, S., Smith, C., Godfrey, C., Smeeth, L., Farrington, P., and Britton J. (2005). Use of nicotine replacement therapy and the risk of acute myocardial infarction, stroke and death. Tobacco Control 14, 416-421.
}
\keyword{datasets}
