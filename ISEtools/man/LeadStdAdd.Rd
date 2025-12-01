\name{LeadStdAdd}
\alias{LeadStdAdd}
\docType{data}
\title{
   ISE measurements of lead in soil
}
\description{
A data set containing emf responses for 3 ISEs measuring lead in soil at Silvermines, Ireland. Calibration data and experimental data for 17 samples (in standard addition format) are included.
}
\usage{data(LeadStdAdd)}
\format{
	Load example lead data as an object of type ISEdata (see function loadISEdata)
}
\examples{
data(LeadStdAdd)
print(LeadStdAdd)
summary(LeadStdAdd)
plot(LeadStdAdd)
\dontrun{
# Additional usage of this dataset with describeISE and analyseISE:
example1 = describeISE(LeadStdAdd, Z = 2, temperature = 21)
print(example1)
summary(example1)
plot(example1)
example2 = analyseISE(LeadStdAdd, Z = 2, temperature = 21)
print(example2)
summary(example2)
plot(example2, ylim = c(-7, -3), xlab = "ID (Sample)", 
	ylab = expression(paste(log[10], " ", Pb^{paste("2","+",sep="")} )))
}
}
\keyword{datasets}
\references{
  Dillingham, P.W., Radu, T., Diamond, D., Radu, A. and McGraw, C.M. (2012). Bayesian Methods for Ion Selective Electrodes. \emph{Electroanalysis}, \strong{24}, 316-324. <doi:10.1002/elan.201100510>

  Dillingham, P.W., Alsaedi, B.S.O., Radu, A., and McGraw, C.M. (2019). Semi-automated data analysis for ion-selective electrodes and arrays using the R package ISEtools. \emph{Sensors} \strong{19}(20), 4544. <doi: 10.3390/s19204544>	
  
  Dillingham, P.W., Alsaedi, B.S.O., Granados-Focil, S., Radu, A., and McGraw, C.M. (2020). Establishing meaningful Limits of Detection for ion-selective electrodes and other nonlinear sensors. \emph{ACS Sensors}, \strong{5}, 250-257. <doi:10.1021/acssensors.9b02133>
}
