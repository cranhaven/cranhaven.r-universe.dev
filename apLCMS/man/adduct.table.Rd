\name{adduct.table}
\alias{adduct.table}
\docType{data}
\title{A table of potential adducts.}
\description{
The data is based on the Metabolomics FieHn Lab's Mass Spectrometry Adduct Calculator. It provides the basis for calculating the m/z of the ion forms of known metabolites.}
\usage{data(adduct.table)}
\format{
  A data frame with 47 observations on the following 4 variables.
  \describe{
    \item{adduct}{The ion form.}
    \item{divider}{The value to divide the neutral mass by.}
    \item{addition}{The value to add after dividing.}
    \item{charge}{The charge state of the ion form.}
  }
}
\source{
http://fiehnlab.ucdavis.edu/staff/kind/Metabolomics/MS-Adduct-Calculator/
}
\references{
Huang N.; Siegel M.M.1; Kruppa G.H.; Laukien F.H. Automation of a Fourier transform ion cyclotron resonance mass spectrometer for acquisition, analysis, and e-mailing of high-resolution exact-mass electrospray ionization mass spectral data. J Am Soc Mass Spectrom 1999, 10, 1166-1173.
}
\examples{
data(metabolite.table)
data(adduct.table)
known.table.example<-make.known.table(metabolite.table[1001:1020,], adduct.table[1:4,])
}
\keyword{datasets}
