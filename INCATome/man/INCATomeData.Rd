\name{INCATomeData}
\alias{INCATomeData}
\docType{data}
\title{
Translatome microarray dataset simulated from experimentally acquired translatome of Hela cells.
}
\description{
An RGList object for raw translatome data (based on one array and its dyesawapped array) simulated to produce n=12 arrays (n[CTRL]=3, n[CDT]=3, n[CTRLdyeswapped]=3, n[CDTdyeswapped]=3). Simulation was performed so that 25\% of genes are deregulated and that this deregulation is skewed by 70\% towards downregulation.
}
\usage{data("INCATomeData")}
\format{
  The data contain a list of 6 objects: 4 RGLists and two dataframes.
\describe{
\item{RGdata}{an RGList object containing R, G, Rb, Gb, targets and source. The main data dimensions are ncol=12 arrays and nrow=2000 probes. The geneset can be fragmented as follows: from 1 to 1664: general probes, from 1665 to 1677: ACTB probes, from 1678 to 1680: PABPC1 probes and from 1681 to 2000: SpikeIn probes}
\item{RGdataBG}{an RGList object of background corrected data containing R, G, targets and source}
\item{RGdataNM}{an RGList object of INCA normalised data containing R, G, targets and source}
\item{RGdataDS}{an RGList object of dyeswapped data containing R, G, targets and source}
\item{idata}{a dataframe containing the Internal Reference (ACTB and PABPC1) Expected logged Ratios for each array as determined experimentally.}
\item{sdata}{a dataframe containing the SpikeIn Expected Ratios for each probe as defined experimentally by the manufacturer.
}
}
}
\source{
Sbarrato T., Spriggs R.V., Wilson L., Jones C., Dudek K., Bastide A., Pichon X., Poyry T. and Willis A.E., \strong{RNA}, 2017 Aug 25, An Improved Analysis Methodology for Translational Profiling by Microarray,  \href{http://rnajournal.cshlp.org/content/early/2017/08/25/rna.060525.116.abstract}{DOI:10.1261/rna.060525.116}
}
\examples{
data(INCATomeData)
}
\keyword{datasets}
