\name{ebola}
\alias{ebola}
\docType{data}
\title{Ebolavirus genome sequence data
%%   ~~ data name/kind ... ~~
}
\description{
A dataset consisting of whole genome sequences for Ebolavirus  from the family Filovirdae.

%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("ebola")}
\format{
	 The format of the data is a list with components $obs and
 $lab.
 "ebola$obs" includes the observations that are stored as numerical values.
"ebola$lab" contains the labels of the data.

This ebola$obs is a matrix of dimension 103x26445 that represents the sequences of 103 viral genomes from the Ebolavirus.
}
\details{
 This data includes whole genome sequences of viruses belonging to the Ebolavirus. Ebolavirus
 is  a subfamily of the Filovirdae family of viruses. The data is downloaded from ViPR, \url{http://www.viprbrc.org}, and are aligned using "MAFFT", see Katoh et a. (2013), and saved in "ebola". Ebolavirus subdivides into five species: Bundibugyo virus (Bun), Reston ebolavirus (Res), Sudan ebolavirus (Sud), Tai Forest ebolavirus (Tai), and Zaire ebolavirus (Zai).
 The hosts are human, monkey, swine, guinea pig, mouse, and bat (denoted hum, mon, swi, gpi, mou, bat, respectively,  in our dataset).  The ebola$lab in the example show the labels, the combination of species and host.
}

\references{
Katoh, K., and D.M. Standley (2013). MAFFT multiple sequence alignment software version 7: improvements in performance and usability. Molecular biology and evolution, 30(4), 772-780.

Pickett, BE et al. (2012). ViPR: an open bioinformatics database and analysis resource for virology research. Nucleic Acids Research 40: D593-8.
}
\examples{
### load Ebolavirus data
#data("ebola")
}
\keyword{datasets}
