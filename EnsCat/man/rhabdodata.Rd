\name{rhabdodata}
\alias{rhabdodata}
\docType{data}
\title{Rhabdoviridae virus genome sequence data
%%   ~~ data name/kind ... ~~
}
\description{
This dataset consists of whole genome sequences of viruses from the Family Rhabdoviridae.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("rhabdodata")}
\format{
The format of the data is a list with components $dat and $lab. The components dat includes
a matrix of dimension 53x26035 that represents the sequences of 53 viral genomes from the
family Rhabdoviridae. The component lab includes labels for each sample that include
abbreviations of the relevant genus and viral host.
}
\details{
 This data includes whole genome sequences of viruses belonging to the subfamily Rhabdoviridae.
 Rhabdoviridae is a family of viruses with single-stranded RNA genomes that are able to
 infect a wide variety of hosts, both plants and animals. cause diseases in humans and
 animals. The data is downloaded from ViPR, \url{http://www.viprbrc.org}, and are aligned
 using "MAFFT", see Katoh et a. (2013), and saved in "rhabdodata". Rhabdoviridae has twelve
genera of which nine are repesented here: Cytorhabdovirus, Ephemerovirus, Novirhabdovirus,
Nucleorhabdovirus, Perhabdovirus, Sigmavirus, Sprivivirus, Tibrovirus, and Tupavirus.
The viruses were collected from different hosts, namely, Alfalfa, Cattle, Drosophila, Eel,
Fish, Garlic, Midge, Mosquito, Eggplant, Taro, Trout, and Unknown.
}

\references{
Katoh, K., and D.M. Standley (2013). MAFFT multiple sequence alignment software version 7: improvements in performance and usability. Molecular biology and evolution, 30(4), 772-780.

Pickett, BE et al. (2012). ViPR: an open bioinformatics database and analysis resource for virology research. Nucleic Acids Research 40: D593-8.
}
\examples{
### load Rhabdoviridae data
data("rhabdodata")
### the following codes define the labels of the data by genera and host.
dim(rhabdodata$dat)
#[1]	53	26035

}
\keyword{datasets}
