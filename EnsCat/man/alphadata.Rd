\name{alphadata}
\alias{alphadata}
\docType{data}
\title{Alphaherpesvirinae virus genome sequence data
%%   ~~ data name/kind ... ~~
}
\description{
A dataset consisting of whole genome sequences for viruses from the family Alphaherpesvirinae
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("alphadata")}
\format{
This dataset is a matrix of dimension 98x359883 that represents the sequences of 98 viral genomes from the subfamily Alphaherpesvirinae.
}
\details{
 This data includes whole genome sequences of viruses belonging to the subfamily Alphaherpesvirinae. Alphaherpesvirinae is  a subfamily of the Herpesviridae family of viruses that cause diseases in humans and animals. The data is downloaded from ViPR, \url{http://www.viprbrc.org}, and are aligned using "MAFFT", see Katoh et a. (2013), and saved in "alphadata". Alphaherpesvirinae has five genera: Iltovirus (IIt), Mardivirus (Mar), Scutavirus, Simplexvirus (Sim), and Vari-cellovirus (Var). The viruses were collected from different hosts, namely, human, monkey, chicken, turkey, duck, cow, bat, equidae, boar, cat, amazona oratrix (denoted hum, mon, chi, tur, duc, cow, bat, equ, boa, cat, aor). The codes in the example show the labels.
}

\references{
Katoh, K., and D.M. Standley (2013). MAFFT multiple sequence alignment software version 7: improvements in performance and usability. Molecular biology and evolution, 30(4), 772-780.

Pickett, BE et al. (2012). ViPR: an open bioinformatics database and analysis resource for virology research. Nucleic Acids Research 40: D593-8.
}
\examples{
### load Alphaherpesvirinae data
#data("alphadata")
### the following codes define the labels of the data by genera and host.
#xlab1<-NULL
#xlab1[1:8]<-"Var-boa";xlab1[9:13]<-"Var-hum";xlab1[14]<-"Var-cat"
#xlab1[15:32]<-"Var-equ";xlab1[33]<-"Var-mon";xlab1[34:40]<-"Var-cow"
#xlab1[41:45]<-"Sim-mon";xlab1[46:47]<-"Sim-mon";xlab1[48:58]<-"Sim-hum"
#xlab1[59]<-"Sim-bat";xlab1[60]<-"Sim-mon";xlab1[61]<-"Mar-tur"
#xlab1[62:71]<-"Mar-chi";xlab1[72:78]<-"Mar-duc";xlab1[79]<-"Ilt-ora";xlab1[80:98]<-"Ilt-chi"
}
\keyword{datasets}
