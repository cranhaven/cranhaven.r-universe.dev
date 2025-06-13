\name{imd}
\alias{imd}
\title{
Calculate of the inter-mutational distance (IMD).
}
\description{
The inter-mutational distance (IMD) is the distance between each somatic substitution and the substitution immediately prior (Nik-Zainal et al. 2012). The \code{imd()} is used to graph the rainfall plot (Nik-Zainal et al. 2012). This graph localizes the regional clustering of mutations and represents the IMD on the y-axis on a log base 10 scale, with mutations ranked and ordered on the x-axis from the first variant on the short arm of chromosome 1 to the last variant on the long arm of chromosome X.
}
\usage{
imd(data = NULL, chr = NULL, position, extra = NULL)
}
\arguments{
  \item{data}{: somatic substitution mutations of the cancer genome data set.}
  \item{chr}{: chromosome where the somatic mutation is located.}
  \item{position}{: position of somatic mutations in the DNA sequence of the cancer genome.}
  \item{extra}{: additional information to posterior graph.}
}
\value{
\code{imd()} returns a data set with six variables: 
\item{chr}{: chromosome where the somatic mutation is located.}
\item{position}{: position of somatic mutations in the DNA sequence of the cancer genome.}
\item{extra}{: additional information.}
\item{number}{: ordered mutations.}
\item{distance}{: Euclidean distance between each somatic substitution and the substitution immediately prior (IMD) (Nik-Zainal et al. 2012).}
\item{log10distance}{: Euclidean distance between each somatic substitution and the substitution immediately prior in base 10.}
}
\references{
Nik-Zainal S, Alexandrov LB, Wedge DC, et al; Breast Cancer Working Group of the International Cancer Genome Consortium. Mutational processes molding the genomes of 21 breast cancers. Cell. 2012 May 25;149(5):979-93.

Wickham H. ggplot2: elegant graphics for data analysis. Springer New York, 2009.
}
\author{
David Lora
}
\seealso{
\code{\link{plot}}
}
\examples{

data(PD4107a)

###New variable
extra <- factor(c(),levels=c("T>C","T>G","T>A","C>T","C>G","C>A"))
extra[PD4107a$Ref_base=="A" & PD4107a$Mutant_base=="G"]<-"T>C"
extra[PD4107a$Ref_base=="T" & PD4107a$Mutant_base=="C"]<-"T>C"
extra[PD4107a$Ref_base=="A" & PD4107a$Mutant_base=="C"]<-"T>G"
extra[PD4107a$Ref_base=="T" & PD4107a$Mutant_base=="G"]<-"T>G"
extra[PD4107a$Ref_base=="A" & PD4107a$Mutant_base=="T"]<-"T>A"
extra[PD4107a$Ref_base=="T" & PD4107a$Mutant_base=="A"]<-"T>A"
extra[PD4107a$Ref_base=="G" & PD4107a$Mutant_base=="A"]<-"C>T"
extra[PD4107a$Ref_base=="C" & PD4107a$Mutant_base=="T"]<-"C>T"
extra[PD4107a$Ref_base=="G" & PD4107a$Mutant_base=="C"]<-"C>G"
extra[PD4107a$Ref_base=="C" & PD4107a$Mutant_base=="G"]<-"C>G"
extra[PD4107a$Ref_base=="G" & PD4107a$Mutant_base=="T"]<-"C>A"
extra[PD4107a$Ref_base=="C" & PD4107a$Mutant_base=="A"]<-"C>A"
PD4107a$extra<-extra

###Generate new data set with intermutational distance;
rainfall<-imd(data=PD4107a,chr=Chr,position=Position,extra=extra)

###Rainfall plot for PD4107a cancer sample;
plot(rainfall$number, rainfall$log10distance,pch=20,
	ylab="Intermutation distance (bp)",xlab="PD4107a",yaxt="n",
	col=c(rep(c("black","red"),14)[rainfall$chr]))
axis(2, at=c(0,1,2,3,4,6), labels=c("1","10","100","1000","10000","1000000"),
	las=2, cex.axis=0.6)

###Rainfall plot for PD4107a cancer sample (Nik-Zainal et al. 2012);
#require(ggplot2)
#graph <- qplot(data=rainfall,number,log10distance,colour=extra, ylim=c(0.2,8), 
#	ylab="log10", xlab="PD4107a")
#graph <- graph + 
#	scale_colour_manual(values =c("T>C"="yellow","T>G"="green","T>A"="pink",
#		"C>T"="red","C>G"="black","C>A"="blue"))
#graph <- graph + theme(legend.title=element_blank())
#graph <- graph + scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 6),
#		labels=c("1","10","100","1000","10000","1000000"))
#graph
}
\keyword{kataegis}
