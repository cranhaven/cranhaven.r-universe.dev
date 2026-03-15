\name{drgene}
\alias{drgene}
\title{causal models with G*E and E}
\description{
 provides concordance probabilities of relative pairs for a causal model with G*E and E components
}
\usage{
 drgene(fdg,frg,eg,e)
}
\arguments{
 \item{fdg}{an array (size=number of dominant genes+recessive genes) of dominant gene frequencies including 0 values of recessive genes of G component of G*E interacting with E of G*E}
 \item{frg}{an array (size=number of dominant genes+recessive genes) of recessive gene frequencies including 0 values of dominant genes of G component of G*E interacting with E of G*E}
 \item{eg}{a proportion of population who are exposed to environmental cause of G*E interacting with genetic cause of G*E during their entire life}
 \item{e}{a proportion of population who are exposed to environmental cause during their entire life}
}
\value{
 matrix of NN, ND, and DD probabilities of 9 relative pairs: 1:mzt,2:parent-offspring,3:dzt,4:sibling,5:2-direct(grandparent-grandchild),6:3rd(uncle-niece),7:3-direct(great-grandparent-great-grandchild),8:4th (causin),9:4d(great-great-grandparent-great-great-grandchild)
}
\seealso{
 drgen.gm
}
\examples{
### PLI=0.01.
ppt<-0.01

### g*e+e model

pge<-0.007  # the proportion of G*E component in total populations
e<-1-(1-ppt)/(1-pge)   # the proportion of E component in total populations

ppe<-0.5
ppg<-pge/ppe

fd<-0.0005  # one dominant gene
tt<-3      # the number of recessive genes

temp<-sqrt(1-((1-ppg)/(1-fd)^2)^(1/tt))
fr<-c(array(0,length(fd)),array(temp,tt))
fd<-c(fd,array(0,tt))

drgene(fd,fr,ppe,e)


}
