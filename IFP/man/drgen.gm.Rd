\name{drgen}
\alias{drgen}
\title{causal models with G*E}
\description{
 provides concordance probabilities of relative pairs for a causal model with G*E component
}
\usage{
 drgen(fd,fr,e)
}
\arguments{
 \item{fd}{an array (size=number of dominant genes+recessive genes) of dominant gene frequencies including 0 values of recessive genes of G component of G*E interacting with E of G*E}
 \item{fr}{an array (size=number of dominant genes+recessive genes) of recessive gene frequencies including 0 values of dominant genes of G component of G*E interacting with E of G*E}
 \item{e}{a proportion of population who are exposed to environmental cause of G*E interacting with genetic cause of G*E during their entire life}
}
\value{
 a list of the g*e proportion in population and a matrix of NN, ND, and DD probabilities of 9 relative pairs: 1:mzt,2:parent-offspring,3:dzt,4:sibling,5:2-direct(grandparent-grandchild),6:3rd(uncle-niece),7:3-direct(great-grandparent-great-grandchild),8:4th (causin),9:4d(great-great-grandparent-great-great-grandchild)
}
\seealso{
 drgene.gm
}
\examples{
### PLI=0.01.
ppt<-0.01

### g*e model

pge<-ppt  # the proportion of G*E component in total populations

ppe<-0.5
ppg<-pge/ppe

fd<-0.0005  # one dominant gene
tt<-3      # the number of recessive genes

temp<-sqrt(1-((1-ppg)/(1-fd)^2)^(1/tt))
fr<-c(array(0,length(fd)),array(temp,tt))
fd<-c(fd,array(0,tt))

drgen(fd,fr,ppe)


}
