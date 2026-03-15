\name{drgn}
\alias{drgn}
\title{causal models with G}
\description{
 provides concordance probabilities of relative pairs for a causal model with G component
}
\usage{
 drgn(fd,fr)
}
\arguments{
 \item{fd}{an array (size=number of dominant genes+recessive genes) of dominant gene frequencies including 0 values of recessive genes of G component}
 \item{fr}{an array (size=number of dominant genes+recessive genes) of recessive gene frequencies including 0 values of dominant genes of G component}
}
\value{
 list of the value of PLI and the matrix of NN, ND, and DD probabilities of 9 relative pairs: 1:mzt,2:parent-offspring,3:dzt,4:sibling,5:2-direct(grandparent-grandchild),6:3rd(uncle-niece),7:3-direct(great-grandparent-great-grandchild),8:4th (causin),9:4d(great-great-grandparent-great-great-grandchild)
}
\seealso{
 drgegne.gm
}
\examples{
### PLI=0.01.
ppt<-0.01

### g model

pp<-ppt  # the proportion of G component in total populations

fdt<-0.001 # one dominant gene with frequency of 0.001
tt<-5      # the number of recessive genes: 5

fd<-c(fdt,array(0,tt))
temp<-sqrt(1-((1-pp)/(1-fdt)^2)^(1/tt))
fr<-c(0,array(temp,tt))

drgn(fd,fr)


}
