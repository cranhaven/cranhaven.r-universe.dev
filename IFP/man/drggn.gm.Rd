\name{drggn}
\alias{drggn}
\title{causal models with G*G}
\description{
 provides concordance probabilities of relative pairs for a causal model with G*G component
}
\usage{
 drggn(fd,fr)
}
\arguments{
 \item{fd}{an array (size=number of dominant genes+recessive genes) of dominant gene frequencies including 0 values of recessive genes of G*G component}
 \item{fr}{an array (size=number of dominant genes+recessive genes) of recessive gene frequencies including 0 values of dominant genes of G*G component}
}
\value{
 a list of PLI and a matrix of NN, ND, and DD probabilities of 9 relative pairs: 1:mzt,2:parent-offspring,3:dzt,4:sibling,5:2-direct(grandparent-grandchild),6:3rd(uncle-niece),7:3-direct(great-grandparent-great-grandchild),8:4th (causin),9:4d(great-great-grandparent-great-great-grandchild)
}
\seealso{
 drgegggne
}
\examples{
### PLI=0.01.
ppt<-0.01

### g*g model

pp<-ppt  # the proportion of G*G component in total populations

gd<-sqrt(pp) # dominant gene proportion = recessive gene proportion
fd<-array(1-sqrt(1-gd^(1/2)),2)  # two dominant genes
tt<-2      # the number of recessive genes: 2

temp<-(pp/gd)^(1/2/tt)
fr<-c(array(0,length(fd)),array(temp,tt))
fd<-c(fd,array(0,tt))

drggn(fd,fr)


}
