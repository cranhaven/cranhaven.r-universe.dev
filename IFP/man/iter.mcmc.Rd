\name{iter.mcmc}
\alias{iter.mcmc}
\title{mcmc inference of causal models with all possible causal factors: G, G*G, G*E and E}
\description{
 provides proportions of each causal factor of G, G*G, G*E and E based on relative concordance data
}
\usage{
 iter.mcmc(ppt,aj=2,n.iter,n.chains,thinning=5,init.cut,darray,x,n,model,mcmcrg=0.01)
}
\arguments{
 \item{ppt}{population lifetime incidence}
 \item{aj}{a constant for the stage of data collection}
 \item{n.iter}{number of mcmc iterations}
 \item{n.chains}{number of mcmc chain}
 \item{thinning}{mcmc thinning parameter (default=5)}
 \item{init.cut}{mcmc data cut}
 \item{darray}{indicating the array positions of available data among 9 relative pairs: 1:mzt,2:parent-offspring,3:dzt,4:sibling,5:2-direct(grandparent-grandchild),6:3rd(uncle-niece),7:3-direct(great-grandparent-great-grandchild),8:4th (causin),9:4d(great-great-grandparent-great-great-grandchild)
}
 \item{x}{number of disease concordance of relative pairs}
 \item{n}{total number of relative pairs}
 \item{model}{an array, size of 4 (1: E component; 2: G component; 3: G*E component; 4: G*G component), indicating the existance of the causal component: 0: excluded; 1: included. }
 \item{mcmcrg}{ parameter of the data collection stage (default=0.01)}
}
\value{
 a list of rejectionRate, result summary, Gelman-Rubin diagnostics (point est. & upper C.I.) for output variables:
 e[1]: proportion of environmental factor (E)
 g[2]: proportion of genetic factor (G)
 ge[3]: proportion of gene-environment interaction (G*E)
 gg[4]: proportion of gene interactions (G*G)
 gn[5]: number of recessive genes in G
 ppe[6]: population proportion of interacting environment in G*E
 ppg[7]: population proportion of interacting genetic factor in G*E
 fd[8]: frequency of dominant genes in G
 fdge[9]: frequency of dominant genes in G*E
 gnge[10]: number of recessive genes in G*E
 ppd[11]: population proportion of dominant genes in G*G
 ppr[12]: population proportion of recessive genes in G*G
 kd[13]: number of dominant genes in G*G
 kr[14]: number of recessive genes in G*G
}
\references{
L. Park, J. Kim, A novel approach for identifying causal models of complex disease from family data, Genetics, 2015 Apr; 199, 1007-1016.
}
\examples{
### PLI=0.01.
ppt<-0.01

### a simple causal model with G and E components

pg<-0.007  # the proportion of G component in total populations
pgg<-0  # the proportion of G*G component in total populations
pge<-0  # the proportion of G*E component in total populations
e<-1-(1-ppt)/(1-pg)   # the proportion of E component in total populations

fd<-0.001  # one dominant gene
tt<-3      # the number of recessive genes

temp<-sqrt(1-((1-pg)/(1-fd)^2)^(1/tt))
fr<-c(array(0,length(fd)),array(temp,tt))
fd<-c(fd,array(0,tt))

rp<-drgegggne(fd,fr,c(0,0),c(0,0),c(0,0),c(0,0),0,e)

sdata<-rp[,3]/(rp[,2]+rp[,3])
#sdata<-round(sdata*500)

darray<-c(1:2,4:6)  
  ## available data= MZT, P-O, sibs, grandparent-grandchild, avuncular pair
n<-array(1000,length(darray))
x<-array()
for(i in 1:length(darray)){
x[i]<-rbinom(1,n[i],sdata[darray[i]])
}
model<-c(1,1,0,0)

## remove # from the following lines to test examples.
#iter.mcmc(ppt,2,15,2,1,1,darray,x,n,model) # provide a running test
#iter.mcmc(ppt,2,2000,2,10,500,darray,x,n,model) # provide a proper result

}
