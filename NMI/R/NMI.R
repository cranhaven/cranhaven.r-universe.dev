
# NMI
# This Function Computes the Normalized Mutual Information of Community Strctures in Network

NMI<-function(X,Y)
{
# X and Y are two matrics representing two partions
# whose first variables are the node id and the second variables are cluster index
  
# Convert X and Y to matrix (mainly for data frames)
X<-as.matrix(X)
Y<-as.matrix(Y)

# Split both X and Y to clusters
XC<-lapply(split(X[,1],X[,2] ), matrix, ncol=1)
YC<-lapply(split(Y[,1],Y[,2] ), matrix, ncol=1)

# Set up the probability matrix
P<-matrix(NA, nrow=length(XC),ncol=length(YC))
for(i in 1:dim(P)[1])
{
  for(j in 1:dim(P)[2])
  {
    P[i,j]<-length(which(as.vector(unlist(XC[i])) %in% as.vector(unlist(YC[j]))))
  }
}
# PI
Pi<-rowSums(P)
# PJ
Pj<-colSums(P)
# P
PP<-sum(P)
# Numerator
D<-0
for(i in 1:dim(P)[1])
{
  for(j in 1:dim(P)[2])
  {
    if(Pi[i]*Pj[j]==0){D<-D+0}
    else{
    temp<-P[i,j]*PP/Pi[i]/Pj[j]
    if(temp==0){D<-D+0}
    if(temp!=0){D<-D+P[i,j]*log(temp)}}
  }
}
D<--2*D
# Denominator 1
N1<-0
for(i in 1:dim(P)[1])
{
  if(PP==0){N1<-N1+0}
  else{
  temp<-Pi[i]/PP
  if(temp==0){N1<-N1+0}
  if(temp!=0){N1<-N1+Pi[i]*log(temp)}}
  
}
# Denominator 2
N2<-0
for(j in 1:dim(P)[2])
{
  if(PP==0){N2<-N2+0}
  else{
  temp<-Pj[j]/PP
  if(temp==0){N2<-N2+0}
  if(temp!=0){N2<-N2+Pj[j]*log(temp)}}
}

if(N1+N2==0){NMI<-0}
if(N1+N2!=0){NMI<-D/(N1+N2)}

# Retuen
object<-list(value=NMI)
object
}