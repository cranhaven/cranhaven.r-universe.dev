#################################################################################
#usage: calculate Pr( x-y < dif );  x~beta(a1,b1);  y~beta(a2,b2)
#################################################################################
#ibeta=function(z,a,b){ pbeta(z,a,b)*beta(a,b) }
fun1=function(y,dif,a1,b1,a2,b2){ pbeta(y+dif,a1,b1)*dbeta(y,a2,b2) }

diff_beta_cdf=function(dif,a1,b1,a2,b2){ 
  fun2=function(x){fun1(x,dif,a1,b1,a2,b2)}
  integrate(fun2,lower=0,upper=1)$value
} 


######################################################################################################
#usage: generate genotype data 
#return: a matrix, cols are samples, rows are snps
#input: indicator for none effect should be the first element of the clusters vector, 
#       positive and negative are the 2nd and 3rd element respectively
######################################################################################################
# index - px1 vector of SNP cluster membership
# clusters - 3x1 vector. 
#    1st element indicates SNP cluster membership 1 (diseased subjects
#      have the same MAF as healthy subjects);
#    2nd element indicates SNP cluster membership 2 (diseased subjects
#      have higher MAF than healthy subjects);
#    3nd element indicates SNP cluster membership 3 (diseased subjects
#      have lower MAF than healthy subjects);
# gtype - 3x1 vector of genotypes; 
#    1st element indicates wildtype homozygote
#    2nd element indicates heterozygote
#    3rd element indicates mutation homozygote
# nx - number of diseased subjects
# ny - number of healthy subjects
# alpha - 
# beta - 
genotype.data=function(index,clusters,gtype,nx,ny,alpha,beta)
{
  G=length(index)
  X=array(NA,dim=c(nx,G))   #genotype data for diseased samples
  Y=array(NA,dim=c(ny,G))   #genotype data for healthy samples
  
  num=c(sum(index==clusters[1]),sum(index==clusters[2]),sum(index==clusters[3]))
  alltheta=model.pars(alpha,beta,num)
  theta1=alltheta$theta.EE        #snp MAF for samples in none effect group
  theta2.X=alltheta$theta.OE.X    #snp MAF for diseased samples in positive effect group
  theta2.Y=alltheta$theta.OE.Y    #snp MAF for healthy samples in positive effect group
  theta3.X=alltheta$theta.UE.X    #snp MAF for diseased samples in negative effect group
  theta3.Y=alltheta$theta.UE.Y    #snp MAF for healthy samples in negative effect group
  
  p.EE=cbind((1-theta1)^2,2*theta1*(1-theta1),theta1^2)  #probability matrix for each gene in none effect group
  p.OE.X=cbind((1-theta2.X)^2,2*theta2.X*(1-theta2.X),theta2.X^2)  #probability matrix for each gene in diseased samples in positive effect group
  p.OE.Y=cbind((1-theta2.Y)^2,2*theta2.Y*(1-theta2.Y),theta2.Y^2)  #probability matrix for each gene in healthy samples in positive effect group
  p.UE.X=cbind((1-theta3.X)^2,2*theta3.X*(1-theta3.X),theta3.X^2)  #probability matrix for each gene in diseased samples in negative effect group
  p.UE.Y=cbind((1-theta3.Y)^2,2*theta3.Y*(1-theta3.Y),theta3.Y^2)  #probability matrix for each gene in healthy samples in negative effect group
  
  count1=count2=count3=0
  for (jj in 1:G){
    # if((index[jj]!=clusters[1])&(index[jj]!=clusters[1])&(index[jj]!=clusters[1])) break("error in index")
    if(index[jj]==clusters[1]){
      count1=count1+1
      temp=sample(gtype,(nx+ny),replace=T,prob=p.EE[count1,])
      X[,jj]=temp[1:nx]
      Y[,jj]=temp[-(1:nx)]
    }
    if (index[jj]==clusters[2]){
      count2=count2+1
      X[,jj]=sample(gtype,nx,replace=T,prob=p.OE.X[count2,])
      Y[,jj]=sample(gtype,ny,replace=T,prob=p.OE.Y[count2,])
    }
    if (index[jj]==clusters[3]){
      count3=count3+1
      X[,jj]=sample(gtype,nx,replace=T,prob=p.UE.X[count3,])
      Y[,jj]=sample(gtype,ny,replace=T,prob=p.UE.Y[count3,])   
    }
  }
  mat=rbind(X, Y)
  rownames(mat)=paste("subj", 1:nrow(mat), sep="")
  colnames(mat)=paste("SNP", 1:ncol(mat), sep="")
  invisible(mat)
} 


######################################################################################################
#usage: generate model parameter (theta.EE, theta.OE, theta.UE)
#return: a list containing theta.EE, theta.OE.X, theta.OE.Y, theta.UE.X, theta.UE.Y
######################################################################################################

model.pars=function(alpha,beta,num){
  
  theta.1=rep(NA,num[1])      #MAF of genes for samples in none effect group
  theta.2.X=rep(NA,num[2])      #MAF of genes for diseased samples in positive effect group
  theta.2.Y=rep(NA,num[2])      #MAF of genes for healthy samples in positive effect group
  theta.2.X=rep(NA,num[3])      #MAF of genes for diseased samples in negative effect group
  theta.2.Y=rep(NA,num[3])      #MAF of genes for healthy samples in negative effect group
  
  count=0
  while(count<num[1]){
    theta=rbeta(1,alpha,beta) #MAF for none effect group
    if((theta>0.05)&(theta<0.5)){
      count=count+1
      theta.1[count]=theta
    }
  }
  #  theta.1=rbeta(num[1],alpha,beta)
  
  MAF.dd=MAF.d(alpha,beta,num[2],num[3])
  theta.2.X=MAF.dd[1:num[2],1]
  theta.2.Y=MAF.dd[1:num[2],2]
  theta.3.X=MAF.dd[-(1:num[2]),2]
  theta.3.Y=MAF.dd[-(1:num[2]),1]
  return(list(theta.EE=theta.1,theta.OE.X=theta.2.X,theta.OE.Y=theta.2.Y,theta.UE.X=theta.3.X,theta.UE.Y=theta.3.Y))
}




######################################################################################################
#usage: generate MAFs for SNPs that have positive and negative effects
#return: matrix with two columns (larger MAFs, smaller MAFs)
######################################################################################################
MAF.d=function(alpha,beta,num.OE,num.UE){
  theta.pool.l=rep(NA,(num.OE+num.UE))    #save the larger one from the pair of generated MAF
  theta.pool.s=rep(NA,(num.OE+num.UE))    #save the smaller one from the pair of generated MAF
  count=0
  while(count<(num.OE+num.UE)){
    theta=rbeta(2,alpha,beta)       #generate a pair of MAF
    #   if (power.prop.test(n=ssize,theta[1],theta[2],sig.level = siglevel)$power>pow){  #proportion test
    #   if(theta[1]>0.05 & theta[2]>0.05 &theta[1]<0.5 &theta[2]<0.5 ){
    if(sum(theta>0.05,theta<0.5)==4){
      count=count+1
      theta.pool.l[count]=max(theta)   
      theta.pool.s[count]=min(theta)    
    }
  }
  return(cbind(theta.pool.l,theta.pool.s))
}






######################################################################################################
#usage: EM algorithm to estimate pi
######################################################################################################
# data - nxp genotype matrix; n=number of subjects; p=number of SNPs
# gtype - 3x1 vector of genotype code
# memSubj - nx1 binary vector of subject disease status:
#      0 stands for healthy subjects; and 1 stands for diseased subjects. 
#      'nx' (number of diseased subjects) and 'ny' (number of healthy subjects) 
#      will be calculated based on 'memSubj'.
#  bb - concentration parameter of the symmetric Dirichlet distribution
EM.model=function(data,gtype, memSubj, chain,pi_init,alpha_init,beta_init,bb)
{  
  if(sum(is.na(memSubj)==TRUE))
  {
    stop("memSubj should have no missing values!")
  }
  nx= sum(memSubj==1) # number of diseased subjects
  ny= sum(memSubj==0) # number of healthy subjects
  nsnp=ncol(data) # number of SNPs
  n0=apply(data==gtype[1],2,sum)  #length=nsnp
  n1=apply(data==gtype[2],2,sum)
  n2=apply(data==gtype[3],2,sum)
  nx0=apply(data[1:nx,]==gtype[1],2,sum)  #length=nsnp
  nx1=apply(data[1:nx,]==gtype[2],2,sum)
  nx2=apply(data[1:nx,]==gtype[3],2,sum)
  ny0=apply(data[-(1:nx),]==gtype[1],2,sum)  #length=nsnp
  ny1=apply(data[-(1:nx),]==gtype[2],2,sum)
  ny2=apply(data[-(1:nx),]==gtype[3],2,sum)
  alpha0=2*n2+n1+alpha_init
  beta0=n1+2*n0+beta_init
  alpha.x=2*nx2+nx1+alpha_init
  beta.x=2*nx0+nx1+beta_init
  alpha.y=2*ny2+ny1+alpha_init
  beta.y=2*ny0+ny1+beta_init
  cx=2^(nx1)*beta(alpha.x,beta.x)/beta(alpha_init,beta_init)
  cy=2^(ny1)*beta(alpha.y,beta.y)/beta(alpha_init,beta_init)
  PV=rep(NA,nsnp)
  for (gg in 1:nsnp){
    PV[gg]=diff_beta_cdf(0,alpha.x[gg],beta.x[gg],alpha.y[gg],beta.y[gg]) #Pr(vx<vy) vx~beta(alpha.x,beta.x) vy~beta(alpha.y,beta.y)
  }
  distn=array(NA,dim=c(nsnp,3))
  distn[,1]=2^(n1)*beta(alpha0,beta0)/beta(alpha_init,beta_init)
  distn[,2]=2*cx*cy*(1-PV)
  distn[,3]=2*cx*cy*PV
  respon=array(NA,dim=c(nsnp,3))
  pi_upd=array(NA,dim=c(chain,3))
  pi_upd[1,]=pi_init
  for (ii in 1: (chain-1)){
    for (kk in 1:3){
      for (gg in 1:nsnp){
        respon[gg,kk]=pi_upd[ii,kk]*distn[gg,kk]/sum(pi_upd[ii,]*distn[gg,])  #responsibility
      }
      pi_upd[ii+1,kk]=(sum(respon[,kk])+bb[kk]-1)/(nsnp+sum(bb)-3)
    }
  }
  return(pi_upd)
}



# 
# 
# #initial settings for genotype data generation
# set.seed(2) 
# alpha=2    #prior parameter
# beta=5     #prior parameter         
# G=1000     #number of genes
# nx=100     #size of diseased samples 
# ny=100     #size of healthy samples
# pi=c(0.8,0.1,0.1)        #cluster probabilities (none, positive, negative)
# clusters=c(0,1,-1)        #cluster code   (none, positive, negative)
# gtype=c(0,1,2)        #genotype code
# #siglevel=0.001        #significance level for proportion test
# #pow=0.8            #power for proportion test
# 
# 
# #generate cluster membership for each snp
# index=sample(clusters,G,replace=T,pi)  
# #prop.table(table(index))
# 
# #generate genotype data
# data=genotype.data(index,clusters,gtype,nx,ny,alpha,beta)  
# 
# 
# 
# 
# ####initial values for EM algorithm
# chains=100     #length of chain
# #pi_init=s1$mean.pi.Hat   #initial values of pi
# pi_inits=c(0.89261, 0.05335, 0.05404)   #from previous simulations
# alpha_inits=3.620842
# beta_inits=10.515104
# BB=c(1,1,1)   #dirichlet(1,1,1)
# 
# 
# 
# #run EM algorithm
# em1=EM.model(data,gtype,ny,chains,pi_inits,alpha_inits,beta_inits,BB)
# em1
# em1[chains,] 
# par(mfrow=c(2,2))
# plot(seq(1,chains),em1[,1],cex=0.3,main="pi for no effect", xlab="chains",ylab="pi")
# plot(seq(1,chains),em1[,2],cex=0.3,main="pi for positive effect", xlab="chains",ylab="pi")
# plot(seq(1,chains),em1[,3],cex=0.3,main="pi for negative effect", xlab="chains",ylab="pi")
# #boxplot(em1[,1],em1[,2],em1[,3],names = c("no effects","positive effects","negative effects"))
# 
# ##Results:
# # pi_EE = 0.7791502     
# # pi_OE = 0.10775265 
# # pi_OE = 0.11309719 
# # 100 iterations cost 1.963581 sec. Converged at the 26th iteration.
# 
# 
# 
# 
#           
